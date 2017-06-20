getRequiredEmployeeDataSet <- function(empData,ProjectDataSet,n)
{ 
  #empData <- testDataSet
  
 if (n < 20) {
   n <- 20
 }
  projectRequiredCompetency <- unlist(strsplit(as.character(ProjectDataSet$REQ_COMPETENCY),","))
  projectRequiredLocation <- unlist(strsplit(as.character(ProjectDataSet$REQ_LOCATION),","))
  # nrow(empData)
  ReqGrade <- as.data.frame(strsplit(as.character(ProjectDataSet$REQ_GRADE),","),stringsAsFactors = FALSE)
  colnames(ReqGrade) <- "ReqGrade"
  print("gradeStart")
  #empData <- subset(empData,empData$EMP_GRADE == ReqGrade$ReqGrade[1:length(ReqGrade$ReqGrade)])
  empDataTest <- data.frame()
  for (i in 1:length(ReqGrade$ReqGrade)) {
    
    empData1 <- subset(empData,empData$EMP_GRADE == ReqGrade$ReqGrade[i])
    empDataTest <- rbind(empDataTest,empData1)
    
    
  }
  print("gradeEnd")
  ###################################################################################
  #######   Elimination by blocking yes/no
  ##  removing na <- 0 
  empDataTest$`BLOCKED (Mark 'Y' if YES)`[is.na(empDataTest$`BLOCKED (Mark 'Y' if YES)`)] <- 0
  empData <- empDataTest[which(toupper(empDataTest$`BLOCKED (Mark 'Y' if YES)`) != 'Y'),]
  
 
  
  empData <- empDataTest[which(as.Date(empDataTest$EMP_AVAILABLE_FROM,"%m/%d/%Y") <= as.Date(ProjectDataSet$REQ_START_DATE) &
                                  as.Date(empDataTest$EMP_AVAILABLE_TILL,"%m/%d/%Y") >= as.Date(ProjectDataSet$REQ_END_DATE) ),]
  
 # View(empData)
  print("compStart")
  finalEmpData <- data.frame()
  for (i in 1:length(empData$EMP_ID)) {
  
    getEmployeeCompetency <- unlist(strsplit(as.character(empData[i,]$EMP_COMPETENCY),","))
    matchedEmployeeCompetencyCount <- length(intersect(getEmployeeCompetency,projectRequiredCompetency)) 
    empData$matchedCount <- matchedEmployeeCompetencyCount
    if(matchedEmployeeCompetencyCount > 0)
      finalEmpData <- rbind(finalEmpData,empData[i,])
    # nrow(empData)
   
  }
  #View(finalEmpData)
  print("compMiddle")
  finalEmpData <- finalEmpData[order(-finalEmpData$matchedCount),] 
  #finalEmpData <- head(finalEmpData,10*n)
 print("compEnd")
  if (NROW(finalEmpData) > 2*n) 
  {
    
    finalEmpDataLocation <- data.frame()
    for (i in 1:length(finalEmpData$EMP_ID)) {
      employeeData <- subset(finalEmpData ,finalEmpData$EMP_ID == finalEmpData$EMP_ID[i])
      
      getEmployeeLocationPreferences <- unlist(strsplit(as.character(employeeData$EMP_PREFERRED_LOCATION) ,","))
        
      
      
      
      
      employeeLocation <- length( intersect(getEmployeeLocationPreferences,projectRequiredLocation))
     
      employeeBaseLocation <- length( intersect(employeeData$EMP_BASE_LOCATION,projectRequiredLocation))
  
    
      if ((employeeLocation + employeeBaseLocation) > 0) {
       
        finalEmpDataLocation <- rbind(finalEmpDataLocation,employeeData)
      }
       
      
      
    }
 
  print(NROW(finalEmpDataLocation))
   
    if (NROW(finalEmpDataLocation$EMP_ID) > 2*n) {
      
      finalEmpDataLocation <- finalEmpDataLocation[order(-finalEmpDataLocation$matchedCount),]
      
      finalEmpDataIOU <- subset(finalEmpDataLocation , finalEmpDataLocation$EMP_IOU == ProjectDataSet$REQ_IOU)
      if (NROW(finalEmpDataIOU) > 2*n) {
        print("iou")
        
        return(finalEmpDataIOU)
      }
      else{
        print("location")
      return(finalEmpDataLocation)
      }
    }else{
      
      
      
     finalEmpData <- finalEmpData[order(-finalEmpData$matchedCount),] 
    
      return(finalEmpData)
    }
    
    
  }else
    return(empData)
  
}
