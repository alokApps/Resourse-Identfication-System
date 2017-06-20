getEmployeeAnalyseData <- function(dataEmp,dataProject,configCompetencyData,configGradeData)
{
  
  dataEmp <- getRequiredEmployeeTable
  
  dataProject <- ProjectDataSet
  
  
  
  #getEmployeIdList <- unique(as.numeric(dataEmp$EMP_ID))
  #getEmployeeNameList <- unique(dataEmp$EMP_NAME)
  matchedEmployeeCompetencyCount <- data.frame()
  #projectRequiredCompetency <- as.data.frame(strsplit(as.character(dataProject$REQ_COMPETENCY),","),stringsAsFactors = FALSE ,row.names = NULL)
  
  projectRequiredCompetency <- strsplit(as.character(dataProject$REQ_COMPETENCY),",")
  
  projectCompetencyLevel <- as.data.frame(strsplit(as.character(dataProject$REQ_COMPETENCY_LEVEL),","),stringsAsFactors = FALSE)
  
  colnames(projectCompetencyLevel) <- "compLevel"
  
  
  for (i  in 1:length(configCompetencyData$COMPETENCY_LEVEL)) {
    
    projectCompetencyLevel$compLevel[toupper(projectCompetencyLevel$compLevel) == configCompetencyData$COMPETENCY_LEVEL[i]] <- configCompetencyData$COMPETENCY_LEVEL_ORDER[i]
    
    
  }
  
  
  
  #employeeComp <- strsplit(as.character(dataEmp$EMP_COMPETENCY[78]),",")
  
  
  
  projectRequiredLocation <- strsplit(as.character(dataProject$REQ_LOCATION),",")
  projectRequiredIOU <- dataProject$REQ_IOU
  
  projectStartDate <- dataProject$REQ_START_DATE
  
  projectEndDate <- dataProject$REQ_END_DATE
  
  sub_projectRequiredIOU <- dataProject$REQ_SUB_IOU
  flag = 0
  # rm(getEmployeeDataSet,getProjectDataSet,getRequiredEmployeeTable,ProjectDataSet,testDataSet)
  if (identical(projectRequiredIOU,"NA")) 
    flag = 1
  #View(dataEmp)  
  
  
  empSet <- data.frame()
  # i <- 1
  for (i in 1:length(dataEmp$EMP_ID)) {
    
    employeeData <- subset(dataEmp , dataEmp$EMP_ID == dataEmp$EMP_ID[i])
    
    if ( as.Date(projectStartDate,"%m/%d/%Y") >= as.Date(employeeData$EMP_AVAILABLE_FROM,"%m/%d/%Y") & 
         as.Date(projectEndDate,"%m/%d/%Y") <= as.Date(employeeData$EMP_AVAILABLE_TILL,"%m/%d/%Y"))
    {
      
      #View(employeeData)
      
      
      getEmployeeCompetency <- strsplit(as.character(employeeData$EMP_COMPETENCY),",")
      
      
      getEmployeeCompetencyLevel <- getEmployeeCompetencyLevel(employeeData$EMP_COMPETENCY_LEVEL,configCompetencyData)
      
      getEmployeeLocationPreferences <- strsplit(as.character(employeeData$EMP_PREFERRED_LOCATION) ,",")
      
      
      getEmployeeGrade <- getEmployeeGrade(employeeData$EMP_GRADE,configGradeData)
      
      getEmployeeIOU <- employeeData$EMP_IOU
      
      sub_EmployeeIOU <- strsplit(as.character(employeeData$EMP_SUB_IOU),",")
      
      #sub_EmployeeIOU_level <- unique(dataEmp[which(as.numeric(dataEmp$EMP_ID)==empDataNameId[i,1]),grep("^IOU_EXPERIENCE$", colnames(dataEmp))])
      empBaseLocation <- employeeData$EMP_BASE_LOCATION
      
      
      employeeBenchAgeing <-  Sys.Date() - as.Date(employeeData$EMP_AVAILABLE_FROM,"%m/%d/%Y") 
      
      
      
      getEmpAvailailabiltyFrom <- employeeData$EMP_AVAILABLE_FROM
      
      getEmpAvailailabiltyTill <- employeeData$EMP_AVAILABLE_TILL
      ##competency and competency_experience
      matchedEmployeeCompetencyCount <- length(intersect(getEmployeeCompetency,projectRequiredCompetency)) 
      #ce_exp <-  ifelse(ce1>0, mean(as.numeric(dataEmp[ which(!is.na(match(comp,projectRequiredCompetency))),15]),na.rm = TRUE),0)
      employeeCompetency_level <- ifelse(matchedEmployeeCompetencyCount > 0 ,as.numeric(competenceTest(getEmployeeCompetency,getEmployeeCompetencyLevel,projectRequiredCompetency,projectCompetencyLevel)),0)
      ##ce1 <- rbind(ce1,ce)
      matchedEmployeeCompetencyCount <- ifelse(matchedEmployeeCompetencyCount > 0,matchedEmployeeCompetencyCount,0)
      ## location 
      if (length(intersect(empBaseLocation,dataProject$REQ_LOCATION))) {
        employeeLocation <- 1
        
      }else{
        employeeLocation <- which(!is.na(match(getEmployeeLocationPreferences,projectRequiredLocation)))
        
      employeeLocation <- ifelse(length(employeeLocation) > 0,which.min(employeeLocation),-1)
      }
     
      employeeLocationMatch <- 1/employeeLocation
      ##grade
      employeeGrade <- employeeData$EMP_GRADE
      ##bench_ageing
      
      ##IOU and sub_iou match
      if (flag == 1) 
        employeeIOUAnalysis <- 2
      if (flag == 0) {
        employeeIOUAnalysis <- ifelse(length(intersect(getEmployeeIOU,projectRequiredIOU)) > 0,
                                      ifelse(length(intersect(sub_EmployeeIOU,sub_projectRequiredIOU)) > 0,1.2*2,2),0)
        employeeIOUAnalysis <- ifelse(length(employeeIOUAnalysis) > 0,employeeIOUAnalysis,0)
        employeeIOUAnalysis <- as.numeric(employeeIOUAnalysis)
      }
      empAvailabilityCheck <- availabilityTest(projectStartDate , projectEndDate , getEmpAvailailabiltyFrom ,getEmpAvailailabiltyTill)
      
      empSet1 <- data.frame(employeeData$EMP_ID,employeeData$EMP_NAME,matchedEmployeeCompetencyCount,employeeCompetency_level,
                            employeeLocationMatch,getEmployeeGrade
                            ,employeeBenchAgeing,employeeIOUAnalysis , empAvailabilityCheck ,stringsAsFactors = FALSE) 
      empSet <- rbind(empSet,empSet1)
    }
    
  }
  names(empSet) <- c("EMP_ID","employee_name","matched_competency_count","competence_level","location","grade","bench_ageing","iou","availabilityPoint")
  
  return(empSet)
  
  ## mean(as.numeric(dataEmp[ which(!is.na(match(comp,projectRequiredCompetency))),15]))
  
}