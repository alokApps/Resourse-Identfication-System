getEmployeeAnalyseData <- function(dataEmp,dataProject,configCompetencyData,configGradeData)
{
  
  #dataEmp <- getRequiredEmployeeTable
  
  #dataProject <- ProjectDataSet
  
  
  
  
  matchedEmployeeCompetencyCount <- data.frame(length(dataEmp$EMP_ID))
  
  projectRequiredCompetency <- unlist(strsplit(as.character(dataProject$REQ_COMPETENCY),","))
  
  projectCompetencyLevel <- as.data.frame(strsplit(as.character(dataProject$REQ_COMPETENCY_LEVEL),","),stringsAsFactors = FALSE)
  
  colnames(projectCompetencyLevel) <- "compLevel"
  
  
  for (i  in 1:length(configCompetencyData$COMPETENCY_LEVEL)) {
    
    projectCompetencyLevel$compLevel[toupper(projectCompetencyLevel$compLevel) == configCompetencyData$COMPETENCY_LEVEL[i]] <- configCompetencyData$COMPETENCY_LEVEL_ORDER[i]
    
    
  }
  
  projectRequiredLocation <- unlist(strsplit(as.character(dataProject$REQ_LOCATION),","))
  flag = 0
  if (identical(dataProject$REQ_IOU,"NA")) 
    flag = 1
  
  
  empSet <- data.frame()
  # i <- 1
  for (i in 1:length(dataEmp$EMP_ID)) {
    
    # employeeData <- subset(dataEmp , dataEmp$EMP_ID == dataEmp$EMP_ID[i])
    #availability  <- dataEmp[i,]$"BLOCKED (Mark 'Y' if YES)"
    
    #View(dataEmp[i,])
    
    
    getEmployeeCompetency <- unlist(strsplit(as.character(dataEmp[i,]$EMP_COMPETENCY),","))
    
    getEmployeeCompetencyLevel <- getEmployeeCompetencyLevel(dataEmp[i,]$EMP_COMPETENCY_LEVEL,configCompetencyData)
    
    getEmployeeLocationPreferences <- unlist(strsplit(as.character(dataEmp[i,]$EMP_PREFERRED_LOCATION) ,","))
    
    
    getEmployeeGrade <- getEmployeeGrade(dataEmp[i,]$EMP_GRADE,configGradeData)
    
    # getEmployeeIOU <- dataEmp[i,]$EMP_IOU
    
    sub_EmployeeIOU <- unlist(strsplit(as.character(dataEmp[i,]$EMP_SUB_IOU),","))
    
    empBaseLocation <- dataEmp[i,]$EMP_BASE_LOCATION
    
    
    employeeBenchAgeing <-  Sys.Date() - as.Date(dataEmp[i,]$EMP_AVAILABLE_FROM,"%m/%d/%Y") 
    
    
    
    
    ##competency and competency_experience
    matchedEmployeeCompetencyCount <- length(intersect(getEmployeeCompetency,projectRequiredCompetency)) 
    
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
    ##bench_ageing
    
    ##IOU and sub_iou match
    if (flag == 1) 
      employeeIOUAnalysis <- 1
    if (flag == 0) {
      employeeIOUAnalysis <- ifelse(length(intersect(dataEmp[i,]$EMP_IOU,dataProject$REQ_IOU)) > 0,
                                    ifelse(length(intersect(sub_EmployeeIOU,dataProject$REQ_SUB_IOU)) > 0,1,0.5),0)
      employeeIOUAnalysis <- ifelse(length(employeeIOUAnalysis) > 0,employeeIOUAnalysis,0)
      employeeIOUAnalysis <- as.numeric(employeeIOUAnalysis)
    }
    #empAvailabilityCheck <- availabilityTest(projectStartDate , projectEndDate , getEmpAvailailabiltyFrom ,getEmpAvailailabiltyTill)
    
    empSet1 <- data.frame(dataEmp[i,]$EMP_ID,dataEmp[i,]$EMP_NAME,matchedEmployeeCompetencyCount,employeeCompetency_level,
                          employeeLocationMatch,getEmployeeGrade,employeeBenchAgeing,employeeIOUAnalysis ,1,dataEmp[i,]$EMP_GRADE, dataEmp[i,]$EMP_IOU,dataEmp[i,]$EMP_SUB_IOU,dataEmp[i,]$EMP_SUB_IOU,dataEmp[i,]$EMP_BASE_LOCATION,dataEmp[i,]$EMP_PREFERRED_LOCATION,dataEmp[i,]$EMP_COMPETENCY,dataEmp[i,]$EMP_COMPETENCY_LEVEL ,stringsAsFactors = FALSE) 
    empSet <- rbind(empSet,empSet1)
    
    
  }
  names(empSet) <- c("EMP_ID","employee_name","matched_competency_count","competence_level","location","grade","bench_ageing","iou","availabilityPoint",
                     "EMP_GRADE", "EMP_IOU","EMP_SUB_IOU","EMP_SUB_IOU","EMP_BASE_LOCATION","EMP_PREFERRED_LOCATION","EMP_COMPETENCY","EMP_COMPETENCY_LEVEL")
  
  return(empSet)
  
  ## mean(as.numeric(dataEmp[ which(!is.na(match(comp,projectRequiredCompetency))),15]))
  
}