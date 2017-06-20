getEmployeeAnalyseData <- function(dataEmp,dataProject)
{
  require(sets)
  #dataEmp <- getRequiredEmployeeTable
  
  #dataProject <- ProjectDataSet
  require(sqldf)
  
  empDataNameId <- sqldf('select distinct(EMP_ID), EMP_NAME from dataEmp ')
  #getEmployeIdList <- unique(as.numeric(dataEmp$EMP_ID))
  #getEmployeeNameList <- unique(dataEmp$EMP_NAME)
  matchedEmployeeCompetencyCount <- data.frame()
  projectRequiredCompetency <- unique(dataProject$REQ_COMPETENCY)
  projectRequiredLocation <- unique(dataProject$REQ_LOCATION)
  projectRequiredIOU <- unique(dataProject$REQ_IOU)
  
  projectStartDate <- unique(dataProject$REQ_START_DATE)
  
  projectEndDate <- unique(dataProject$REQ_END_DATE)
  
  sub_projectRequiredIOU <-  unique(dataProject$REQ_SUB_IOU)
  flag = 0
  # rm(getEmployeeDataSet,getProjectDataSet,getRequiredEmployeeTable,ProjectDataSet,testDataSet)
  if (identical(projectRequiredIOU,"NA")) 
    flag = 1
  #View(dataEmp)  
 
  
  empSet <- data.frame()
  # i <- 1
  for (i in 1:length(empDataNameId$EMP_ID)) {
    getEmployeeCompetency <- unique(dataEmp[which(as.numeric(dataEmp$EMP_ID) == empDataNameId[i,1]),grep("^EMP_COMPETENCY$", colnames(dataEmp))])
    
    getEmployeeLocationPreferences <- unique(dataEmp[which(as.numeric(dataEmp$EMP_ID) == empDataNameId[i,1]),grep("^EMP_PREFERRED_LOCATION$", colnames(dataEmp))])
    
    getEmployeeGrade <- unique(dataEmp[which(as.numeric(dataEmp$EMP_ID) == empDataNameId[i,1]),grep("^EMP_GRADE$", colnames(dataEmp))])
    
    getEmployeeIOU <- unique(dataEmp[which(as.numeric(dataEmp$EMP_ID) == empDataNameId[i,1]),grep("^EMP_IOU$", colnames(dataEmp))])
    
    sub_EmployeeIOU <- unique(dataEmp[which(as.numeric(dataEmp$EMP_ID) == empDataNameId[i,1]),grep("^EMP_SUB_IOU$", colnames(dataEmp))])
    #sub_EmployeeIOU_level <- unique(dataEmp[which(as.numeric(dataEmp$EMP_ID)==empDataNameId[i,1]),grep("^IOU_EXPERIENCE$", colnames(dataEmp))])
    empBaseLocation <- unique(dataEmp[which(as.numeric(dataEmp$EMP_ID) == empDataNameId[i,1]),grep("^EMP_BASE_LOCATION$", colnames(dataEmp))])
    
    getEmpAvailailabiltyFrom <- unique(dataEmp[which(as.numeric(dataEmp$EMP_ID) == empDataNameId[i,1]),grep("^EMP_AVAILABLE_FROM$", colnames(dataEmp))])
    
    getEmpAvailailabiltyTill <- unique(dataEmp[which(as.numeric(dataEmp$EMP_ID) == empDataNameId[i,1]),grep("^EMP_AVAILABLE_TILL$", colnames(dataEmp))])
    ##competency and competency_experience
    matchedEmployeeCompetencyCount <- length(intersect(getEmployeeCompetency,projectRequiredCompetency)) 
    #ce_exp <-  ifelse(ce1>0, mean(as.numeric(dataEmp[ which(!is.na(match(comp,projectRequiredCompetency))),15]),na.rm = TRUE),0)
    employeeCompetency_level <- ifelse(matchedEmployeeCompetencyCount > 0 ,competenceTest(dataEmp,empDataNameId[i,1],dataProject),0)
    ##ce1 <- rbind(ce1,ce)
    matchedEmployeeCompetencyCount <- ifelse(matchedEmployeeCompetencyCount > 0,matchedEmployeeCompetencyCount,0)
    ## location 
    if (length(intersect(empBaseLocation,dataProject$REQ_LOCATION))) {
      employeeLocationMatch <- -1
      
    }else{
      employeeLocationMatch <- as.numeric(dataEmp[ which(!is.na(match(getEmployeeLocationPreferences,projectRequiredLocation))),grep("^EMP_LOCATION_PRIORITY$", colnames(dataEmp))])
      employeeLocationMatch <- ifelse(length(employeeLocationMatch) > 0,-employeeLocationMatch,-5)
    }
    ##grade
    employeeGrade <- unique(as.numeric(dataEmp[which(as.numeric(dataEmp$EMP_ID) == empDataNameId[i,1]),grep("^EMP_GRADE$", colnames(dataEmp))]))
    
    ##bench_ageing
    employeeBenchAgeing <- unique(as.numeric(dataEmp[which(as.numeric(dataEmp$EMP_ID) == empDataNameId[i,1]),grep("^BENCH_AGEING$", colnames(dataEmp))]))
    
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
    
    empSet1 <- data.frame(empDataNameId[i,1],empDataNameId[i,2],matchedEmployeeCompetencyCount,employeeCompetency_level,
                          employeeLocationMatch,employeeGrade,employeeBenchAgeing,employeeIOUAnalysis , empAvailabilityCheck ) 
    empSet <- rbind(empSet,empSet1)
    
    
  }
  names(empSet) <- c("EMP_ID","employee_name","matched_competency_count","competence_level","location","grade","bench_ageing","iou","availabilityPoint")
  
  return(empSet)
  
  ## mean(as.numeric(dataEmp[ which(!is.na(match(comp,projectRequiredCompetency))),15]))
  
}