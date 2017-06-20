getFinalResultSet <- function(getProjectDataSet,getEmployeeDataSet)
{
  empIdList <- unique(as.numeric(getEmployeeDataSet$EMP_ID))
  finalList <- data.frame()
  i <- 2
  for (i in 1:length(empIdList)) {
    employee <- getEmployeeDataSet[which(as.numeric(getEmployeeDataSet$EMP_ID) == empIdList[i]),]
    
    
    Req_comp <- ifelse(getProjectDataSet$ReqCompetency == 0 | identical(getProjectDataSet$ReqCompetency,"NA"), 1 ,getProjectDataSet$ReqCompetency)
    
    
    ReqIOU <- ifelse(getProjectDataSet$ReqIOU == 0 | identical(getProjectDataSet$ReqIOU,"NA"), 1 ,getProjectDataSet$ReqIOU)
    
    ReqGrade <- ifelse(getProjectDataSet$grade == 0 | identical(getProjectDataSet$grade,"NA"), 1 ,getProjectDataSet$grade)
    
    empCompetencyPercentage <- employee$competence_level / Req_comp
    empCompetencyPercentage[empCompetencyPercentage > 1] <- 1
    empCompetencyCount <- employee$matched_competency_count/getProjectDataSet$ReqCompetency
    empIOUPercentage <- employee$iou
    empGradePercentage <- as.numeric(employee$grade)/ReqGrade
    empGradePercentage[empGradePercentage > 1] <- 1
    empLocation <- employee$location
    Bench_ageing <- employee$bench_ageing
  empPointSum <- (empCompetencyCount + empCompetencyPercentage)*(1 + getProjectDataSet$competency_weightage)/100 + 
                      empIOUPercentage + empGradePercentage*(1 + getProjectDataSet$Grade_weightage)/100 + 
                              empLocation*(1 + getProjectDataSet$location_weightage)/100 + employee$availabilityPoint
    employeeAnalysedFunctionValue <- empPointSum
    finalList1 <- data.frame(employee$EMP_ID,employee$employee_name , employeeAnalysedFunctionValue , Bench_ageing ,employee$EMP_GRADE,employee$EMP_IOU,employee$EMP_SUB_IOU,
                             employee$EMP_BASE_LOCATION,employee$EMP_PREFERRED_LOCATION,employee$EMP_COMPETENCY,employee$EMP_COMPETENCY_LEVEL,stringsAsFactors = FALSE)
    finalList <- rbind(finalList,finalList1)
  }
    names(finalList) <- c("Employee ID","Employee name","employeeIdentifiedValue","Bench_ageing","EMP_GRADE","EMP_IOU","EMP_SUB_IOU",
                          "EMP_BASE_LOCATION","EMP_PREFERRED_LOCATION","EMP_COMPETENCY","EMP_COMPETENCY_LEVEL")
  
  return(finalList) 
}


