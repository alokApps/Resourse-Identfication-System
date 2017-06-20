
competenceTest <- function(getEmployeeCompetency,getEmployeeCompetencyLevel,projectRequiredCompetency , projectCompetencyLevel)
  
{
  incrementLevel <- 0
 # employeeData <- testDataSource()
  require(sqldf) 
  #projectData <- ProjectDataSetFunction(9003)
 # projectData = projectData1
  particular_EmployeeData <- data.frame(getEmployeeCompetency ,getEmployeeCompetencyLevel)
 
  colnames(particular_EmployeeData) <- c("EMP_COMPETENCY","EMP_COMPETENCY_LEVEL")
  
  projectCompetencyData <- data.frame(projectRequiredCompetency)
  colnames(projectCompetencyData) <- "REQ_COMPETENCY"
  projectData <- cbind(projectCompetencyData,projectCompetencyLevel)
  # particular_EmployeeData <- employeeData[which(employeeData$EMP_ID == emp_Id),]
  #length(projectCompetencyData$REQ_COMPETENCY)

  
  matched_employeeProjectCompetenceTable <- data.frame(sqldf("select particular_EmployeeData.EMP_COMPETENCY, particular_EmployeeData.EMP_COMPETENCY_LEVEL,projectData.compLevel from projectData ,particular_EmployeeData
                                                             where particular_EmployeeData.EMP_competency=projectData.REQ_COMPETENCY;"))
  
  
  
  
  
  
  
  for (i in 1:length(matched_employeeProjectCompetenceTable$EMP_COMPETENCY)) {
    
    competencyLevelDifference <- as.numeric(matched_employeeProjectCompetenceTable$EMP_COMPETENCY_LEVEL[i]) - as.numeric(matched_employeeProjectCompetenceTable$compLevel[i])
    competencyLevelDifference <- ifelse(competencyLevelDifference > 0 ,competencyLevelDifference,0)
    incrementLevel <- incrementLevel + competencyLevelDifference + 1
    
  }
  calculatedCompetencyLevel <- (incrementLevel + length(matched_employeeProjectCompetenceTable$EMP_COMPETENCY_LEVEL)) * (length(matched_employeeProjectCompetenceTable$EMP_COMPETENCY_LEVEL)/length(projectCompetencyData$REQ_COMPETENCY))
  ifelse(calculatedCompetencyLevel >= 0,return(calculatedCompetencyLevel),return(0))
  
  
  
}



