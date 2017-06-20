
## changing values from character to numerical value

## for project
configProjectData <- function(ProjectDataSet,configCompetencyData,configGradeData)
{


  reqGrade <- as.data.frame(strsplit(as.character(ProjectDataSet$REQ_GRADE),","),stringsAsFactors = FALSE)
  colnames(reqGrade) <- "grade"
  projectCompetencyLevel <- as.data.frame(strsplit(as.character(ProjectDataSet$REQ_COMPETENCY_LEVEL),","),stringsAsFactors = FALSE)
  
  colnames(projectCompetencyLevel) <- "compLevel"

  for (j in 1:length(configGradeData$GRADE)) {
    
  
    
    reqGrade$grade[toupper(reqGrade$grade) == configGradeData$GRADE[j]] <- configGradeData$GRADE_LEVEL[j]
    
    
  
  }
  for (i  in 1:length(configCompetencyData$COMPETENCY_LEVEL)) {
    
    projectCompetencyLevel$compLevel[toupper(projectCompetencyLevel$compLevel) == configCompetencyData$COMPETENCY_LEVEL[i]] <- configCompetencyData$COMPETENCY_LEVEL_ORDER[i]
        
    
  }
   return(data.frame(grade = mean(as.numeric(reqGrade$grade),na.rm = TRUE),compMean = mean(as.numeric(projectCompetencyLevel$compLevel),na.rm = TRUE)))
  
  }

#############################################################################################################################
##for employeeData (grade)

getEmployeeGrade <- function(grade,configGradeData)
{
 
  
  for (j in 1:length(configGradeData$GRADE)) {
    
    
    
    grade[toupper(grade) == configGradeData$GRADE[j]] <- configGradeData$GRADE_LEVEL[j]
    
    
    
  } 
  
  return(grade)
}

########################################################################################################################################
##############  Emp Competency level

getEmployeeCompetencyLevel <- function(compLevel , configCompetencyData)
{
  #compLevel <- employeeData$EMP_COMPETENCY_LEVEL
  employeeCompetencyLevel <- as.data.frame(strsplit(as.character(compLevel),","),stringsAsFactors = FALSE)
  colnames(employeeCompetencyLevel) <- "empCompLevel"
  
  for (i  in 1:length(configCompetencyData$COMPETENCY_LEVEL)) {
    
    employeeCompetencyLevel$empCompLevel[toupper(employeeCompetencyLevel$empCompLevel) == configCompetencyData$COMPETENCY_LEVEL[i]] <- configCompetencyData$COMPETENCY_LEVEL_ORDER[i]
    
    
  }
  
  return(employeeCompetencyLevel$empCompLevel)
  
}
