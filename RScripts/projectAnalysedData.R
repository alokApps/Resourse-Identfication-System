getProjectAnalysedData <- function(ProjectDataSet,configCompetencyData,configGradeData)
{
  
  compGradeMean <- configProjectData(ProjectDataSet,configCompetencyData,configGradeData)
  ##Required competency analysis
  comp <- as.data.frame(strsplit(as.character(ProjectDataSet$REQ_COMPETENCY),","),stringsAsFactors = FALSE)
  colnames(comp) <- "comp"
  projectCompetencyCount <- length(comp$comp)
  projectCompetencyLevel <- compGradeMean[1]
  projectRequiredIOU <- ProjectDataSet$REQ_IOU
  projectRequiredSubIOU <- ProjectDataSet$REQ_SUB_IOU
   projectReqGrades <- compGradeMean[2]
  
  projectAnalysedTable <- data.frame(ReqCompetency = projectCompetencyCount,Req_competencyLevel = projectCompetencyLevel,ReqIOU = projectRequiredIOU,ReqSubIOU = projectRequiredSubIOU ,ReqGrades = projectReqGrades,
                                     Req_Start_date = ProjectDataSet$REQ_START_DATE , Req_End_Date = ProjectDataSet$REQ_END_DATE,
                                     Grade_weightage = ProjectDataSet$GRADE_WEIGHTAGE,location_weightage = ProjectDataSet$LOCATION_WEIGHTAGE,
                                     competency_weightage = ProjectDataSet$COMPETENCY_WEIGHTAGE)
 return(projectAnalysedTable)
}



