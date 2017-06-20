getRequiredEmployeeDataSet <- function(empData,ProjectDataSet)
{ 
  #empData <- testDataSet
 
  ReqGrade <- as.data.frame(strsplit(as.character(ProjectDataSet$REQ_GRADE),","),stringsAsFactors = FALSE)
  colnames(ReqGrade) <- "ReqGrade"
 # projectReqGrades <- unique(as.numeric(ProjectDataSet$REQ_GRADE))
  finalEmpData <- data.frame()
  for (i in 1:length(ReqGrade$ReqGrade)) {
     
    empData1 <- subset(empData,empData$EMP_GRADE == ReqGrade$ReqGrade[i])
    finalEmpData <- rbind(finalEmpData,empData1)
   
    
  }


return(finalEmpData)


}
