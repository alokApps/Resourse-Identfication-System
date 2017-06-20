## developed by alok kumar



  
main <- function(won,n = 0)
{
  require(rio)
  

  configCompetencyData <- import("data\\configurationFile\\configCompetencyData.ods")
  
  configGradeData <- import("data\\configurationFile\\configGradeData.ods")
  
   ProjectDataSet <- ProjectDataSetFunction(won)
 if(n == 0){
   n <- ProjectDataSet$NO_OF_RESOURCES
 }
   #View(ProjectDataSet)
   if (!is.null(ProjectDataSet)) {
     


     testDataSet <- import("data\\employeeData\\employeeData.xlsx")

## analysis of project data
 getProjectDataSet <- getProjectAnalysedData(ProjectDataSet,configCompetencyData,configGradeData)
 #View(getProjectDataSet)
 
 
 
## fetching required employees 
#rm(getRequiredEmployeeTable)
 getRequiredEmployeeTable <- getRequiredEmployeeDataSet(testDataSet,ProjectDataSet,n)
# View(getRequiredEmployeeTable)
 ## analysis of employee data
 #print(nrow(getRequiredEmployeeTable))
 getEmployeeDataSet <- getEmployeeAnalyseData(getRequiredEmployeeTable,ProjectDataSet,configCompetencyData,configGradeData )
View(getEmployeeDataSet)
#print(NROW(getEmployeeDataSet))


 finalResultSet <- getFinalResultSet(getProjectDataSet,getEmployeeDataSet)
 finalResultSet <- finalResultSet[order(-finalResultSet$employeeIdentifiedValue , -finalResultSet$Bench_ageing),] 
 #print(NROW(finalResultSet))
 View(head(finalResultSet,n))
 
   }else
     print("please enter correct won")
}

  
