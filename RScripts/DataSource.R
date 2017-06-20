
ProjectDataSetFunction <- function(won)
{
  require(rio)
  
  
  projectDataSet <- import("data\\projectData\\projectData.xlsx")
  
  
  
  finalProjectDataset <- projectDataSet[which(as.numeric(projectDataSet$REQ_ID) == won),]
  ifelse(NROW(finalProjectDataset) > 0,return(finalProjectDataset),return(NULL))
  
}


testDataSource <- function(configGradeData,configCompetencyData)
{
  require(rio)
  
  
  testDataSet <- import("C:\\Users\\alok\\Desktop\\employeeData.xlsx")
  #View(testDataSet)
  
  
  #View(testDataSet)
  return(testDataSet)
}



ProjectDatasource <- function()
{
  require(rio)
  
  
  trainDataSet <- import("data\\projectData\\projectdata.ods")
  
  
  return(trainDataSet)
}


getWon <- function(trainDataSet)
{
  return(unique(trainDataSet$REQ_WON))
}