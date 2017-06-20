availabilityTest <- function(projectStartDate , projectEndDate , getEmpAvailailabiltyFrom , getEmpAvailailabiltyTill)
  
{
  tryCatch(
 if ( as.Date(projectStartDate,"%m/%d/%Y") >= as.Date(getEmpAvailailabiltyFrom,"%m/%d/%Y")) {
   
   projectRequiredNoOfDays <- as.Date(projectEndDate,"%m/%d/%Y") - Sys.Date()
   EmpAvailableDays <- as.Date(getEmpAvailailabiltyTill,"%m/%d/%Y") - Sys.Date()
  
        return(ifelse( EmpAvailableDays >= projectRequiredNoOfDays , return(2.5) , 
               ifelse(EmpAvailableDays >= 0.8*projectRequiredNoOfDays , return(1.5),0.5)))
   
 }else
   return(-3)
  
  
  )
}

