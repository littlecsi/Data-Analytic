## Start Script

base_data <- tryCatch(base_data,
         error = function(e) {
           source("script/_1_database/getDB.R")
           source("script/_2_functions/functions.R")
           source("script/_2_functions/mlfunctions.R")
           return(getSeattleCrimeData())
         })

income_data <- tryCatch(income_data,
        error=function(e) {
          return(getIncomeData())
        })