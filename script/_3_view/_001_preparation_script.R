## Start Script
source("script/_1_database/getDB.R")
source("script/_2_functions/functions.R")
source("script/_2_functions/mlfunctions.R")

base_data <- tryCatch(base_data,
         error = function(e) {
           return(getSeattleCrimeData('*'))
         })