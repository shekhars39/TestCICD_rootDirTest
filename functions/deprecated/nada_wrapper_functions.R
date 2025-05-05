# # Akritas-Theil-Sen line from NADA2

# Don't bother with it. Not used by EPA and difficult to get on the VM.

# nada2_ats <- function(dataset){
#   
#   # initial data using helper function
#   a <- envstats_initial_helper(dataset)
#   dist_dectected <- a$dist_dectected
#   
#   # clean for ATS
#   b <- dataset %>% 
#     mutate(dectime = decimal_date(SAMPLE_DATE),
#            censored = if_else(DETECT_FLAG == "N", 1, 0))
#   
#   if(nrow(dist_dectected) > 2){
#     table <- NADA2::ATSmini(b$REPORT_RESULT_VALUE,
#                             b$censored,
#                             b$dectime) %>% 
#       mutate(across(where(is.numeric), signif)) %>% 
#       mutate(method = "Akritas-Theil-Sen line")
#   } else {
#     table  <- tibble()
#   }
#   
#   return(table)
# }
