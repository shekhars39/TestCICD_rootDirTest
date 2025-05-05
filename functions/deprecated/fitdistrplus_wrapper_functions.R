# not used

# # fit distribution and generate ecdf graphs
# fit_dist_values <- function(dataset){
#   
#   # initial data using helper function
#   a <- envstats_initial_helper(dataset)
#   vals <- a$vals
#   cens <- a$cens
#   dist_detected <- a$dist_detected
#   
#   # data frame for fitdistr
#   # edited version of https://github.com/SwampThingPaul/NADA2/blob/master/R/cenCompareCdfs.R
#   ydat <- na.omit(data.frame(vals, cens))
#   y.var <- ydat[,1]
#   cen.var <- ydat[,2]
#   yname <- deparse(substitute("REPORT_RESULT_VALUE"))
#   
#   if(any(cens == 1)){
#     
#     # if doesn't have two non-missing, uncensored, distinct values
#     if(nrow(dist_detected) < 2){
#       
#       table <- tibble(note = "Does not contain at least two non-missing, uncensored, distinct values")
#       text <- ""
#       dist_text <- ""
#       graph <- blank_graph()
#       
#     } else {
#       # has enough uncensored
#       left <- y.var*(1-as.integer(cen.var))
#       right <- y.var
#       var.frame <- data.frame(left, right)
#       
#       dist_lnorm <- fitdistrplus::fitdistcens(var.frame, "lnorm")
#       dist_gamma <- fitdistrplus::fitdistcens(var.frame, "gamma")
#       dist_norm <- fitdistrplus::fitdistcens(var.frame, "norm")
#       
#       graph1 <- fitdistrplus::cdfcompcens(list(dist_lnorm, dist_gamma, dist_norm),
#                             legendtext = c("lognormal", "gamma", "normal"),
#                             xlab = "REPORT_RESULT_UNIT",
#                             fitcol = c("red", "blue", "orange"), fitlty = 1, fitlwd = 1, 
#                             plotstyle = "ggplot", addlegend = FALSE) 
#       
#       graph  <- graph1 +
#         theme_bw() +
#         theme(legend.position = "bottom")
#       
#       text <- "The grey box shows the censored values along with the emperical cumulative distribution function. "
#       
#     }
#     
#   } else {
#     # is all uncensored
#     dist_lnorm <- fitdistrplus::fitdist(y.var, "lnorm", "mle")
#     dist_gamma  <- fitdistrplus::fitdist(y.var, "gamma", "mle")
#     dist_norm  <- fitdistrplus::fitdist(y.var, "norm", "mle")
#     
#     # uncensored only
#     graph1 <- fitdistrplus::denscomp(list(dist_lnorm, dist_gamma, dist_norm),
#                        legendtext = c("lognormal", "gamma", "normal"),
#                        xlab = "REPORT_RESULT_UNIT",
#                        fitcol = c("red", "blue", "orange"), fitlty = 1, fitlwd = 1, 
#                        plotstyle = "ggplot", addlegend = FALSE) 
#     
#     graph  <- graph1 +
#       theme_bw() +
#       theme(legend.position = "bottom")
#     
#     text <- "The the empirical distribution is shown on top of the histogram of the values for this analyte."
#     
#   }
#   
#   if(nrow(dist_detected) >= 2){
#     # lowest BIC or highest log likelihood
#     table <- tribble(~dist, ~BIC, ~log_likelihood,
#                      "Lognormal", signif(dist_lnorm$bic, 3), signif(dist_lnorm$loglik, 3),
#                      "Gamma", signif(dist_gamma$bic, 3), signif(dist_gamma$loglik, 3),
#                      "Normal", signif(dist_norm$bic, 3), signif(dist_norm$loglik, 3)) %>% 
#       arrange(BIC)
#     
#     suggested <- slice(table, 1) %>% 
#       pull(dist)
#     
#     dist_text <- HTML(glue("The suggested distribution using Bayesian Information Criteria (BIC) is the <b>{suggested} distribution</b>. "))
#     
#   } 
#   
#   return(list("table" = table,
#               "graph" = graph,
#               "text" = text,
#               "dist_text" = dist_text))
# }
