# original envstats kendall wratpper function used maybe with the old mk function

envstats_kendall <- function(dataset, args, conf_level = 0.95){
  
  fit <- EnvStats::kendallTrendTest(REPORT_RESULT_VALUE ~ SAMPLE_DATE,
                                    data = dataset,
                                    alternative = args,
                                    conf.level = conf_level)
  
  tibble(kendall_S = fit$S,
         std_dev_S = sqrt(fit$var.S),
         p_value = fit$p.value,
         slope = fit$estimate[2],
         intercept = fit$estimate[3],
         alternative = fit$alternative,
         method = str_squish(str_remove_all(fit$method, "\\n")),
         est_method = str_squish(str_remove_all(fit$estimation.method, "\\n"))) %>%
    mutate(across(where(is.numeric), signif))
  
}