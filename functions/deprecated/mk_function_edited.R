#' Mann Kendall Analysis for Multiple Chemicals and Location
#' 
#' This function uses the MannKendall function from the Kendall package in order to run the Mann Kendall analysis. It requires a chemical data-frame which contains SAMPLE_DATE, REPORT_RESULT_VALUE, SYS_LOC_CODE, and CHEMICAL_NAME. All other columns will not be used in this function.
#' @param sample_result a dataframe which contains SAMPLE_DATE, REPORT_RESULT_VALUE, SYS_LOC_CODE, and CHEMICAL_NAME. Mann Kendall results are calculated based off this dataframe. This dataframe should ideally be generated with 'table_combination'.
#' @param sites An optional list (eg. c("MW02","MW33","GWM03")) which specifies which locations the analysis is to be done over. If no list is provided the analysis is performed over all sites.
#' @param chemicals An optional list (eg. c("Aluminium","Trichloroethene")) which specifies which chemicals the analysis is to be done over. If no list is provided the analysis is performed over all chemicals.
#' @param P The confidence in the answer (0.95 means 95 per cent confidence). This value is converted into the appropriate Z score for a two-tailed test.
#' @examples mannkendall_analysis(sample_result, chemicals = c("Trichloroethene", "Aluminium"))
#' @export
#' @import dplyr
#' @import Kendall
#' @import magrittr
#' @import lubridate
#' @return This function returns a dataframe where each row maps to a unique combination of Chemical and Site. The columns include the 5 parameters calculated by Mann Kendall. Additionally the Z score, CHEMICAL_NAME, SYS_LOC_CODE, trend, direction, number of entries, and minimum and and maximum values are included as columns. 

# edited to work with the cleaned up equis data
# also to remove the magrittr assignment pipe -this one %<>% 
mannkendall_analysis <- function(sample_result,
                                 sites = 'default',
                                 chemicals = 'default',
                                 groups = NULL,
                                 P = 0.95) {
  
  # This first section is literally just primitive error checking to make sure the right columns are in place
  required <- c("SAMPLE_DATE", "REPORT_RESULT_VALUE", "SYS_LOC_CODE", "CHEMICAL_NAME")
  
  temp_probability <-  abs(qnorm((1 - P)/2)) # this calculates the Z score-threshold used for determining if a trend is present
  
  mkdata <- sample_result %>%
    mutate_if(.predicate = is.factor, .funs = as.character)
  
  if (sites == "default") {
    actualSites = unique(mkdata$SYS_LOC_CODE)
  } else {
    actualSites = sites
  }
  
  # Sorts out the dataframe into the right form.
  mkdata <- mkdata %<>%
    filter(SYS_LOC_CODE %in% actualSites) %>%# & sample_type =='Normal') %>%
    arrange(SYS_LOC_CODE, SAMPLE_DATE) #%>%
  # # SampleDates doesn't get used anywhere in this, but was in original
  # mutate(SampleDates = as.Date(SAMPLE_DATE, "%d-%m-%y")) 
  
  
  mkdata_count <- mkdata  # mkdata_count equals an n which counts the number of different loccodes-analytes
  
  if(is.null(groups)) { 
    mkdata_count$groups <- "NA"
  } else {
    mkdata_count$groups <- unlist(mkdata_count[, groups])
  }
  mkdata_count <- mkdata_count %>%
    group_by(SYS_LOC_CODE, CHEMICAL_NAME, groups) %>%
    summarise(n = length(REPORT_RESULT_VALUE), min = min(REPORT_RESULT_VALUE), max=max(REPORT_RESULT_VALUE)) %>%
    filter(n>2)
  
  
  if (chemicals == "default") {
    actualChemicals = unique(mkdata_count$CHEMICAL_NAME)
  } else {
    actualChemicals = chemicals
  }
  
  
  for(cn in actualChemicals) { # for each unique CHEMICAL_NAME in mkdata and for each unique locationcode for that CHEMICAL_NAME, do the mann kendall analysis
    print(cn)
    
    temp_chemfiltered <- mkdata[which(mkdata$CHEMICAL_NAME==cn),]
    mk_count_filtered <- mkdata_count %>%
      filter(CHEMICAL_NAME == cn)
    
    
    # A little something you should know, FAULT 12 just means there are less than 12 data points
    # so the p value calculated is not that reliable - that's all it means
    
    for(l in unique(mk_count_filtered$SYS_LOC_CODE)){
      print(l)
      
      final_filtered <- temp_chemfiltered %>%
        filter(SYS_LOC_CODE == l)
      
      tmp <-  as.data.frame(t(unlist(
        MannKendall(
          final_filtered$REPORT_RESULT_VALUE)))) %>%
        mutate(SYS_LOC_CODE = l,
               CHEMICAL_NAME = cn)
      
      if(exists("MK_summary")) {
        MK_summary <- bind_rows(MK_summary, tmp) #if the mk_summary object exists, bind mk_summary with tmp
      } else{
        MK_summary <- tmp
      }
    }
  }
  
  # The following sections logically determine what the Mann Kendall results mean
  MK_summary2 <-  left_join(MK_summary, mkdata_count, by = c("SYS_LOC_CODE", "CHEMICAL_NAME")) %>%
    filter(CHEMICAL_NAME %in% actualChemicals) %>%
    mutate(
      
      Zs = case_when(
        n <= 10 ~ 1000, # Z value irrelevant for smaller sample sizes as p value determines by S
        S > 0 ~ ((S-1)/(varS^0.5)),
        S < 0 ~ ((S+1)/(varS^0.5)),
        TRUE ~ 1000),
      
      trend = case_when(
        n == 3 ~ "Not enough data",
        
        n == 4 & abs(S) >= 9 ~ "Trend",
        n == 4 & abs(S) < 9 ~ "No Trend",
        
        n == 5 & abs(S) >= 10 ~ "Trend",
        n == 5 & abs(S) < 10 ~ "No Trend",
        
        n == 6 & abs(S) >= 13 ~ "Trend",
        n == 6 & abs(S) < 13 ~ "No Trend",
        
        n == 7 & abs(S) >= 15 ~ "Trend",
        n == 7 & abs(S) < 15 ~ "No Trend",
        
        n == 8 & abs(S) >= 18 ~ "Trend",
        n == 8 & abs(S) < 18 ~ "No Trend",
        
        n == 9 & abs(S) >= 20 ~ "Trend",
        n == 9 & abs(S) < 20 ~ "No Trend",
        
        n == 10 & abs(S) >= 23 ~ "Trend",
        n == 10 & abs(S) < 23 ~ "No Trend",
        
        n > 10 & abs(Zs) > temp_probability ~ "Trend",
        n > 10 & abs(Zs) < temp_probability ~ "No Trend"),
      
      direction = case_when(
        trend == "Trend" & S > 0 ~ "Increasing",
        trend == "Trend" & S < 0 ~ "Decreasing",
        trend == "Trend" & S == 0 ~ "No Trend",
        
        trend == "Trend" & Zs > 0 ~ "Increasing",
        trend == "Trend" & Zs < 0 ~ "Decreasing",
        trend == "Trend" & Zs == 0 ~ "No Trend",
        trend == "No Trend" ~ "No Trend",
        trend == "Not enough data" ~ "Not enough data") # fixes ggplot issue
    ) %>%
    arrange(SYS_LOC_CODE, CHEMICAL_NAME) %>%
    as_tibble()
  
  
  if(!is.null(groups)) MK_summary2 %>% left_join(sample_result %>% select_(groups, "SYS_LOC_CODE"), by = "SYS_LOC_CODE")
  
  MK_plot <- MK_summary2 %>%
    mutate(SYS_LOC_CODE = factor(SYS_LOC_CODE, levels = actualSites))
  
  plot <- ggplot() +
    geom_point(data=MK_plot,
               aes(x = SYS_LOC_CODE,
                   y = CHEMICAL_NAME,
                   shape = direction,
                   size = direction,
                   colour = direction)) +
    scale_shape_manual(
      name = "Trend Direction",
      labels = c("Decreasing", "Increasing", "No Trend", "Not enough data"),
      values = c("\u2B63", "\u2B61", "\u2012", 4)) +
    scale_color_manual(
      name = "Trend Direction",
      labels = c("Decreasing", "Increasing", "No Trend", "Not enough data"),
      values = c("#1d91c0", "#ff7f00", "#999999", "#999999")) +
    scale_size_manual(
      name = "Trend Direction",
      labels = c("Decreasing", "Increasing", "No Trend", "Not enough data"),
      values = c(12, 12, 6, 5))+
    theme_bw() +
    labs(y = 'Analytes',
         x = 'Monitoring Location',
         title = "Mann-Kendall Trend Analysis",
         subtitle = "Symbols indicate the trend in the data for each analyte at each location") +
    scale_y_discrete("Analytes", position="right") +
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.y = element_text(angle = 180, size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          legend.title = element_text(face = "bold", size = 12)) +
    guides(shape = guide_legend(override.aes = list(size = 8)))
  
  return(list("table" = MK_summary2,
              "plot" = plot,
              "actualChemicals" = actualChemicals))
}



# the same but vectorised and without the plot
mannkendall_analysis2 <- function(sample_result,
                                 sites = 'default',
                                 chemicals = 'default',
                                 groups = NULL,
                                 P = 0.95) {

  # this calculates the Z score-threshold used for determining if a trend is present
  temp_probability <-  abs(qnorm((1 - P)/2)) 
  
  mkdata1 <- sample_result %>%
    mutate_if(.predicate = is.factor, .funs = as.character)
  
  actualSites <- if_else(sites == "default", unique(mkdata1$SYS_LOC_CODE), sites)
  
  mkdata <- mkdata1 %<>%
    filter(SYS_LOC_CODE %in% actualSites) %>%# & sample_type =='Normal') %>%
    arrange(SYS_LOC_CODE, SAMPLE_DATE)
  
  mkdata_count1 <- mkdata  # mkdata_count equals an n which counts the number of different loccodes-analytes
  
  # groups for mkdata_count - min of 2?
  if(is.null(groups)) { 
    mkdata_count1$groups <- "NA"
  } else {
    mkdata_count1$groups <- unlist(mkdata_count[, groups])
  }
  
  mkdata_count <- mkdata_count1 %>%
    group_by(SYS_LOC_CODE, CHEMICAL_NAME, groups) %>%
    summarise(n = length(REPORT_RESULT_VALUE),
              min = min(REPORT_RESULT_VALUE),
              max = max(REPORT_RESULT_VALUE)) %>%
    filter(n>2)
  
  actualChemicals <- if_else(chemicals == "default", unique(mkdata_count$CHEMICAL_NAME), chemicals)
  
  # distint values to map through
  dist_chems_locs <- mkdata %>% 
    distinct(CHEMICAL_NAME, SYS_LOC_CODE)
  
  # vectorised function should be faster
  mks <- function(dataset, chem, loc){
    print(paste0(chem, loc))
    
    df <- dataset %>% 
      filter(CHEMICAL_NAME == chem,
             SYS_LOC_CODE == loc)
    
    as.data.frame(t(unlist(
      MannKendall(
        df$REPORT_RESULT_VALUE)))) %>%
      mutate(SYS_LOC_CODE = loc,
             CHEMICAL_NAME = chem)
  }
  
  # output from map function
  MK_summary <- map2_df(dist_chems_locs$CHEMICAL_NAME,
                        dist_chems_locs$SYS_LOC_CODE,
                        ~mks(mkdata, .x, .y))
  
  
  # The following sections logically determine what the Mann Kendall results mean
  MK_summary2 <-  left_join(MK_summary, mkdata_count, by = c("SYS_LOC_CODE", "CHEMICAL_NAME")) %>%
    filter(CHEMICAL_NAME %in% actualChemicals) %>%
    mutate(
      
      Zs = case_when(
        n <= 10 ~ 1000, # Z value irrelevant for smaller sample sizes as p value determines by S
        S > 0 ~ ((S-1)/(varS^0.5)),
        S < 0 ~ ((S+1)/(varS^0.5)),
        TRUE ~ 1000),
      
      trend = case_when(
        n == 3 ~ "Not enough data",
        
        n == 4 & abs(S) >= 9 ~ "Trend",
        n == 4 & abs(S) < 9 ~ "No Trend",
        
        n == 5 & abs(S) >= 10 ~ "Trend",
        n == 5 & abs(S) < 10 ~ "No Trend",
        
        n == 6 & abs(S) >= 13 ~ "Trend",
        n == 6 & abs(S) < 13 ~ "No Trend",
        
        n == 7 & abs(S) >= 15 ~ "Trend",
        n == 7 & abs(S) < 15 ~ "No Trend",
        
        n == 8 & abs(S) >= 18 ~ "Trend",
        n == 8 & abs(S) < 18 ~ "No Trend",
        
        n == 9 & abs(S) >= 20 ~ "Trend",
        n == 9 & abs(S) < 20 ~ "No Trend",
        
        n == 10 & abs(S) >= 23 ~ "Trend",
        n == 10 & abs(S) < 23 ~ "No Trend",
        
        n > 10 & abs(Zs) > temp_probability ~ "Trend",
        n > 10 & abs(Zs) < temp_probability ~ "No Trend"),
      
      direction = case_when(
        trend == "Trend" & S > 0 ~ "Increasing",
        trend == "Trend" & S < 0 ~ "Decreasing",
        trend == "Trend" & S == 0 ~ "No Trend",
        
        trend == "Trend" & Zs > 0 ~ "Increasing",
        trend == "Trend" & Zs < 0 ~ "Decreasing",
        trend == "Trend" & Zs == 0 ~ "No Trend",
        trend == "No Trend" ~ "No Trend",
        trend == "Not enough data" ~ "Not enough data") # fixes ggplot issue
    ) %>%
    arrange(SYS_LOC_CODE, CHEMICAL_NAME) %>%
    as_tibble()
  
  
  return(list("table" = MK_summary2,
              "actualChemicals" = actualChemicals))
}


# mannkendall_analysis3 ----------------------------------------
# the safe version of mannkendall_analysis2
mannkendall_analysis3 <- safely(mannkendall_analysis2)
