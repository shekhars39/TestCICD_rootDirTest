# extra graph grouping functions 


#' gets distinct CHEMICAL_NAME and CAS_RN for the graph groupings objects later
#' 
#' @return a dataframe
#' 
#' @param dataset
#' 
cas_rn_for_chemname_graph_groups <- function(dataset){
  
  # build the graph groupings from the CAS_RN object
  dataset %>% 
    distinct(CAS_RN, CHEMICAL_NAME) %>% 
    filter(CAS_RN %in% c("75-35-4", "74-86-2", "ALK", "16887-00-6", "DO", "74-84-0",
                         "74-85-1", "FE(FS)", "7439-96-5", "74-82-8", "14797-55-8",
                         "14797-65-0", "NO3NO2N", "ORP", "14808-79-8", "18496-25-8",
                         "127-18-4", "TOC", "79-01-6", "75-01-4", "156-59-2", "PH", "156-60-5")) %>% 
    arrange(CHEMICAL_NAME)
}



#' function to create the graph groupings object for coc_geochem
#' 
#' @return a dataframe
#' 
#' @param dataset
#' @param groups_list a list of the groups wot work with
#' @param analyte_short_df the analyte_short lookup table
#' 
create_graph_groupings <- function(dataset, groups_list, analyte_short_df){
  
  dataset %>%
    # groups_list comes from app_datasets.R
    mutate(group = case_when(CAS_RN %in% groups_list$`Chlorinated Ethene and Daughter Product` ~ "Chlorinated Ethene and Daughter Product",
                             CAS_RN %in% groups_list$`Redox Parameters` ~ "Redox Parameters",
                             CAS_RN %in% groups_list$`Other Geochemistry` ~ "Other Geochemistry",
                             TRUE ~ NA_character_),
           group2 = case_when(CAS_RN %in% groups_list$`Water Quality Parameters` ~ "Water Quality Parameters",
                              CAS_RN %in% groups_list$`TOC and Redox Parameters` ~ "TOC and Redox Parameters",
                              TRUE ~ NA_character_),
           req_unit = case_when(CAS_RN %in% c("ALK", "16887-00-6", "DO", "FE(FS)", "7439-96-5", "74-82-8",
                                              "14797-55-8", "14797-65-0", "NO3NO2N", "14808-79-8", "18496-25-8", "TOC") ~ "mg/L",
                                CAS_RN %in% c("75-35-4", "74-86-2", "74-84-0", "74-85-1", "127-18-4", "79-01-6", "75-01-4", "156-59-2", "156-60-5") ~ "ug/L",
                                CAS_RN == "ORP" ~ "mV",
                                CAS_RN == "PH" ~ "su")) %>%
    # analyte_short_df comes from app_datasets.R
    left_join(analyte_short_df, by = "CAS_RN") %>% 
    mutate(CAS_RN = factor(CAS_RN, levels = groups_list$order)) %>%
    arrange(CAS_RN) %>% 
    select(group, CAS_RN, CHEMICAL_NAME, req_unit, analyte_short, group2)
}
