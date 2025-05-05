# helper functions ---------------------------------------------------------------------------

#' creates named colour vectors from data and selected columns - for the coc graphs
#'
#' @return a named vector of colours
#' @param colour_df the colour input data frame
#' @param data_df the dataset
#' @param columns the relevant columns to select
#' @param name_col the column where the names are
#' 
named_colour_function <- function(colour_df, data_df, columns, name_col){
  
  gc_df <- colour_df %>% 
    left_join(data_df %>% 
                select(all_of(columns)),
              by = "CAS_RN") %>% 
    distinct() %>% 
    filter(!is.na(.data[[name_col]]))
  
  col_vec <- gc_df$colour_code
  names(col_vec) <- gc_df[[name_col]]
  
  return(col_vec)
  
}

#' gets the colour palette based on what is chosen and the number of colours required
#' See colour_palettes object in app_constants.R - this is likely to be passed in to the col_pal argument
#'
#' @return a vector of the colours
#' @param col_pal the name of the palette
#' @param ncolours the number of colours
#' 
colour_palette_selection <- function(col_pal, ncolours){
  
  if(col_pal %in% c("R4", "Classic Tableau")){
    pal <- palette.colors(ncolours, palette = col_pal) |> 
      as.vector()
  } else {
    # "Pastel 1", "Dark 2", "Spectral", "Viridis", "Inferno", "Mako", "Plasma", "Rocket"
    pal <- hcl.colors(ncolours, palette = col_pal) |> 
      as.vector()
  }
  return(pal)
}

#' blank graph for use when there is no data
#'
#' @return a blank graph
#' @param label_text the text on the graph
#' 
blank_graph <- function(label_text = "No data"){
  
  ggplot() +
    theme_bw() +
    labs(x = NULL, 
         y = NULL) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    annotate("text", x = 1, y = 1, label = label_text)
}

#' dataset for the molar_conc_graph() function
#'
#' @return a dataframe
#' @param dataset 
#' @param well_selected the wells to show
#' @param analytes_selected the analytes to show
#' @param molar_mass_table a lookup table of molar concentrations 
#' @param factor_order_vector a vector for the factor order - analyte short
#' 
get_molar_conc_df <- function(dataset, analytes_selected, well_selected, molar_mass_table, factor_order_vector){
  
  # factor order for chems
  chem_factor_order <- dataset %>% 
    distinct(analyte_short, CAS_RN, CHEMICAL_NAME) %>% 
    filter(analyte_short %in% factor_order_vector) %>% 
    mutate(analyte_short = factor(analyte_short, levels = factor_order_vector)) %>% 
    pull(CHEMICAL_NAME)
  
  df <- dataset %>%
    filter(CHEMICAL_NAME %in% analytes_selected,
           SYS_LOC_CODE == well_selected,
           CAS_RN != "16887-00-6", # don't include Chloride
           DETECT_FLAG == "Y") %>%
    # this bit because there may be duplicates for specific dates !!!!!!!!!!
    group_by(CAS_RN, SAMPLE_DATE) %>%
    summarise(REPORT_RESULT_VALUE = sum(REPORT_RESULT_VALUE)) %>%
    ungroup() %>%
    left_join(molar_mass_table, by = "CAS_RN") %>%
    left_join(dataset %>% 
                filter(CHEMICAL_NAME %in% analytes_selected) %>% 
                distinct(CAS_RN, CHEMICAL_NAME),
              by = "CAS_RN") %>% 
    mutate(value = REPORT_RESULT_VALUE/molar_mass) %>%
    # fill empty
    tidyr::complete(CHEMICAL_NAME, SAMPLE_DATE, fill = list(value = 0)) %>% 
    # make sure the cas rn are filled in after the complete
    left_join(dataset %>% 
                filter(CHEMICAL_NAME %in% analytes_selected) %>% 
                distinct(CAS_RN, CHEMICAL_NAME) %>% 
                rename(CAS_RN2 = CAS_RN),
              by = "CHEMICAL_NAME") %>% 
    mutate(CAS_RN = if_else(is.na(CAS_RN), CAS_RN2, CAS_RN)) %>% 
    select(-CAS_RN2) %>% 
    # chem_factor_order from above
    mutate(CHEMICAL_NAME = factor(CHEMICAL_NAME, levels = rev(chem_factor_order))) |> 
    arrange(SAMPLE_DATE) |> 
    # for proportion values
    group_by(SAMPLE_DATE) |> 
    mutate(prop_value = value / sum(value)) |> 
    ungroup()
  
  return(df)
}

#' dataset for the molar_conc_sum_graph function
#'
#' @return a list of two datasets, interim and final
#' @param dataset 
#' @param wells_selected the wells to show
#' @param analytes_selected the analytes to show
#' @param molar_mass_table a lookup table of molar concentrations 
#' @param min_date the start date of the graph
#' @param max_date the end date of the graph
#' 
get_molar_conc_sum_df <- function(dataset, analytes_selected, wells_selected, molar_mass_table, min_date, max_date){
  
  # interim table not summarised
  interim <- dataset %>%
    filter(CHEMICAL_NAME %in% analytes_selected,
           SYS_LOC_CODE %in% wells_selected,
           DETECT_FLAG == "Y",
           SAMPLE_DATE >= min_date,
           SAMPLE_DATE <= max_date) %>%
    # this bit because there may be duplicates for specific dates !!!!!!!!!!
    group_by(SYS_LOC_CODE, CAS_RN, SAMPLE_DATE) %>%
    summarise(REPORT_RESULT_VALUE = sum(REPORT_RESULT_VALUE)) %>%
    ungroup() %>%
    left_join(molar_mass_table, by = "CAS_RN") %>%
    left_join(dataset %>% 
                filter(CHEMICAL_NAME %in% analytes_selected) %>% 
                distinct(CAS_RN,REPORT_RESULT_UNIT),
              by = "CAS_RN") %>% 
    mutate(value = REPORT_RESULT_VALUE/MOLECULAR_WEIGHT) %>%
    select(SYS_LOC_CODE, CAS_RN, CHEMICAL_NAME, SAMPLE_DATE, REPORT_RESULT_VALUE, REPORT_RESULT_UNIT, MOLECULAR_WEIGHT, value) |> 
    arrange(SAMPLE_DATE) 
  
  # final - summarised
  final <- interim |> 
    # for sum values
    group_by(SAMPLE_DATE, SYS_LOC_CODE) |> 
    summarise(value = sum(value)) |> 
    ungroup()
  
  return(list("final" = final,
              "interim" = interim))
}


#' dataset for the total_voc_graph function
#'
#' @return a list of two datasets, interim and final
#' @param dataset 
#' @param analytes_selected the analytes to show
#' @param wells_selected the wells to show
#' @param use_rep_value whether to use a representative value - 'Yes' or 'No'
#' @param rep_value  representative value to use
#' 
get_total_voc_df <- function(dataset, analytes_selected, wells_selected, use_rep_value, rep_value){
  
  # initial
  initial <- dataset |> 
    filter(CHEMICAL_NAME %in% analytes_selected,
           SYS_LOC_CODE %in% wells_selected) |> 
    mutate(CHEMICAL_NAME = factor(CHEMICAL_NAME, levels = analytes_selected)) |> 
    select(SYS_LOC_CODE:MATRIX_CODE) 
  
  # not summarised - just the detected values
  interim1 <- initial |> 
    filter(DETECT_FLAG == "Y") |> 
    mutate(REP_VALUE_FLAG = "N")
  
  # interim summarised
  interim_summarised <- interim1 |> 
    group_by(SYS_LOC_CODE, SAMPLE_DATE, REP_VALUE_FLAG) |> 
    summarise(REPORT_RESULT_VALUE = sum(REPORT_RESULT_VALUE)) |> 
    ungroup() |> 
    arrange(SYS_LOC_CODE, SAMPLE_DATE)
  
  
  if(use_rep_value == "Yes"){
    
    # dates and wells where all NA - add flag and rep value
    rep_values_to_add <- initial |> 
      group_by(SYS_LOC_CODE, SAMPLE_DATE) |>
      arrange(SYS_LOC_CODE, SAMPLE_DATE) |> 
      summarise(ND = sum(if_else(DETECT_FLAG == "N", 1, 0)),
                D = sum(if_else(DETECT_FLAG == "Y", 1, 0))) |> 
      mutate(REP_VALUE_FLAG = if_else(ND == ND + D, "Y", "N")) |> 
      filter(REP_VALUE_FLAG == "Y") |> 
      mutate(REPORT_RESULT_VALUE = rep_value) |> 
      select(SYS_LOC_CODE, SAMPLE_DATE, REP_VALUE_FLAG, REPORT_RESULT_VALUE) |> 
      ungroup()
    
    print(rep_values_to_add)
    
    interim2 <- bind_rows(interim_summarised, rep_values_to_add) 
    
  } else {
    # summarised - no representative value
    interim2 <- interim_summarised
  }
  
  final <- interim2 |> 
    mutate(REP_VALUE_FLAG = factor(REP_VALUE_FLAG, levels = c("N", "Y"))) |> 
    arrange(SYS_LOC_CODE, SAMPLE_DATE)

  return(list("final" = final,
              "interim" = interim1))
}

#' gets the annotation text values
#'
#' @return annotation text dataframe
#' @param dataset
#' 
get_ann_text_df <- function(dataset, mid_date){
  
  dataset %>% 
    distinct(CHEMICAL_NAME,
             adjusted_criteria_value,
             REPORT_RESULT_UNIT,
             CRITERIA_TEXT) |> 
    mutate(SAMPLE_DATE = mid_date) |> 
    rename(REPORT_RESULT_VALUE  = adjusted_criteria_value) |>
    convert_mu_symbol(REPORT_RESULT_UNIT) |>
    mutate(label_text = if_else(!is.na(REPORT_RESULT_VALUE),
                                glue("{CRITERIA_TEXT} ({REPORT_RESULT_VALUE} {REPORT_RESULT_UNIT})"),
                                "")) |> 
    # only want it there is a value - this is probably a long way of doing it
    filter(!is.na(REPORT_RESULT_VALUE))
  
}

# coc geochem graphs ------------------------------------------------------------------------

#' TOC and Redox graph on the COC Geochem page
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param well_selected the wells to show
#' @param analytes_selected the analytes to show
#' @param min_date the start date of the graph
#' @param max_date the end date of the graph
#' @param y_scale the scale on the y axis- normal or log10
#' @param graph_groupings the groupings used in the graph
#' @param colour_dataset the colours used for the analytes
#' @param annotation_df annotation_lines dataset
#' @param show_annotations show the annotation lines or not
#' @param chem_lookup CAS_RN and CHEMICAL_NAME lookup table
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#' 
toc_redox_graph_ggg_v4 <- function(dataset, well_selected, analytes_selected, 
                                   min_date, max_date, y_scale = "normal",
                                   graph_groupings, colour_dataset = graph_colours_df, 
                                   annotation_df, show_annotations, chem_lookup, is_ggiraph = FALSE){
  
  # annotations - filter out if not wanted
  if(show_annotations == "No"){
    annotation_df <- annotation_df |> 
      filter(is.na(DATE))
  }
  
  # the CAS RNs for the chem order
  toc_redox_factor_order_cas_rn <- c("TOC", "DO", "NO3NO2N", "14797-55-8", "14797-65-0",
                                     "FE(FS)", "7439-96-5", "14808-79-8", "74-82-8", "18496-25-8", "ORP")
  
  # order for graphs below
  the_order <- dataset %>% 
    filter(CHEMICAL_NAME %in% analytes_selected) %>% 
    distinct(CAS_RN, CHEMICAL_NAME, REPORT_RESULT_UNIT) %>% 
    mutate(CAS_RN = factor(CAS_RN, levels = toc_redox_factor_order_cas_rn)) %>% 
    arrange(CAS_RN) %>% 
    mutate(combined_facet = glue("{CHEMICAL_NAME} ({REPORT_RESULT_UNIT})")) %>% 
    pull(combined_facet)
  
  # data
  df <- dataset %>%
    filter(CHEMICAL_NAME %in% analytes_selected,
           SYS_LOC_CODE == well_selected) %>%
    ### version with combined_facets
    mutate(combined_facet = glue("{CHEMICAL_NAME} ({REPORT_RESULT_UNIT})")) %>% 
    arrange(SAMPLE_DATE, combined_facet) 
  
  # colour vector from helper function
  named_colour_vec <- named_colour_function(colour_dataset,
                                            df,
                                            c("CAS_RN", "CHEMICAL_NAME", "combined_facet"),
                                            "CHEMICAL_NAME")
  
  # same scale
  min_val <- min(df$REPORT_RESULT_VALUE)
  max_val <- max(df$REPORT_RESULT_VALUE)
  
  if(nrow(df) > 1){
    
    mid_date <- min_date + (max_date - min_date) / 2
    
    # date breaks depend on number of years in dataset
    num_years <- interval(min_date, max_date) / years(1)
    if(num_years > 8){
      date_break_value <- "2 years"
    } else {
      date_break_value <- waiver()
    }
    
    # df for when no data exists
    empty_df <- graph_groupings %>% 
      filter(group2 == "TOC and Redox Parameters") %>% 
      mutate(combined_facet = glue("{CHEMICAL_NAME} ({req_unit})")) %>% 
      # get the ones not in df
      filter(!CAS_RN %in% unique(df$CAS_RN)) %>% 
      select(CHEMICAL_NAME, analyte_short, combined_facet) %>% 
      mutate(SAMPLE_DATE = mid_date,
             REPORT_RESULT_VALUE = NA_real_,
             empty = "No data",
             DETECT_FLAG = "N") %>% 
      left_join(chem_lookup,
                by = "CHEMICAL_NAME") 
    
    # initially bind
    df1 <- bind_rows(df, empty_df)
    
    # get the facet order
    the_order <- df1 %>% 
      filter(CHEMICAL_NAME %in% analytes_selected) %>% 
      distinct(CAS_RN, CHEMICAL_NAME, REPORT_RESULT_UNIT, combined_facet) %>% 
      mutate(CAS_RN = factor(CAS_RN, levels = toc_redox_factor_order_cas_rn)) %>% 
      arrange(CAS_RN) %>%
      pull(combined_facet)
    
    # combine so as to show where no text 
    df2 <- bind_rows(df, empty_df) |> 
      mutate(combined_facet = factor(combined_facet, level = the_order))
    
    # initial graph
    g1 <- ggplot(df2, aes(SAMPLE_DATE, REPORT_RESULT_VALUE, colour = CHEMICAL_NAME)) +
      theme_bw() +
      geom_line() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    # intermediate graph depending on if ggiraph
    if(is_ggiraph == TRUE){
      g2 <- g1 +
        geom_point_interactive(size = 2, aes(shape = DETECT_FLAG,
                                             tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                 CHEMICAL_NAME: {CHEMICAL_NAME}
                                 SYS_LOC_CODE: {SYS_LOC_CODE}
                                 REPORT_RESULT_VALUE: {REPORT_RESULT_VALUE}
                                 REPORT_RESULT_UNIT: {REPORT_RESULT_UNIT}
                                 QUALIFIER: {INTERPRETED_QUALIFIERS}
                                 DETECT_FLAG: {DETECT_FLAG}")),
                               show.legend = TRUE) 
    } else {
      g2 <- g1 +
        geom_point(size = 3,
                   aes(shape = DETECT_FLAG),
                   show.legend = TRUE)
    }
    
    # show vertical annotation lines
    if(nrow(annotation_df) > 0){
      
      # named vector
      ann_linetype <- annotation_df |> 
        pull(LINETYPE, LABEL)
      
      g2a <- g2 +
        geom_vline(data = annotation_df,
                   aes(xintercept = DATE, linetype = LABEL), # linetype as dummy here
                   colour = "#808080", linewidth = 0.5) +
        scale_linetype_manual(values = ann_linetype)
      
    } else {
      g2a <- g2
    }
    
    # then the rest
    g3 <- g2a +
      scale_x_date(labels = scales::label_date_short(),
                   limits = c(min_date, max_date),
                   date_breaks = date_break_value,
                   date_minor_breaks = "1 year") +
      facet_wrap(~combined_facet, ncol = 2, scales = "free_y", drop = FALSE) +
      theme(legend.position = "bottom",
            legend.box = "vertical") +
      scale_shape_manual(values = c("N" = 1, "Y" = 19),
                         labels = c("N" = "Not detected", "Y" = "Detected"),
                         drop = FALSE) +
      scale_colour_manual(values = named_colour_vec,
                          guide = "none") +
      labs(title = glue("{well_selected} TOC and Redox Parameters"),
           x = NULL,
           y = NULL,
           colour = NULL,
           shape = NULL,
           linetype = NULL) +
      # empty text
      geom_text(data = df2 %>% 
                  filter(!is.na(empty)),
                aes(SAMPLE_DATE, y = 0.5, label = empty), colour = "darkgrey")  +
      guides(linetype = guide_legend(override.aes = list(shape = NA)))
    
    
    # depending on log10 scale
    if(y_scale == "log10"){
      g <- g3 +
        scale_y_log10(limits = c(closest_lower(cust_y_poss_vals, min_val),
                                 closest_higher(cust_y_poss_vals, max_val))) + # note that negative numbers (for ORP) won't show up in log10 scale
        labs(subtitle = "Mass Concentrations (log10 Scale)")
      
    } else {
      # normal
      g <- g3 +
        labs(subtitle = "Mass Concentrations") +
        # to zero or lower if has negatives
        scale_y_continuous(limits = ~c(min(0, min(.x)), ceiling(max(.x)))) 
    }
    
  } else {
    
    # NULL object that is handled later. not a ggplot2 blank_graph() as previously
    g <- NULL
    
  }
  
  # output dependent on whether ggiraph or not
  if(is_ggiraph == TRUE & !is.null(g)){
    
    girafe(ggobj = g,
           # no button - there is a manual one
           options = list(opts_toolbar(saveaspng = FALSE)))
    
  } else {
    g
  }
  
} # end function


#' water quality facets with total alkalinty, pH and Chloride on the COC Geochem page
#'
#' @return ggplot or ggiraph object, faceted for water quality params
#' @param dataset 
#' @param well_selected the wells to show
#' @param analytes_selected the analytes to show
#' @param min_date the start date of the graph
#' @param max_date the end date of the graph
#' @param colour_dataset the colours used for the analytes
#' @param annotation_df annotation_lines dataset
#' @param show_annotations show the annotation lines or not
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#' 
water_quality_graph_ggg_v4 <- function(dataset, well_selected, analytes_selected,
                                       min_date, max_date, colour_dataset = graph_colours_df,
                                       annotation_df, show_annotations, is_ggiraph = FALSE){
  # the CAS RNs
  wq_cas_rn <- c("ALK", "PH", "16887-00-6")
  
  # data
  df <- dataset |> 
    filter(SYS_LOC_CODE == well_selected,
           # alkalinity, pH, chloride
           CAS_RN %in% wq_cas_rn) |> 
    mutate(combined_facet = glue("{CHEMICAL_NAME} ({REPORT_RESULT_UNIT})"))
  
  # the facet order
  wq_order <- df |> 
    distinct(CAS_RN, combined_facet) |> 
    mutate(CAS_RN = factor(CAS_RN, levels = wq_cas_rn)) |> 
    arrange(CAS_RN) |> 
    pull(combined_facet)

  # colour vector from helper function
  named_colour_vec <- named_colour_function(colour_dataset,
                                            df,
                                            c("CAS_RN", "CHEMICAL_NAME"),
                                            "CHEMICAL_NAME")
  
  # annotations - filter out if not wanted
  if(show_annotations == "No"){
    annotation_df <- annotation_df |> 
      filter(is.na(DATE))
  }
  
  # graph data if there is data
  if(nrow(df) > 1){
    
    mid_date <- min_date + (max_date - min_date) / 2
    
    # date breaks depend on number of years in dataset
    num_years <- interval(min_date, max_date) / years(1)
    if(num_years > 8){
      date_break_value <- "2 years"
    } else {
      date_break_value <- waiver()
    }
    
    # initial graph
    g1 <- ggplot(df, aes(SAMPLE_DATE, REPORT_RESULT_VALUE, colour = CHEMICAL_NAME)) +
      theme_bw() +
      geom_line() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
    # intermediate graph depending on if ggiraph
    if(is_ggiraph == TRUE){
      g2 <- g1 +
        geom_point_interactive(size = 2, aes(shape = DETECT_FLAG,
                                             tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                 CHEMICAL_NAME: {CHEMICAL_NAME}
                                 SYS_LOC_CODE: {SYS_LOC_CODE}
                                 REPORT_RESULT_VALUE: {REPORT_RESULT_VALUE}
                                 REPORT_RESULT_UNIT: {REPORT_RESULT_UNIT}
                                 QUALIFIER: {INTERPRETED_QUALIFIERS}
                                 DETECT_FLAG: {DETECT_FLAG}")),
                               show.legend = TRUE) 
    } else {
      g2 <- g1 +
        geom_point(size = 3, aes(shape = DETECT_FLAG),
                   show.legend = TRUE)
    }
    
    # show vertical annotation lines
    if(nrow(annotation_df) > 0){
      
      # named vector
      ann_linetype <- annotation_df |> 
        pull(LINETYPE, LABEL)
      
      g2a <- g2 +
        geom_vline(data = annotation_df,
                   aes(xintercept = DATE, linetype = LABEL), # linetype as dummy here
                   colour = "#808080", linewidth = 0.5) +
        scale_linetype_manual(values = ann_linetype)
      
    } else {
      g2a <- g2
    }
    
    # then the rest except for the faceting
    g <- g2a +
      scale_x_date(labels = scales::label_date_short(),
                   limits = c(min_date, max_date),
                   date_breaks = date_break_value,
                   date_minor_breaks = "1 year") +
      facet_wrap(~combined_facet, ncol = 1, scales = "free_y", drop = FALSE) +
      theme(legend.position = "bottom",
            legend.box = "vertical") +
      scale_shape_manual(values = c("N" = 1, "Y" = 19),
                         labels = c("N" = "Not detected", "Y" = "Detected"),
                         drop = FALSE) +
      scale_colour_manual(values = named_colour_vec,
                          guide = "none") +
      scale_y_continuous(limits = c(0, NA)) +
      guides(linetype = guide_legend(override.aes = list(shape = NA))) +
      labs(title = glue("{well_selected} Water Quality Parameters"),
           x = NULL,
           y = NULL,
           colour = NULL,
           shape = NULL,
           linetype = NULL) 
    
  } else {
    
    # NULL object that is handled later. not a ggplot2 blank_graph() as previously
    g <- NULL
  }
  
  # output dependent on whether ggiraph or not
  if(is_ggiraph == TRUE & !is.null(g)){
    
    girafe(ggobj = g,
           # no button - there is a manual one
           options = list(opts_toolbar(saveaspng = FALSE)))
    
  } else {
    g
  }
}


#' Chlorinated ethene graphs on the COC Geochem page
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param well_selected the wells to show
#' @param y_scale the scale on the y axis- normal or log10
#' @param min_date the start date of the graph
#' @param max_date the end date of the graph
#' @param colour_dataset the colours used for the analytes
#' @param criteria_dataset the criteria df to use for lines
#' @param annotation_df annotation_lines dataset
#' @param show_annotations show the annotation lines or not
#' @param facets whether to use facets ("faceted") or to combine the graphs ("combined")
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#' 
chlorinated_ethene_graph_ggg_v4 <- function(dataset,
                                            well_selected,
                                            y_scale = "normal",
                                            min_date,
                                            max_date,
                                            colour_dataset = graph_colours_df,
                                            criteria_dataset,
                                            annotation_df,
                                            show_annotations,
                                            facets = "faceted",
                                            is_ggiraph = FALSE){
  
  # new order for graphs
  # "PCE", "TCE", "cis-1,2-DCE", "trans-1,2-DCE", "1,1-DCE", "Vinyl Chloride"
  new_ce_upper <- c("127-18-4", "79-01-6", "156-59-2", "156-60-5", "75-35-4", "75-01-4")
  # "Ethene", "Ethane", "Acetylene"
  new_ce_lower <- c("74-85-1", "74-84-0", "74-86-2")
  
  analytes_selected <- c(new_ce_upper, new_ce_lower)
  ce_facet_order <- c("Chlorinated Ethenes", "Non-Chlorinated Organic Products")
  
  # annotations - filter out if not wanted
  if(show_annotations == "No"){
    annotation_df <- annotation_df |> 
      filter(is.na(DATE))
  }
  
  # data
  df <- dataset %>%
    filter(CAS_RN %in% analytes_selected,
           SYS_LOC_CODE == well_selected) %>%
    mutate(CAS_RN = factor(CAS_RN, levels = analytes_selected),
           ctype = if_else(CAS_RN %in% new_ce_upper, "Chlorinated Ethenes", "Non-Chlorinated Organic Products"),
           ctype = factor(ctype, levels = ce_facet_order)) 
  
  # colour vector from helper function
  named_colour_vec <- named_colour_function(colour_dataset,
                                            df,
                                            c("CAS_RN", "CHEMICAL_NAME"),
                                            "CHEMICAL_NAME")
  
  # check whether empty or not
  ncop_rows <- nrow(filter(df, ctype == ce_facet_order[2]))
  
  # date to place label if no data
  mid_date <- min_date + (max_date - min_date) / 2
  
  # combine so as to show where no text 
  if(ncop_rows == 0){
    empty_df <- tibble(REPORT_RESULT_VALUE = mean(df$REPORT_RESULT_VALUE, na.rm = TRUE),
                       SAMPLE_DATE = mid_date,
                       ctype = ce_facet_order[2],
                       DETECT_FLAG = "N",
                       empty = "No data")
  } else {
    empty_df <- tibble(REPORT_RESULT_VALUE = NA_real_,
                       SAMPLE_DATE = NA_Date_,
                       ctype = ce_facet_order[2],
                       DETECT_FLAG = "N",
                       empty = "No data")
  }
  
  # date breaks depend on number of years in dataset
  num_years <- interval(min_date, max_date) / years(1)
  if(num_years > 8){
    date_break_value <- "2 years"
  } else {
    date_break_value <- waiver()
  }
  
  # same scale
  min_val <- min(df$REPORT_RESULT_VALUE)
  max_val <- max(df$REPORT_RESULT_VALUE)
  
  # subtitle dependent on yscale
  yscale_text <- if_else(y_scale == "normal",
                         "Concentration (\u03BCg/L)", # mu in unicode for ug/L
                         "Concentration (\u03BCg/L) log10 scale")

  if(nrow(df) > 1){
    
    # initial bit
    
    g1 <- ggplot(df, aes(SAMPLE_DATE, REPORT_RESULT_VALUE, colour = CHEMICAL_NAME)) +
      theme_bw() +
      geom_line() + 
      scale_x_date(labels = scales::label_date_short(),
                   limits = c(min_date, max_date),
                   date_breaks = date_break_value,
                   date_minor_breaks = "1 year") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    # intermediate graph depending on if ggiraph
    if(is_ggiraph == TRUE){
      g2 <- g1 +
        geom_point_interactive(size = 2, aes(shape = DETECT_FLAG,
                                             tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                 CHEMICAL_NAME: {CHEMICAL_NAME}
                                 SYS_LOC_CODE: {SYS_LOC_CODE}
                                 REPORT_RESULT_VALUE: {REPORT_RESULT_VALUE}
                                 REPORT_RESULT_UNIT: {REPORT_RESULT_UNIT}
                                 QUALIFIER: {INTERPRETED_QUALIFIERS}
                                 DETECT_FLAG: {DETECT_FLAG}")),
                               show.legend = TRUE)
    } else {
      g2 <- g1 +
        geom_point(size = 3,
                   aes(shape = DETECT_FLAG),
                   show.legend = TRUE)
    }
    
    # if is scale log10
    if(y_scale == "log10"){
      
      g3 <- g2 +
        scale_y_log10(limits = c(closest_lower(cust_y_poss_vals, min_val),
                                 closest_higher(cust_y_poss_vals, max_val)),
                      breaks = cust_y_poss_vals) 
    } else {
      # is normal
      # don't do the theil-sen line as it stuffs the order
      g3 <- g2 +
        scale_y_continuous(limits = ~c(min(0, min(.x)), ceiling(max(.x)))) 
      
    }
    
    # show vertical annotation lines
    if(nrow(annotation_df) > 0){
      
      # named vector
      ann_linetype <- annotation_df |> 
        pull(LINETYPE, LABEL)
      
      g4 <- g3 +
        geom_vline(data = annotation_df,
                   aes(xintercept = DATE, linetype = LABEL), # linetype as dummy here
                   colour = "#808080", linewidth = 0.5) +
        scale_linetype_manual(values = ann_linetype)
      
    } else {
      g4 <- g3
    }
    
    
    # most of the rest
    g5 <- g4 +
      labs(title = glue("{well_selected} Chlorinated Ethene and Daughter Product"),
           x = NULL,
           y = yscale_text, 
           colour = NULL,
           shape = NULL,
           linetype = NULL) +
      scale_colour_manual(values = named_colour_vec,
                          drop = FALSE) +
      scale_shape_manual(values = c("N" = 1, "Y" = 19),
                         labels = c("N" = "Not detected", "Y" = "Detected"),
                         drop = FALSE) +
      theme(legend.position = "bottom",
            legend.box = "vertical",
            legend.text = element_text(size = 8),
            legend.box.just = "center",
            legend.box.spacing = unit(0, "pt"), # The spacing between the plotting area and the legend box (unit)
            legend.margin=margin(0, 0, 0, 0)) + # the margin around each legend
      guides(colour = guide_legend(order = 1),
             shape = guide_legend(order = 2),
             linetype = guide_legend(order = 3,
                                     override.aes = list(shape = NA))) 
    
    if(facets == "faceted"){
      
      g <- g5 +
        facet_wrap(~ctype, nrow = 2, drop = FALSE) +
        # text if facet is empty (bottom only)
        geom_text(data = empty_df,
                  aes(SAMPLE_DATE, y = REPORT_RESULT_VALUE, label = empty), colour = "darkgrey")
    } else {
      # the unfaceted option
      g <- g5
    }
    
    
  } else {
    
    # NULL object that is handled later. not a ggplot2 blank_graph() as previously
    g <- NULL
  }
  
  # output dependent on whether ggiraph or not
  if(is_ggiraph == TRUE & !is.null(g)){
    
    girafe(ggobj = g,
           # no button - there is a manual one
           options = list(opts_toolbar(saveaspng = FALSE)))
    
  } else {
    g
  }
  
} # end function


#' molar concentration graph on the COC Geochem page
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param well_selected the wells to show
#' @param analytes_selected the analytes to show
#' @param molar_mass_table a lookup table of molar concentrations 
#' @param factor_order_vector a vector for the factor order - analyte short
#' @param min_date the start date of the graph
#' @param max_date the end date of the graph
#' @param colour_dataset the colours used for the analytes
#' @param annotation_df annotation_lines dataset
#' @param show_annotations show the annotation lines or not
#' @param molar_conc_type graph type - stacked_area or stacked_bar
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#'
molar_conc_graph_ggg_v2 <- function(dataset, well_selected, analytes_selected, molar_mass_table, factor_order_vector,
                                    min_date, max_date, colour_dataset = graph_colours_df,
                                    annotation_df, show_annotations, molar_conc_type, is_ggiraph = FALSE){
  
  # annotations - filter out if not wanted
  if(show_annotations == "No"){
    annotation_df <- annotation_df |> 
      filter(is.na(DATE))
  }
  
  # data from helper function
  df <- get_molar_conc_df(dataset,
                          analytes_selected,
                          well_selected,
                          molar_mass_table,
                          factor_order_vector)
  
  # colour vector from helper function
  named_colour_vec <- named_colour_function(colour_dataset,
                                            df,
                                            c("CAS_RN", "CHEMICAL_NAME"),
                                            "CHEMICAL_NAME")
  
  if(nrow(df) > 1){
    
    # check that at least one analyte has more than one value
    sample_counts <- df %>% 
      count(CHEMICAL_NAME) %>% 
      filter(n == max(n)) %>% 
      slice(1) %>% 
      pull(n)
    
    if(sample_counts > 1){
      
      # date breaks depend on number of years in dataset
      num_years <- interval(min_date, max_date) / years(1)
      if(num_years > 8){
        date_break_value <- "2 years"
      } else {
        date_break_value <- waiver()
      }
      
      # initial graph
      p0 <- ggplot(df, aes(SAMPLE_DATE, value, fill = CHEMICAL_NAME)) +
        theme_bw() + 
        scale_x_date(labels = scales::label_date_short(),
                     limits = c(min_date, max_date),
                     date_breaks = date_break_value,
                     date_minor_breaks = "1 year") +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 8),
              legend.box.spacing = unit(0, "pt"), # The spacing between the plotting area and the legend box (unit)
              legend.margin=margin(0, 0, 0, 0)) + # the margin around each legend
        scale_fill_manual(values = named_colour_vec,
                          drop = FALSE) +
        guides(fill = guide_legend(nrow = 2,
                                  byrow = TRUE)) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
      # concentrations
      # intermediate graph depending on if ggiraph
      if(is_ggiraph == TRUE){
        if(molar_conc_type == "stacked_bar"){
          p1a <- p0 +
            geom_bar_interactive(position = "stack",
                                 stat = "identity",
                                 aes(tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                                  CHEMICAL_NAME: {CHEMICAL_NAME}
                                                  CONCENTRATION: {signif(value, 3)} (\u03BCmol/L)")))
        } else {
          # stacked area
          p1a <- p0 +
            geom_area_interactive(aes(tooltip = CHEMICAL_NAME))
        }
      } else {
        # not interactive
        if(molar_conc_type == "stacked_bar"){
          p1a <- p0 +
            geom_bar(position = "stack",
                     stat = "identity")
        } else {
          # stacked area
          p1a <- p0 +
            geom_area()
        }
      }
      
      p1b <- p1a +
        labs(title = NULL,
             subtitle = "Molar Concentrations (detected values only)",
             x = NULL,
             y = "Concentration (\u03BCmol/L)",
             fill = NULL,
             linetype = NULL)
      
      # fractions
      # intermediate graph depending on if ggiraph
      if(is_ggiraph == TRUE){
        if(molar_conc_type == "stacked_bar"){
          p2a <- p0 +
            geom_bar_interactive(position = "fill",
                                 stat = "identity",
                                 aes(tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                                  CHEMICAL_NAME: {CHEMICAL_NAME}
                                                  PROPORTION: {signif(prop_value, 3)}")))
        } else {
          # stacked area
          p2a <- p0 +
            geom_area_interactive(position = "fill",
                                  aes(tooltip = CHEMICAL_NAME))
        }
      } else {
        # not interactive
        if(molar_conc_type == "stacked_bar"){
          p2a <- p0 +
            geom_bar(position = "fill",
                     stat = "identity")
        } else {
          # stacked area
          p2a <- p0 +
            geom_area(position = "fill")
        }
        
      }
      
      p2b <- p2a +
        labs(title = NULL,
             subtitle = "Molar fractions (detected values only)",
             x = NULL,
             y = "Proportion",
             fill = NULL,
             linetype = NULL)
      
      # show vertical annotation lines - final graphs
      if(nrow(annotation_df) > 0){
        
        # named vector
        ann_linetype <- annotation_df |> 
          pull(LINETYPE, LABEL)
        
        p1 <- p1b +
          geom_vline(data = annotation_df,
                     aes(xintercept = DATE, linetype = LABEL), # linetype as dummy here
                     colour = "#808080", linewidth = 0.5) +
          scale_linetype_manual(values = ann_linetype)
        
        p2 <- p2b +
          geom_vline(data = annotation_df,
                     aes(xintercept = DATE, linetype = LABEL), # linetype as dummy here
                     colour = "#808080", linewidth = 0.5) +
          scale_linetype_manual(values = ann_linetype)
        
      } else {
        p1 <- p1b
        p2 <- p2b
      }
      
      # patchwork
      g <- (p1 / p2)  +
        plot_annotation(title = glue("{well_selected} Chlorinated Ethene and Daughter Product")) +
        guide_area() +
        plot_layout(guides = "collect",
                    ncol = 1,
                    heights = c(4, 4, 2))
      
    } else {
      # NULL object that is handled later. not a ggplot2 blank_graph() as previously
      g <- NULL
    }
    
  } else {
    # NULL object that is handled later. not a ggplot2 blank_graph() as previously
    g <- NULL
  }
  
  # output dependent on whether ggiraph or not
  if(is_ggiraph == TRUE & !is.null(g)){
    
    girafe(ggobj = g,
           # no button - there is a manual one
           options = list(opts_toolbar(saveaspng = FALSE)))
    
  } else {
    g
  }
  
} # end function



#' combine the graphs on the COC Geochem page
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param well_selected the wells to show
#' @param scale_type passed in to 'y_scale' arg in some of the functions
#' @param graph_groupings_list list of graph groups
#' @param graph_groupings graph_groupings object for toc_redox_graph_ggg_v3()
#' @param criteria_dataset criteria df for lines on graph in chlorinated_ethene_graph_ggg_v3()
#' @param constant_min_date min date to start graphs at
#' @param annotation_df annotation_lines dataset
#' @param show_annotations show the annotation lines or not
#' @param molar_conc_type the type of graph for the molar concentration graphs
#' @param chem_lookup table to look up for toc graphs
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#' @param cedp_type passed in to the 'facets' arg in chlorinated_ethene_graph_ggg_v4()
#' @param prog progress if in shiny app
#' 
well_cell_graphs_ggg_v2 <- function(dataset, well_selected, scale_type = "normal", graph_groupings_list,
                                    graph_groupings, criteria_dataset, constant_min_date, annotation_df, show_annotations, 
                                    molar_conc_type, chem_lookup, is_ggiraph = FALSE, cedp_type = "faceted", prog = NULL){
  
  # same for all graphs - so ignores well min and max dates
  graph_min_date <- constant_min_date 
  graph_max_date <- max(dataset$SAMPLE_DATE)
  
  wq <- water_quality_graph_ggg_v4(dataset,
                                   well_selected,
                                   analytes_selected = graph_groupings_list$`Water Quality Parameters`,
                                   min_date = graph_min_date,
                                   max_date = graph_max_date,
                                   colour_dataset = graph_colours_df,
                                   annotation_df = annotation_df,
                                   show_annotations = show_annotations,
                                   is_ggiraph)
  
  tocr <- toc_redox_graph_ggg_v4(dataset,
                                 well_selected,
                                 analytes_selected = graph_groupings_list$`TOC and Redox Parameters`,
                                 min_date = graph_min_date,
                                 max_date = graph_max_date,
                                 scale_type, 
                                 graph_groupings, # added
                                 colour_dataset = graph_colours_df,
                                 annotation_df = annotation_df, 
                                 show_annotations = show_annotations,
                                 chem_lookup = chem_lookup,
                                 is_ggiraph)
  
  ce_norm <- chlorinated_ethene_graph_ggg_v4(dataset,
                                             well_selected,
                                             "normal",
                                             min_date = graph_min_date,
                                             max_date = graph_max_date,
                                             colour_dataset = graph_colours_df,
                                             criteria_dataset,
                                             annotation_df = annotation_df, 
                                             show_annotations = show_annotations,
                                             facets = cedp_type,
                                             is_ggiraph)
  
  ce_log10 <- chlorinated_ethene_graph_ggg_v4(dataset,
                                              well_selected,
                                              "log10",
                                              min_date = graph_min_date,
                                              max_date = graph_max_date,
                                              colour_dataset = graph_colours_df,
                                              criteria_dataset,
                                              annotation_df = annotation_df, 
                                              show_annotations = show_annotations,
                                              facets = cedp_type,
                                              is_ggiraph)
  
  ce_molar <- molar_conc_graph_ggg_v2(dataset,
                                      well_selected,
                                      analytes_selected = graph_groupings_list$`Chlorinated Ethene and Daughter Product`,
                                      factor_order_vector = chlor_eth_factor_order,
                                      molar_mass_table = molecular_weights,
                                      min_date = graph_min_date,
                                      max_date = graph_max_date,
                                      colour_dataset = graph_colours_df,
                                      annotation_df = annotation_df, 
                                      show_annotations = show_annotations,
                                      molar_conc_type = molar_conc_type,
                                      is_ggiraph)
  
  # progress increment if on shiny
  if(isRunning()){
    incProgress(prog) 
  }
  
  return(list("wq" = wq,
              "tocr" = tocr,
              "ce_norm" = ce_norm,
              "ce_log10" = ce_log10,
              "ce_molar" = ce_molar))
}

# inorgaincs page --------------------------------------------------------

#' The graphs on the inorganics page
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param analytes_selected the analytes to show
#' @param wells_selected the wells to show
#' @param frac_filter 'T' or 'D' on the FRACTION column
#' @param crit_data the criteria dataset
#' @param y_scale the scale on the y axis - normal or log10
#' @param annotation_df annotation_lines dataset
#' @param show_annotations show the annotation lines or not
#' @param col_pal the selected colour palette
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#' @param prog progress if running in Shiny
#' @param ncols the number of columns in the facet
#' 
inorg_graphs_ggg_v2 <- function(dataset, analyte_selected, wells_selected, frac_filter, crit_data,
                                y_scale, annotation_df, show_annotations, col_pal, 
                                is_ggiraph = FALSE, prog = NULL, ncols = 3){
  
  df <- dataset %>%
    filter(CHEMICAL_NAME == analyte_selected,
           SYS_LOC_CODE %in% wells_selected,
           FRACTION == frac_filter) %>%
    # criteria
    left_join(crit_data %>% 
                select(CAS_RN, action_level, CRITERIA_UNIT, CRITERIA_TEXT),
              by = c("CAS_RN")) %>% 
    criteria_unit_conversion() %>% 
    mutate(SYS_LOC_CODE = factor(SYS_LOC_CODE,
                                 levels = str_sort(wells_selected, numeric = TRUE))) %>% 
    arrange(SYS_LOC_CODE, SAMPLE_DATE) %>% 
    convert_mu_symbol(REPORT_RESULT_UNIT)
  
  # annotations - filter out if not wanted
  if(show_annotations == "No"){
    annotation_df <- annotation_df |> 
      filter(is.na(DATE))
  }
  
  # run if not empty
  if(nrow(df) > 0){
    # for y label
    rep_unit <- unique(df$REPORT_RESULT_UNIT)
    y_label <- glue("Concentration ({rep_unit})")
    
    # so on the same date scale
    min_date <- min(df$SAMPLE_DATE)
    max_date <- max(df$SAMPLE_DATE)
    mid_date <- min_date + (max_date - min_date) / 2
    
    # date breaks depend on number of years in dataset
    num_years <- interval(min_date, max_date) / years(1)
    if(num_years > 8){
      date_break_value <- "2 years"
    } else {
      date_break_value <- waiver()
    }
    
    # annotation text from function
    ann_text <- get_ann_text_df(df, mid_date)
    
    # colour palette
    n_colours <- length(unique(df$SYS_LOC_CODE))
    pal <- colour_palette_selection(col_pal, n_colours)
    
    # graph
    initial_graph <- ggplot(df, aes(SAMPLE_DATE, REPORT_RESULT_VALUE, colour = SYS_LOC_CODE)) +
      theme_bw() +
      geom_line()
    
    # intermediate graph depending on if ggiraph
    if(is_ggiraph == TRUE){
      intermediate_graph <- initial_graph +
        geom_point_interactive(size = 2, aes(shape = DETECT_FLAG,
                                             tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                 CHEMICAL_NAME: {CHEMICAL_NAME}
                                 SYS_LOC_CODE: {SYS_LOC_CODE}
                                 REPORT_RESULT_VALUE: {REPORT_RESULT_VALUE}
                                 REPORT_RESULT_UNIT: {REPORT_RESULT_UNIT}
                                 QUALIFIER: {INTERPRETED_QUALIFIERS}
                                 DETECT_FLAG: {DETECT_FLAG}")),
                               show.legend = TRUE)
    } else {
      intermediate_graph <- initial_graph +
        geom_point(size = 3,
                   aes(shape = DETECT_FLAG),
                   show.legend = TRUE)
    }
    
    g1 <- intermediate_graph +
      scale_x_date(labels = scales::label_date_short(),
                   date_breaks = date_break_value,
                   date_minor_breaks = "1 year") +
      scale_shape_manual(values = c("N" = 1, "Y" = 19),
                         labels = c("N" = "Not detected", "Y" = "Detected"),
                         drop = FALSE) +
      scale_colour_manual(values = pal,
                          guide = "none") +
      labs(title = glue("{analyte_selected}"),
           subtitle = glue("Fraction: ({frac_filter})"),
           x = "Sample Date",
           y = y_label,
           linetype = NULL,
           colour = NULL,
           shape = NULL) + 
      facet_wrap(~SYS_LOC_CODE, ncol = ncols, drop = FALSE) +
      theme(legend.position = "bottom",
            legend.box = "vertical",
            legend.key.height = unit(0.1, "cm"),
            legend.spacing = unit(0.1, 'cm')) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      guides(shape = guide_legend(order = 1),
             linetype = guide_legend(order = 2,
                                     override.aes = list(shape = NA)))
    
    
    # if is scale log10
    if(y_scale == "log10"){
      
      # same scale
      min_val <- min(df$REPORT_RESULT_VALUE)
      max_val <- max(df$REPORT_RESULT_VALUE)
      
      g2 <- g1 +
        scale_y_log10(limits = c(closest_lower(cust_y_poss_vals, min_val),
                                 closest_higher(cust_y_poss_vals, max_val)),
                      breaks = cust_y_poss_vals) 
    } else {
      # is normal
      # don't do the theil-sen line as it stuffs the order
      g2 <- g1 +
        scale_y_continuous(limits = ~c(min(0, min(.x)), ceiling(max(.x)))) 
    }
    
    # show vertical annotation lines
    if(nrow(annotation_df) > 0){
      
      # named vector
      ann_linetype <- annotation_df |> 
        pull(LINETYPE, LABEL)
      
      g3 <- g2 +
        geom_vline(data = annotation_df,
                   aes(xintercept = DATE, linetype = LABEL), # linetype as dummy here
                   colour = "#808080", linewidth = 0.5, key_glyph = "vpath") 
      
    } else {
      g3 <- g2
    }
    
    # the horizontal line
    if(nrow(ann_text) > 0){
      # if there is a value for the criteria
      g <- g3 +
        geom_hline(data = ann_text, aes(yintercept = REPORT_RESULT_VALUE, linetype = label_text),
                   colour = "darkgrey",
                   key_glyph = "path")
    } else {
      # no value for the criteria
      g <- g3
    }
    
  } else {
    g <- blank_graph()
  }
  
  # if writing, update progress
  if(isRunning()){
    incProgress(prog) 
  }
  
  # output dependent on whether ggiraph or not
  if(is_ggiraph == TRUE){
    
    # return ggirafe
    g <- girafe(ggobj = g)

  } # else just what is in there
  
  return(g)
  
} # end function

# single chem selected wells page ----------------------------------------------------------

#' for one chemical with selected wells
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param chem_selected the analyte to show
#' @param wells_selected the wells to show
#' @param annotation_df annotation_lines dataset
#' @param crit_data criteria for lines
#' @param col_pal the selected colour palette
#' @param show_annotations show the annotation lines or not
#' @param y_scale the scale on the y axis- normal or log10
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#' @param prog progress if in shiny app
#' 
single_chem_selected_wells_ggg <- function(dataset, chem_selected, wells_selected, crit_data, annotation_df,
                                           col_pal, show_annotations, y_scale = "normal", is_ggiraph = FALSE, prog = NULL){
  
  print(chem_selected)
  
  # filter and check criteria
  df <- dataset %>% 
    filter(CHEMICAL_NAME == chem_selected,
           SYS_LOC_CODE %in% wells_selected) %>% 
    # criteria
    left_join(crit_data %>% 
                select(CAS_RN, CRITERIA_TEXT, action_level, CRITERIA_UNIT, CRITERIA_TEXT),
              by = c("CAS_RN")) %>% 
    criteria_unit_conversion() %>% 
    convert_mu_symbol(REPORT_RESULT_UNIT)
  
  # annotations - filter out if not wanted
  if(show_annotations == "No"){
    annotation_df <- annotation_df |> 
      filter(is.na(DATE))
    
    # so on the same date scale
    min_date <- min(df$SAMPLE_DATE)
    max_date <- max(df$SAMPLE_DATE)
    mid_date <- min_date + (max_date - min_date) / 2
    
  } else {
    
    # showing the annotations
    
    # so on the same date scale - the make sure to include the annotation line
    min_date <- min(c(min(df$SAMPLE_DATE), min(annotation_df$DATE)))
    max_date <- max(c(max(df$SAMPLE_DATE), max(annotation_df$DATE)))
    mid_date <- min_date + (max_date - min_date) / 2
    
  }
  
  # colour palette
  n_colours <- length(unique(df$SYS_LOC_CODE))
  pal <- colour_palette_selection(col_pal, n_colours)
  
  # label names
  ylab <- if_else(y_scale == "normal",
                  glue("Concentration ({unique(df$REPORT_RESULT_UNIT)})"),
                  glue("Concentration ({unique(df$REPORT_RESULT_UNIT)}) log10 scale"))
  
  # date breaks depend on number of years in dataset
  num_years <- interval(min_date, max_date) / years(1)
  if(num_years > 8){
    date_break_value <- "2 years"
  } else {
    date_break_value <- waiver()
  }
  
  # same scale
  min_val <- min(df$REPORT_RESULT_VALUE)
  max_val <- max(df$REPORT_RESULT_VALUE)
  
  # annotation text from function
  ann_text <- get_ann_text_df(df, mid_date)
  
  # the graph
  g0 <- ggplot(df, aes(SAMPLE_DATE, REPORT_RESULT_VALUE, colour = SYS_LOC_CODE)) +
    theme_bw() +
    geom_line()
  
  # intermediate graph depending on if ggiraph
  if(is_ggiraph == TRUE){
    g1 <- g0 +
      geom_point_interactive(size = 2,
                             aes(shape = DETECT_FLAG,
                                 tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                 CHEMICAL_NAME: {CHEMICAL_NAME}
                                 SYS_LOC_CODE: {SYS_LOC_CODE}
                                 REPORT_RESULT_VALUE: {REPORT_RESULT_VALUE}
                                 REPORT_RESULT_UNIT: {REPORT_RESULT_UNIT}
                                 QUALIFIER: {INTERPRETED_QUALIFIERS}
                                 DETECT_FLAG: {DETECT_FLAG}")),
                             show.legend = TRUE)
  } else {
    g1 <- g0 +
      geom_point(size = 3, aes(shape = DETECT_FLAG), show.legend = TRUE)
  }
  
  
  # the horizontal line
  if(nrow(ann_text) > 0){
    # if there is a value for the criteria
    g1a <- g1 +
      geom_hline(data = ann_text, aes(yintercept = REPORT_RESULT_VALUE, linetype = label_text),
                 colour = "darkgrey",
                 key_glyph = "path")
  } else {
    # no value for the criteria
    g1a <- g1
  }
  
  g2 <- g1a +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(size = 8),
          legend.spacing = unit(0.5, 'cm'),
          legend.margin = margin()) +
    scale_x_date(labels = scales::label_date_short(),
                 limits = c(min_date, max_date),
                 date_breaks = date_break_value,
                 date_minor_breaks = "1 year") +
    scale_colour_manual(values = pal,
                        # wrap long labels in legend
                        labels = function(x) str_wrap(x, width = 13)) +
    scale_shape_manual(values = c("N" = 1, "Y" = 19),
                       labels = c("N" = "Not detected", "Y" = "Detected"),
                       drop = FALSE) +
    labs(title = chem_selected,
         x = NULL,
         y = ylab,
         colour = NULL,
         linetype = NULL,
         shape = NULL) +
    guides(colour = guide_legend(nrow = ceiling(length(unique(df$SYS_LOC_CODE))/4),
                                 byrow = TRUE), # four wells per row in legend
           linetype = guide_legend(override.aes = list(shape = NA))) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  # show vertical annotation lines
  if(nrow(annotation_df) > 0){
    
    # named vector
    ann_linetype <- annotation_df |> 
      pull(LINETYPE, LABEL)
    
    g3 <- g2 +
      geom_vline(data = annotation_df,
                 aes(xintercept = DATE, linetype = LABEL), # linetype as dummy here
                 colour = "#808080", linewidth = 0.5, key_glyph = "vpath") 
    
  } else {
    g3 <- g2
  }
  
  
  if(y_scale == "normal"){
    g <- g3 + 
      scale_y_continuous(limits = ~c(min(0, min(.x)), ceiling(max(.x)))) 
  } else {
    # log10
    g <- g3 + 
      scale_y_log10(limits = c(closest_lower(cust_y_poss_vals, min_val),
                               closest_higher(cust_y_poss_vals, max_val)),
                    breaks = cust_y_poss_vals)
  }
  
  # if writing, update progress
  if(isRunning()){
    incProgress(prog) 
  }
  
  # output dependent on whether ggiraph or not
  if(is_ggiraph == TRUE){
    
    # return ggirafe
    g <- girafe(ggobj = g,
                # no button - there is a manual one
                options = list(opts_toolbar(saveaspng = FALSE)))
    
  } # else just what is in there
  
  return(g)
  
} # end function

#' all wells for coc geochemistry
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param annotation_df annotation_lines dataset
#' @param max_num the maximum number of facets to show per graph
#' @param legend_pos where to show the legend
#' @param show_annotations show the annotation lines or not
#' @param y_scale the scale on the y axis- normal or log10
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#' 
single_chem_many_facets_ggg <- function(dataset, annotation_df, max_num, legend_pos, crit_data,
                                        show_annotations, y_scale = "normal", is_ggiraph = FALSE){
  
  # label names
  ylab <- if_else(y_scale == "normal",
             glue("{unique(dataset$REPORT_RESULT_UNIT)}"),
             glue("{unique(dataset$REPORT_RESULT_UNIT)} (log10 scale)"))
  
  # annotations - filter out if not wanted
  if(show_annotations == "No"){
    annotation_df <- annotation_df |> 
      filter(is.na(DATE))
    
    # so on the same date scale
    min_date <- min(dataset$SAMPLE_DATE)
    max_date <- max(dataset$SAMPLE_DATE)
    mid_date <- min_date + (max_date - min_date) / 2
    
  } else {
    
    # showing the annotations
    
    # so on the same date scale - the make sure to include the annotation line
    min_date <- min(c(min(dataset$SAMPLE_DATE), min(annotation_df$DATE)))
    max_date <- max(c(max(dataset$SAMPLE_DATE), max(annotation_df$DATE)))
    mid_date <- min_date + (max_date - min_date) / 2
    
  }
  
  # there is only one chemical here
  chem_name <- unique(dataset$CHEMICAL_NAME)
  
  # check the number of wells in the group with data
  num_locations <- length(unique(dataset$SYS_LOC_CODE))
  
  sorted_locations <- dataset %>% 
    distinct(SYS_LOC_CODE) %>% 
    arrange(SYS_LOC_CODE) %>% 
    pull(SYS_LOC_CODE)
  
  # split up if more than max_num 
  if(num_locations > max_num) {
    num_charts <- ceiling(num_locations / max_num)
    loc_vectors <- split(sorted_locations, cut(seq_along(sorted_locations), num_charts, labels=FALSE))
    
  } else {
    loc_vectors <- list(sorted_locations)
  }
  
  # date breaks depend on number of years in dataset
  num_years <- interval(min_date, max_date) / years(1)
  if(num_years > 8){
    date_break_value <- "2 years"
  } else {
    date_break_value <- waiver()
  }
  
  # same scale
  min_val <- min(dataset$REPORT_RESULT_VALUE)
  max_val <- max(dataset$REPORT_RESULT_VALUE)
  
  # check criteria
  df <- dataset %>% 
    # criteria
    left_join(crit_data %>% 
                select(CAS_RN, action_level_code, action_level, CRITERIA_UNIT, CRITERIA_TEXT),
              by = c("CAS_RN")) %>% 
    criteria_unit_conversion() 
  
  # annotation text from function
  ann_text <- get_ann_text_df(df, mid_date)
  
  map(loc_vectors, 
      function(.x){
        
        g0 <- ggplot(dataset %>% 
                       filter(SYS_LOC_CODE %in% .x), aes(SAMPLE_DATE, REPORT_RESULT_VALUE)) +
          theme_bw() +
          geom_line()
        
        
        # intermediate graph depending on if ggiraph
        if(is_ggiraph == TRUE){
          g1 <- g0 +
            geom_point_interactive(size = 2, aes(shape = DETECT_FLAG,
                                                 tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                 CHEMICAL_NAME: {CHEMICAL_NAME}
                                 SYS_LOC_CODE: {SYS_LOC_CODE}
                                 REPORT_RESULT_VALUE: {REPORT_RESULT_VALUE}
                                 REPORT_RESULT_UNIT: {REPORT_RESULT_UNIT}
                                 QUALIFIER: {INTERPRETED_QUALIFIERS}
                                 DETECT_FLAG: {DETECT_FLAG}")),
                                   show.legend = TRUE)
        } else {
          g1 <- g0 +
            geom_point(size = 3, aes(shape = DETECT_FLAG),
                       show.legend = TRUE)
        }
        
        # the horizontal line
        if(nrow(ann_text) > 0){
          # if there is a value for the criteria
          g1a <- g1 +
            geom_hline(data = ann_text, aes(yintercept = REPORT_RESULT_VALUE, linetype = label_text),
                       # linetype = "dashed",
                       colour = "darkgrey",
                       key_glyph = "path")
        } else {
          # no value for the criteria
          g1a <- g1
        }
        
        g2 <- g1a +
          theme(legend.position = legend_pos,
                legend.box = "vertical") +
          scale_x_date(labels = scales::label_date_short(),
                       limits = c(min_date, max_date),
                       date_breaks = date_break_value,
                       date_minor_breaks = "1 year") +
          scale_shape_manual(values = c("N" = 1, "Y" = 19),
                             labels = c("N" = "Not detected", "Y" = "Detected"),
                             drop = FALSE) +
          labs(title = chem_name,
               x = NULL,
               y = ylab,
               colour = NULL,
               shape = NULL,
               linetype = NULL) +
          facet_wrap(~SYS_LOC_CODE, ncol = 3) + 
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
        
        if(y_scale == "normal"){
          g3 <- g2 + 
            scale_y_continuous(limits = ~c(min(0, min(.x)), ceiling(max(.x)))) 
        } else {
          # log10
          g3 <- g2 + 
            scale_y_log10(limits = c(closest_lower(cust_y_poss_vals, min_val),
                                     closest_higher(cust_y_poss_vals, max_val)),
                          breaks = cust_y_poss_vals)
        }
        
        # show vertical annotation lines
        if(nrow(annotation_df) > 0){
          
          # named vector
          ann_linetype <- annotation_df |> 
            pull(LINETYPE, LABEL)
          
          g4 <- g3 +
            geom_vline(data = annotation_df,
                       aes(xintercept = DATE, linetype = LABEL), # linetype as dummy here
                       colour = "#808080", linewidth = 0.5) +
            guides(linetype = guide_legend(order = 1,
                                           override.aes = list(shape = NA)),
                   shape = guide_legend(order = 2))
          
        } else {
          g4 <- g3
        }
        
        return(g4)
        
      }) # end of map function
            
      # don't return the anything here
          
} # end function

# single dataset page ----------------------------------------------------------------

#' Graph used in the single dataset view page. Initial graph, as trend line gets added later.
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param selected_scale "log" or "normal"
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#' 
single_dataset_graph_initial_ggg <- function(dataset, selected_scale, is_ggiraph = FALSE){
  
  if(nrow(dataset) > 1){
    
    chem_unit <- unique(dataset$REPORT_RESULT_UNIT)
    
    # so on the same date scale
    min_date <- min(dataset$SAMPLE_DATE)
    max_date <- max(dataset$SAMPLE_DATE)
    mid_date <- min_date + (max_date - min_date) / 2
    
    # date breaks depend on number of years in dataset
    num_years <- interval(min_date, max_date) / years(1)
    if(num_years > 8){
      date_break_value <- "2 years"
    } else {
      date_break_value <- waiver()
    }
    
    # for title
    title_text <- glue("{unique(dataset$SYS_LOC_CODE)}: {unique(dataset$CHEMICAL_NAME)}")
    
    # graphs
    p0 <- ggplot(dataset, aes(SAMPLE_DATE, REPORT_RESULT_VALUE)) +
      theme_bw() +
      scale_x_date(labels = scales::label_date_short(),
                   limits = c(min_date, max_date),
                   date_breaks = date_break_value,
                   date_minor_breaks = "1 year") +
      scale_shape_manual(values = c("N" = 1, "Y" = 19),
                         labels = c("N" = "Not detected", "Y" = "Detected"),
                         drop = FALSE) +
      scale_linetype_manual(values = c("Thiel-Sen regression line" = "solid")) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
      guides(shape = guide_legend(order = 1),
             linetype = guide_legend(order = 2,
                                     override.aes = list(shape = NA)))
    
    # if ggiraph object
    if(is_ggiraph == TRUE){
      
      p1 <- p0 + 
        geom_point_interactive(size = 2,
                               aes(shape = DETECT_FLAG,
                                   tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                 CHEMICAL_NAME: {CHEMICAL_NAME}
                                 SYS_LOC_CODE: {SYS_LOC_CODE}
                                 REPORT_RESULT_VALUE: {REPORT_RESULT_VALUE}
                                 REPORT_RESULT_UNIT: {REPORT_RESULT_UNIT}
                                 QUALIFIER: {INTERPRETED_QUALIFIERS}
                                 DETECT_FLAG: {DETECT_FLAG}")),
                               show.legend = TRUE)
    } else {
      
      p1 <- p0 +
        geom_point(size = 3,
                   aes(shape = DETECT_FLAG),
                   show.legend = TRUE) 
      
    }
    
    # if selected scale is log scale
    if(selected_scale == "log"){
    
      min_val <- min(dataset$REPORT_RESULT_VALUE)
      max_val <- max(dataset$REPORT_RESULT_VALUE)
      
      p2 <- p1 +
        scale_y_log10(limits = c(closest_lower(cust_y_poss_vals, min_val),
                                 closest_higher(cust_y_poss_vals, max_val)),
                      breaks = cust_y_poss_vals) +
        labs(title = title_text,
             x = NULL,
             y = glue("{chem_unit} (log10 scale)"),
             shape = NULL,
             linetype = NULL) +
        theme(legend.position = "bottom") 
      
    } else {
      # normal
      p2 <- p1 +
        scale_y_continuous(labels = scales::number_format(),
                           limits = ~c(min(0, min(.x)), ceiling(max(.x)))) +
        labs(title = title_text,
             x = NULL,
             y = chem_unit,
             shape = NULL,
             linetype = NULL) +
        theme(legend.position = "bottom") 
    }
    
  } else {
    
    p2 <- blank_graph()
  }
  return(p2)
  # note: trend lines get added in the module
  
} # end function

#' jittered boxplot for the single dataset graph
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#' 
single_dataset_jittered_boxplot <- function(dataset, is_ggiraph = FALSE){
  
  # title and labels
  
  # initial graph
  g1 <- ggplot(dataset, aes(x = "", y = REPORT_RESULT_VALUE)) +
    theme_bw() +
    # remember that the labs get flipped
    labs(x = NULL,
         y = unique(dataset$REPORT_RESULT_UNIT),
         title = glue("{unique(dataset$SYS_LOC_CODE)}: {unique(dataset$CHEMICAL_NAME)}"),
         caption = "Jittered data points shown in blue",
         shape = NULL) +
    coord_flip() + 
    scale_shape_manual(values = c("N" = 1, "Y" = 19),
                       labels = c("N" = "Not detected", "Y" = "Detected"),
                       drop = FALSE) +
    theme(legend.position = "bottom")
  
  
  # output dependent on whether ggiraph or not
  if(is_ggiraph == TRUE){
    
    g <- g1 + 
      geom_boxplot_interactive() +
      geom_jitter_interactive(aes(shape = DETECT_FLAG,
                                  tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                 CHEMICAL_NAME: {CHEMICAL_NAME}
                                 SYS_LOC_CODE: {SYS_LOC_CODE}
                                 REPORT_RESULT_VALUE: {REPORT_RESULT_VALUE}
                                 REPORT_RESULT_UNIT: {REPORT_RESULT_UNIT}
                                 QUALIFIER: {INTERPRETED_QUALIFIERS}
                                 DETECT_FLAG: {DETECT_FLAG}")),
                              alpha = 0.5,
                              width = 0.2,
                              colour = "blue",
                              show.legend = TRUE) 
    
  } else {
    
    g <- g1 +
      geom_boxplot() +
      geom_jitter(aes(shape = DETECT_FLAG),
                  alpha = 0.5,
                  width = 0.2,
                  colour = "blue")
  }
  
  # output here
  return(g)
} # end function


# total voc page ------------------------------------------------------------------------------------

#' Total VOC mass graph
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param wells_selected a vector of wells to show
#' @param analytes_selected the analytes to show
#' @param use_rep_value whether to use a representative value - 'Yes' or 'No'
#' @param rep_value  representative value to use
#' @param y_scale the scale on the y axis- normal or log10
#' @param y_scale_type whether to autofit or use the user defined range
#' @param y_scale_range vector of two values for user defined range
#' @param min_date the start date of the graph
#' @param max_date the end date of the graph
#' @param col_pal the selected colour palette
#' @param criteria_dataset the criteria df to use for lines
#' @param annotation_df annotation_lines dataset
#' @param show_annotations show the annotation lines or not
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#' 
total_voc_mass_graph_ggg <- function(dataset,
                                     wells_selected,
                                     analytes_selected,
                                     use_rep_value,
                                     rep_value,
                                     y_scale = "log10",
                                     y_scale_type,
                                     y_scale_range = c(NA_real_, NA_real_),
                                     min_date,
                                     max_date,
                                     col_pal,
                                     criteria_dataset,
                                     annotation_df,
                                     show_annotations,
                                     is_ggiraph = FALSE){
  
  # data from helper function
  df <- get_total_voc_df(dataset,
                         analytes_selected,
                         wells_selected,
                         use_rep_value,
                         rep_value)$final
  
  # annotations - filter out if not wanted
  if(show_annotations == "No"){
    annotation_df <- annotation_df |> 
      filter(is.na(DATE))
  }

  # colour palette
  n_colours <- length(unique(df$SYS_LOC_CODE))
  pal <- colour_palette_selection(col_pal, n_colours)
  
  # date to place label if no data
  mid_date <- min_date + (max_date - min_date) / 2
  
  # date breaks depend on number of years in dataset
  num_years <- interval(min_date, max_date) / years(1)
  if(num_years > 8){
    date_break_value <- "2 years"
  } else {
    date_break_value <- waiver()
  }
  
  if(nrow(df) > 1){
    
    # initial bit
    g1 <- ggplot(df, aes(SAMPLE_DATE, REPORT_RESULT_VALUE, colour = SYS_LOC_CODE)) +
      theme_bw() +
      geom_line() + 
      scale_x_date(labels = scales::label_date_short(),
                   limits = c(min_date, max_date),
                   date_breaks = date_break_value,
                   date_minor_breaks = "1 year") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
    
    # if showing rep value
    if(use_rep_value == "Yes"){
      
      # intermediate graph depending on if ggiraph
      if(is_ggiraph == TRUE){
        g2 <- g1 +
          geom_point_interactive(size = 2, 
                                 aes(shape = REP_VALUE_FLAG,
                                     tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                             SYS_LOC_CODE: {SYS_LOC_CODE}
                                                            TOTAL_VOC_CONCENTRATION: {REPORT_RESULT_VALUE}")),
                                 show.legend = TRUE) +
          scale_shape_manual(values = c("Y" = 1, "N" = 19),
                             labels = c("N" = "Detected values", "Y" = "All VOCs are nondetect"),
                             drop = FALSE)
      } else {
        g2 <- g1 +
          geom_point(size = 2,
                     aes(shape = REP_VALUE_FLAG),
                     show.legend = TRUE) +
          scale_shape_manual(values = c("Y" = 1, "N" = 19),
                             labels = c("N" = "Detected values", "Y" = "All VOCs are nondetect"),
                             drop = FALSE)
      }
    } else {
      # not using rep value
      
      # intermediate graph depending on if ggiraph
      if(is_ggiraph == TRUE){
        g2 <- g1 +
          geom_point_interactive(size = 2, 
                                 aes(tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                             SYS_LOC_CODE: {SYS_LOC_CODE}
                                                            TOTAL_VOC_CONCENTRATION: {REPORT_RESULT_VALUE}"))) +
          labs(caption = "Only detected VOCs are shown on the graph")
      } else {
        g2 <- g1 +
          geom_point(size = 2) +
          labs(caption = "Only detected VOCs are shown on the graph")
      }
    }
    
    # for log 10 yscale value if user defined
    upper_value_defined <- if_else(y_scale_range[2] >= 10,
                                   (y_scale_range[2] - 1),
                                   y_scale_range[2])
    
    # if y_scale_type is auto, the usual range
    if(y_scale_type == "autofit"){
      
      min_val <- min(df$REPORT_RESULT_VALUE)
      max_val <- max(df$REPORT_RESULT_VALUE)
      
      # if is scale log10
      if(y_scale == "log10"){
        
        min_val <- min(df$REPORT_RESULT_VALUE)
        max_val <- max(max(df$REPORT_RESULT_VALUE))

        g3 <- g2 +
          scale_y_log10(limits = c(closest_lower(cust_y_poss_vals, min_val),
                                   closest_higher(cust_y_poss_vals, max_val)),
                        breaks = cust_y_poss_vals) 
      } else {
        # is normal
        g3 <- g2 +
          # scale_y_continuous(limits = ~c(min(0, min_val), ceiling(max_val))) 
          scale_y_continuous(limits = ~c(min(0, min(.x)), ceiling(max(.x)))) 
      }
      
    } else {
      # else, if user defined, use the y_scale_range
      # if is scale log10
      if(y_scale == "log10"){
        
        g3 <- g2 +
          scale_y_log10(limits = c(closest_lower(cust_y_poss_vals, y_scale_range[1]),
                                   # one lower for upper in case power of 10
                                   closest_higher(cust_y_poss_vals, upper_value_defined)),
                        breaks = cust_y_poss_vals,
                        oob = scales::oob_keep) 

      } else {
        # is normal
        g3 <- g2 +
          scale_y_continuous(limits = ~c(y_scale_range[1], y_scale_range[2]),
                             oob = scales::oob_keep) 
      }
      
    }
    
    # show vertical annotation lines
    if(nrow(annotation_df) > 0){
      
      # named vector
      ann_linetype <- annotation_df |> 
        pull(LINETYPE, LABEL)
      
      g4 <- g3 +
        geom_vline(data = annotation_df,
                   aes(xintercept = DATE, linetype = LABEL), # linetype as dummy here
                   colour = "#808080", linewidth = 0.5) +
        scale_linetype_manual(values = ann_linetype)
      
    } else {
      g4 <- g3
    }
    
    
    # most of the rest
    g <- g4 +
      scale_colour_manual(values = pal) +
      labs(title = "Total VOC Mass Concentrations",
           x = NULL,
           y = "Total VOC Concentration (\u03BCg/L)", 
           shape = NULL,
           colour = NULL,
           linetype = NULL) +
      theme(legend.position = "bottom",
            legend.box = "vertical",
            legend.text = element_text(size = 8),
            legend.box.spacing = unit(0, "pt"), # The spacing between the plotting area and the legend box (unit)
            legend.margin=margin(0, 0, 0, 0)) + # the margin around each legend
      guides(colour = guide_legend(order = 1),
             linetype = guide_legend(order = 2,
                                     override.aes = list(shape = NA)))
  } else {
    
    g <- blank_graph()
  }
  
  # output dependent on whether ggiraph or not
  if(is_ggiraph == TRUE){
    
    girafe(ggobj = g)
    
  } else {
    g
  }
  
} # end function


#' molar concentration total graph - keep in log10
#'
#' @return ggplot or ggiraph object
#' @param dataset 
#' @param wells_selected the wells to show
#' @param analytes_selected the analytes to show
#' @param molar_mass_table a lookup table of molar concentrations 
#' @param min_date the start date of the graph
#' @param max_date the end date of the graph
#' @param annotation_df annotation_lines dataset
#' @param col_pal the selected colour palette
#' @param show_annotations show the annotation lines or not
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#'
molar_conc_sum_graph_ggg <- function(dataset,
                                     wells_selected,
                                     analytes_selected,
                                     molar_mass_table,
                                     min_date, 
                                     max_date,
                                     col_pal,
                                     annotation_df,
                                     show_annotations,
                                     is_ggiraph = FALSE){
  
  # data from helper function
  df <- get_molar_conc_sum_df(dataset,
                              analytes_selected,
                              wells_selected,
                              molar_mass_table,
                              min_date,
                              max_date)$final 
  
  # annotations - filter out if not wanted
  if(show_annotations == "No"){
    annotation_df <- annotation_df |> 
      filter(is.na(DATE))
  }
  

  # colour palette
  n_colours <- length(unique(df$SYS_LOC_CODE))
  pal <- colour_palette_selection(col_pal, n_colours)
  
  if(nrow(df) > 1){
    
    # check that at there is at least one location
    sample_counts <- df %>% 
      count(SYS_LOC_CODE) %>% 
      filter(n == max(n)) %>% 
      slice(1) %>% 
      pull(n)
    
    if(sample_counts > 1){
      
      # date breaks depend on number of years in dataset
      num_years <- interval(min_date, max_date) / years(1)
      if(num_years > 8){
        date_break_value <- "2 years"
      } else {
        date_break_value <- waiver()
      }
      
      # initial graph
      p0 <- ggplot(df, aes(SAMPLE_DATE, value, colour = SYS_LOC_CODE)) +
        theme_bw()
      
      # show vertical annotation lines
      if(nrow(annotation_df) > 0){
        
        # named vector
        ann_linetype <- annotation_df |> 
          pull(LINETYPE, LABEL)
        
        p0a <- p0 +
          geom_vline(data = annotation_df,
                     aes(xintercept = DATE, linetype = LABEL), # linetype as dummy here
                     colour = "#808080", linewidth = 0.5) +
          scale_linetype_manual(values = ann_linetype)
        
      } else {
        p0a <- p0
      }
      
      # then
      p0b <- p0a + 
        scale_colour_manual(values = pal) +
        scale_x_date(labels = scales::label_date_short(),
                     limits = c(min_date, max_date),
                     date_breaks = date_break_value,
                     date_minor_breaks = "1 year") +
        theme(legend.position = "bottom",
              legend.box = "vertical",
              legend.text = element_text(size = 8),
              legend.box.spacing = unit(0, "pt"), # The spacing between the plotting area and the legend box (unit)
              legend.margin=margin(0, 0, 0, 0)) + # the margin around each legend
        guides(colour = guide_legend(order = 1,
                                     byrow = TRUE),
               linetype = guide_legend(order = 2,
                                       override.aes = list(shape = NA))) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
      
      # concentrations
      # intermediate graph depending on if ggiraph
      if(is_ggiraph == TRUE){
        p1 <- p0b +
          geom_line() +
          geom_point_interactive(aes(tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                                  SYS_LOC_CODE: {SYS_LOC_CODE}
                                                  CONCENTRATION: {signif(value, 3)} (\u03BCmol/L)")),
                                 size = 2)
      } else {
        p1 <- p0b +
          geom_line() +
          geom_point(size = 2)
      }
      
      # same scale
      min_val <- min(df$value)
      max_val <- max(max(df$value))
      
      p2 <- p1 +
        scale_y_log10(limits = c(closest_lower(cust_y_poss_vals, min_val),
                                 closest_higher(cust_y_poss_vals, max_val)),
                      breaks = cust_y_poss_vals) 
      
      g <- p2 +
        labs(title = NULL,
             subtitle = "Total Detected Molar Concentrations",
             x = NULL,
             y = "Concentration (\u03BCmol/L) log10 scale",
             colour = NULL,
             linetype = NULL)
      
      
    } else {
      g <- blank_graph()
    }
    
  } else {
    g <- blank_graph()
  }
  
  # output dependent on whether ggiraph or not
  if(is_ggiraph == TRUE){
    
    girafe(ggobj = g,
           # no button - there is a manual one
           options = list(opts_toolbar(saveaspng = FALSE)))
    
  } else {
    g
  }
  
} # end function
