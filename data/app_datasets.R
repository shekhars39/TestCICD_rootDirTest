# values on log10 scale ------------------------------------------------------
log10_num_vec <- c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 1e1, 1e2, 1e3, 1e4, 1e5)

# the column names required in the equis dataset -------------------------------------------------------
# this excludes the LOC_GROUP_CODE and  METHOD_ANALYTE_GROUP that are in the Bannock app
wanted_column_names <- c("SYS_LOC_CODE", "CHEMICAL_NAME", "CAS_RN",  "FRACTION", "SAMPLE_DATE",
                         "SYS_SAMPLE_CODE", "SAMPLE_TYPE_CODE", "REPORT_RESULT_VALUE",  "DETECT_FLAG",
                         "REPORT_RESULT_UNIT", "INTERPRETED_QUALIFIERS", "METHOD_DETECTION_LIMIT", "MATRIX_CODE")

# molar concentrations -----------------------------------------------------------

# note: different to the data/voc_molecular_weights.R file
molecular_weights <- tibble::tribble(
  ~CAS_RN,    ~unit, ~molar_mass,
  "127-18-4", "umol/L", 165.8339996,     # "PCE"
  "79-01-6", "umol/L", 131.3889465,     #   "TCE"
  "156-59-2", "umol/L", 96.94387817,     #   "cis-1,2-DCE"
  "75-35-4", "umol/L", 96.94387817,     #   "1,1-DCE"
  "156-60-5", "umol/L", 96.94387817,     #   "trans-1,2-DCE"
  "75-01-4", "umol/L", 62.49882126,     #   "Vinyl Chloride"
  "74-85-1", "umol/L",      28.052,     #   "Ethene"
  "74-84-0", "umol/L",      30.068,     #   "Ethane"
  "74-86-2", "umol/L",      26.036,     #   "Acetylene"
  "16887-00-6", "mmol/L",       35.45     #   "Chloride"
)

# analytes for the application statistics ----------------------------------------
default_subset_analytes <- c(
  "127-18-4", # PCE
  "79-01-6", # TCE
  "156-59-2",  # cis-1,2-DCE
  "75-35-4", # 1,1-DCE 
  "156-60-5", # trans-1,2-DCE
  "75-01-4", # Vinyl Chloride 
  "74-85-1",  # Ethene 
  "74-84-0",  # Ethane
  "74-82-8",  # Methane
  "16887-00-6" # Chloride
)

# to build graph groupings from the CAS_RN -------------------------------------------------------
# use to build graph_groupings
cas_rn_graph_groups <- list("Chlorinated Ethene and Daughter Product" = c("75-35-4", "74-86-2", "16887-00-6",
                                                                          "74-84-0", "74-85-1", "127-18-4",
                                                                          "79-01-6", "75-01-4", "156-59-2", "156-60-5"),
                            "Redox Parameters" = c("DO", "FE(FS)", "7439-96-5", "74-82-8", "14797-55-8",
                                                   "14797-65-0", "NO3NO2N", "ORP", "14808-79-8"),
                            "Other Geochemistry" = c("ALK", "TOC", "PH"),
                            "Water Quality Parameters" = c("ALK", "16887-00-6", "PH"),
                            "TOC and Redox Parameters" = c("DO", "FE(FS)", "7439-96-5", "74-82-8", "14797-55-8",
                                                           "14797-65-0", "NO3NO2N", "ORP", "14808-79-8", "18496-25-8", "TOC"),
                            "order" = c("127-18-4", "79-01-6", "156-59-2", "156-60-5", "75-35-4", "75-01-4", "74-85-1", 
                                        "74-84-0", "74-86-2", "16887-00-6", "DO", "NO3NO2N", "14797-55-8", "14797-65-0",
                                        "FE(FS)", "7439-96-5", "14808-79-8", "74-82-8", "ORP", "ALK", "PH", "TOC", "18496-25-8"))

# short names - used in the graph groupings
cas_rn_analyte_short <- tibble::tribble(
  ~CAS_RN,                ~analyte_short,
  "75-35-4",                     "1,1-DCE",
  "74-86-2",                   "Acetylene",
  "ALK", "Total Alkalinity (as CaCO3)",
  "16887-00-6",                    "Chloride",
  "DO",            "Dissolved Oxygen",
  "74-84-0",                      "Ethane",
  "74-85-1",                      "Ethene",
  "FE(FS)",                "Ferrous Iron",
  "7439-96-5",                   "Manganese",
  "74-82-8",                     "Methane",
  "14797-55-8",                     "Nitrate",
  "14797-65-0",                     "Nitrite",
  "NO3NO2N",  "Nitrate and Nitrite (as N)",
  "ORP",                         "ORP",
  "14808-79-8",                     "Sulfate",
  "18496-25-8",                     "Sulfide",
  "127-18-4",                         "PCE",
  "TOC",                         "TOC",
  "79-01-6",                         "TCE",
  "75-01-4",              "Vinyl Chloride",
  "156-59-2",                 "cis-1,2-DCE",
  "PH",                          "pH",
  "156-60-5",               "trans-1,2-DCE"
)

# all of these are analyte short names - vectors for graph ordering -----------------------------
chlor_eth_factor_order <- c("PCE", "TCE", "cis-1,2-DCE", "trans-1,2-DCE", "1,1-DCE", "Vinyl Chloride", "Ethene", "Ethane", "Acetylene", "Chloride")
redox_factor_order <- c("Dissolved Oxygen", "Nitrate and Nitrite (as N)", "Nitrate", "Nitrite", "Ferrous Iron", "Manganese", "Sulfate", "Methane", "ORP")
gchem_factor_order <- c("Total Alkalinity (as CaCO3)", "pH", "TOC")
toc_redox_factor_order <- c("TOC", "Dissolved Oxygen", "Nitrate and Nitrite (as N)", "Nitrate", "Nitrite",
                            "Ferrous Iron", "Manganese", "Sulfate", "Methane", "Sulfide", "ORP")

# colours --------------------------------------------------
# try to use this!
graph_colours_df <-  tibble::tribble(
  ~CAS_RN,   ~colour_code,
  "74-82-8", "#DEC197", # "Methane"
  "127-18-4", "#B65708", # "PCE"
  "14797-55-8", "#A89BB0", # "Nitrate"
  "14808-79-8", "#F8D348", # "Sulfate"
  "156-59-2", "#F79646", # "cis-1,2-DCE"
  "156-60-5", "#D99694", # "trans-1,2-DCE"
  "16887-00-6", "#0000FF", # "Chloride"
  "18496-25-8", "#0000CD", # "Sulfide"
  "7439-96-5", "#C7D846", # "Manganese"
  "74-84-0", "#92CE50", # "Ethane"
  "74-85-1", "#00AD4F", # "Ethene"
  "75-01-4", "#FFFF00", # "Vinyl Chloride"
  "75-35-4", "#000000", # "1,1-DCE"
  "79-01-6", "#FF0000", # "TCE"
  "ALK", "#66C2A5", # "Total Alkalinity (as CaCO3)"
  "DO", "#66C2A5", # "Dissolved Oxygen"
  "FE(FS)", "#C6B18B", # "Ferrous Iron"
  "ORP", "#B3B3B3", # "ORP"
  "PH", "#FC8D62", # "pH"
  "TOC", "#8DA0CB", # "TOC",
  "123-91-1", "#7CFC00" # 1,4-Dioxane
)

# excel column headers for the screening summary --------------------
excel_cols <- c(LETTERS[1:26],
                map(LETTERS, ~paste0(.x, LETTERS)) %>% 
                  flatten_chr())

# load interim datasets --------------------------------------------------------
