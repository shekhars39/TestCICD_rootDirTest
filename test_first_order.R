# load libraries ------------------------
source("load_libraries.R")

# # load datasets ---------------------------------------------------------------
source("data/app_constants.R")
source("data/app_datasets.R")
source("data/voc_molecular_weights.R")

# load functions ---------------------------------------------------------------
list.files("functions", pattern = ".R") %>%
  walk(~source(glue("functions/{.x}")))

list.files("lft_templates", pattern = ".R") %>%
  walk(~source(glue("lft_templates/{.x}")))

path <- "./test_upload_files/general_use_test1e.xlsx"
# path <- "./test_upload_files/limited TAPCO data-- HR tests.xlsx"
# path <- "./test_upload_files/limited TAPCO data-- edit1.xlsx" # deliberately incorrect VOC
# path <- "./test_upload_files/Copy of Data_test.xlsx"
# path <- "./test_upload_files/DATA s008b-Export-MW-Wells TAPCO 20240806_fixed CASRN.xlsx"
# path <- "./test_upload_files/GAGA input BPMM 20241101_ELE.xlsx" # edited to add criteria for sulfide, has inorgs

# get the datasets --------------------------------------------------------------
source("test_upload_files/test_run_from_excel.R")


first_order_data(data_df = rv$clean_df, # maybe one of the others
                 well_selected = "DMW21A",
                 analyte_selected = "Cis-1,2-Dichloroethylene",
                 min_date = "2014-07-15",
                 max_date = "2023-10-25",
                 frac = "T",
                 criteria_df = rv$criteria_df,
                 molar_mass_table = voc_molecular_weights,
                 conf_level = 0.95)

first_order_data(data_df = rv$clean_df, # maybe one of the others
                 well_selected = "DMW21A",
                 analyte_selected = "Vinyl Chloride",
                 min_date = "2014-07-15",
                 max_date = "2023-10-25",
                 frac = "T",
                 criteria_df = rv$criteria_df,
                 molar_mass_table = voc_molecular_weights,
                 conf_level = 0.95)

first_order_data(data_df = rv$clean_df, # maybe one of the others
                 well_selected = "DMW21A",
                 analyte_selected = "Cis-1,2-Dichloroethylene",
                 min_date = "2014-07-15",
                 max_date = "2023-10-25",
                 frac = "T",
                 criteria_df = rv$criteria_df,
                 molar_mass_table = voc_molecular_weights,
                 conf_level = 0.95,
                 is_ggiraph  = TRUE)

