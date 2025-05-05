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

path <- "./test_upload_files/DataInput_20250219.xlsx"

# get the datasets --------------------------------------------------------------
source("test_upload_files/test_run_from_excel.R")


# the current version --------------------------------------------------------------------------
wells <- rv$well_groups_df |>
  filter(LOC_GROUP_CODE == "Injection") |>
  pull(SYS_LOC_CODE) # note that the SYS_SAMPLE_CODE is missing in this dataset

scrn_groundwater <- screening_report_datasets(rv$screening_df %>%
                                                filter(SYS_LOC_CODE %in% wells,
                                                       !is.na(REPORT_RESULT_UNIT),
                                                       CHEMICAL_NAME %in% c("1,1-DICHLOROETHENE", "ANTIMONY", "ARSENIC", "LEAD")),
                                              criteria = rv$criteria_df,
                                              start_date = "2010-01-010")

groundwater_data <- scrn_groundwater$chem_criteria |> 
  arrange(SYS_LOC_CODE, SAMPLE_DATE, CHEMICAL_NAME) 

## only for function testing
# crit_dataset <- groundwater_data
# combined_dataset <- combined

wb <- screening_with_qualifier(groundwater_data)
openXL(wb)

# some other version ------------------------------------------------------------

# make it sort of look like the esdat exceedance tables
# multiple criteria though

scrn_groundwater$chem_criteria
rv$criteria_df

rv$screening_df

# this looks alright for the chem results
# but how would you do the formatting?
# would need regex on the numeric bit, but also the text bit
# the criteria would need to be above somewhere maybe
test1 <- rv$screening_df |> 
  filter(FRACTION == "T") |> 
  highest_filter() |> 
  mutate(CHEM_UNIT = glue("{CHEMICAL_NAME} ({REPORT_RESULT_UNIT})"),
         Q_TEXT = if_else(is.na(INTERPRETED_QUALIFIERS), "", INTERPRETED_QUALIFIERS),
         RESULT_TEXT = glue("{REPORT_RESULT_VALUE} {Q_TEXT}")) |> 
  select(SYS_LOC_CODE, CHEM_UNIT, SAMPLE_DATE, SYS_SAMPLE_CODE, RESULT_TEXT) |> 
  pivot_wider(id_cols = c(SYS_LOC_CODE, SAMPLE_DATE, SYS_SAMPLE_CODE), names_from = CHEM_UNIT, values_from = RESULT_TEXT) |> 
  arrange(SYS_LOC_CODE, SAMPLE_DATE)



