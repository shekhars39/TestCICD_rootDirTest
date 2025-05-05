library(tidyverse)
source("data/app_constants.R")
source("data/app_datasets.R")

# function for the empty cols
empty_tibble <- function(col_vec){
  as_tibble(matrix(nrow = 0, ncol = length(col_vec)), .name_repair = ~ col_vec)
}

data <- empty_tibble(wanted_column_names[wanted_column_names != "ACTION_LEVEL_VALUE"] )
criteria <- empty_tibble(c("CAS_RN", "CHEMICAL_NAME", "ACTION_LEVEL_VALUE", "MATRIX_CODE"))
annotation_lines <- empty_tibble(c("DATE", "LABEL", "LINETYPE"))
well_groups <- empty_tibble(c("SYS_LOC_CODE", "LOC_GROUP_CODE"))

a <- list("DATA" = data,
     "CRITERIA" = criteria,
     "ANNOTATION_LINES" = annotation_lines,
     "WELL_GROUPS" = well_groups)

# to excel, maybe the column type doesn't matter here
openxlsx::write.xlsx(a, "general_use_template.xlsx")

# would need notes on acceptable values