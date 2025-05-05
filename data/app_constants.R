# app version --------------
version_number <- "0.2.0.4.8"
version_date <- "2025-04-04" # ymd

# date formats -----------
date_format_dmy <- "%d %B %Y"
date_format_mdy <- "%m/%d/%Y"

# plotly heights ------------------
plotly_height_val <- "800px"
plotly_small_height_val <- "200px"

# word template for markdown ------------------------------------------------------
word_template_file <- "lft_templates/blank_aus_edit_Letter.docx"

# app info page text -------------------------------------------------------------------
introduction_text <- HTML("The GAGA is an R Shiny web application hosted on an internal CDM Smith server, 
                          developed under CDM Smith&rsquo;s R&amp;D program in partnership with CDM Smith Australia. ",
                          "The GAGA is a groundwater data analytics tool that generates automated, accurate, 
                           reproducible outputs to support efficient and interactive data analysis and reporting. ",
                          "Appropriate use of this tool can significantly reduce time, budget, and personnel requirements and improve quality of deliverables. ",
                          "Its modules allow the user to upload project-specific data, screen data against numerical criteria,
                           generate a variety of interactive and downloadable time series graphs,
                           and perform statistical analysis in accordance with relevant U.S. Environmental Protection Agency (EPA) guidance and industry practices. ",
                          "Results graphics and tables can be downloaded in the form of consistent, formatted, and organized Microsoft Word, Excel, or png files.")

liability_text <- HTML("<b>The General Application for Groundwater Analytics (GAGA) tool is for CDM Smith internal use only. 
                       Please do not share this link with others, including other CDM Smith employees. </b></br>",
                       "You have been provided access to the tool specifically for your project.
                       No part of it may be shared or produced for external viewing without the permission of the Research and Development (R&D) team contacts,
                       the appropriate Business Technology (BT) Data Engineering representative(s), and Office of the General Counsel (OGC).
                       Because the GAGA tool is in development, users need to be tracked for quality assurance until it is ready for broader release. </br>",
                       "</br>",
                       "<b>This application is the intellectual property of CDM Smith, 
                       and the use of this application for a client project does not grant the Client any ownership or license in the application. </b></br>",
                       "Please communicate with R&D team contacts regarding sharing with other CDM Smith employees or limited client viewing (e.g., slides, screen-sharing). 
                       Outputs (charts, screening and statistical tables) can be shared with clients,
                       but it is very important to maintain ownership to the GAGA and that clients cannot access the GAGA. 
                       Please work with OGC to ensure we have the appropriate IP protection language.  </br>",
                       "</br>",
                       "<b>By accessing this link to GAGA, you agree that you have executed, 
                       delivered, are bound and accept the CDM Smith Employee Confidentiality, 
                       Invention and Writing Agreement and all CDM Enterprise Intellectual Property and Invention Development policies. </b></br>",
                       "For clarification, these terms supplement, 
                       and do not modify or replace the terms of the agreements referenced above or any other agreements that you have already entered into with CDM Smith.  
                       In the event of a conflict between those agreements and the terms set forth herein, the more restrictive terms shall apply.")

# stats text -----------------------------------------------------------------------
stat_notes_text <- htmltools::HTML("<ul>",
                        "<li><a href = 'https://doi.org/10.1111/j.1745-6584.2003.tb02605.x\', target=\'_blank\'>
                        Aziz, J., L. Meng, H. Rifai, C. Newell, and J. Gonzales. 2003.
                        'MAROS: A Decision Support System for Optimizing Monitoring Plans,'
                        Ground Water 41, no. 3 (May - June): 355 - 67.</a></li>",          
                        "<li><a href = 'https://cfpub.epa.gov/si/si_public_record_Report.cfm?dirEntryId=354971&Lab=CESER\', target=\'_blank\'>
                        Barnett, F., P. Carson, T. Linscome-Hatfield, AND H. Brittingham.
                        ProUCL 5.2. U.S. Environmental Protection Agency, Washington, DC, 2022.</a></li>",
                        "<li><a href = 'https://www.gsienv.com/product/gsi-mann-kendall-toolkit/', target=\'_blank\'>
                        Connor, J., S. Farhat, and M. Vanderford. 2012. Software User Manual GSI Mann-Kendall Toolkit for Constituent Trend Analysis. Version 1.</a></li>",
                        "<li><a href = 'https://archive.epa.gov/epawaste/hazard/web/pdf/unified-guid.pdf\', target=\'_blank\'>
                        EPA (United States Environmental Protection Agency). 2009. 
                        Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities,
                        Unified Guidance. Washington, DC: EPA. EPA 530/R-09-007.</a></li>",
                        "<li><a href = 'https://www2.deq.idaho.gov/admin/LEIA/api/document/download/7087\', target=\'_blank\'>
                        Idaho Department of Environmental Quality. 2014.
                        Statistical Guidance for Determining Background Ground Water Quality and Degradation, 
                        Boise, ID: State of Idaho.</a></li>",
                        "</ul>")

stats_packages_text <- htmltools::HTML("<ul>",
                            "<li>The <a href = 'https://cran.r-project.org/web/packages/EnvStats/index.html\', target=\'_blank\'> EnvStats</a> package for Environmental Statistics, Including US EPA Guidance</li>",
                            "<li>The <a href = 'https://www.tidyverse.org/\', target=\'_blank\'> tidyverse</a> collection of packages</li>",
                            "</ul>")

# screening summary text dfs ----------------
screen_notes <- tibble::tribble(
  ~`Notes:`,
  "Orange highlight indicates values greater than the Standard Criterion",
  "Bold indicates a detected value",
  "Italics indicate a nondetect value"
)

screen_abbrevs <- tibble::tribble(
  ~`Abbreviations:`,
  "Standard Criterion = The maximum contaminant level for the analyte",
  "Q = Qualifier",
  "J = Result estimated",
  "U = Analyte was not detected at the associated value",
  "UJ = The non-detection at the associated value is an estimate",
  "J+ = The result is estimated and biased high",
  "\u03BCg/L = micrograms per litre", # µg/L
)

exceedance_summary_notes <- tibble::tribble(
  ~`Notes:`,
  "This tables show values where the pH value is outside of the range 6.5-8.5 ",
  "or for other analytes, where they exceed the standard and do not have a 'U' qualifier"
)

# values for custom y scales ------------------------
cust_y_poss_vals <- map(c(-10:10), function(x){
  a <- paste0("1e", x)
  as.numeric(a) 
}) %>% 
  unlist()

# notes at the bottom of the Application Statistics ---------------------
# provided by Emma
app_stats_abbrevs <- tibble::tribble(
  ~`Acronyms and Abbreviations`,
  "% - percent",
  "alpha - the number against which probability is compared to determine statistical significance, corresponding to a confidence level equal to 1 minus alpha as a percent",
  "J - estimated result",
  "MDL - method detection limit",
  "n - dataset result count",
  "NC - not calculated",
  "ND - dataset nondetect count",
  "p-value - probability that S would occur without a statistically significant trend",
  "Q - qualifier",
  "S - Mann Kendall S statistic, the number that represents all samples and direction of trend in the Mann-Kendall analysis",
  "sd(S) - Standard deviation of S (computed using equations 10-14 and 10-15 on page 276 of the ProUCL technical guide v5.20)",
  "TS - Theil Sen",
  "U - nondetect result (value equals MDL)",
  "Z - The standardised S statistic (computed using equations 10-16 on page 276 of the ProUCL technical guide v5.20)",
  "\u03BCg/L - micrograms per liter", # µg/L
  "GSI COV = Covariance (Standard deviation / mean) of (up to) the last 40 observations",
  "GSI Toolkit Trend = The trend as per the GSI Toolkit"
)

# inorg chems ------------------------------------------------------------------
# c("Aluminum", "Antimony", "Arsenic", "Barium", "Beryllium", "Boron", "Cadmium", "Calcium", "Chromium", "Cobalt", "Copper", "Cyanide", "Iron", "Lead", "Magnesium", "Manganese", "Mercury", "Nickel", "Potassium", "Selenium", "Silver", "Sodium", "Sulfide", "Thallium", "Tin", "Vanadium", "Zinc")
inorg_chems_cas_rn <- c("7439-89-6", "7440-36-0", "7440-38-2", "7440-39-3", "7440-41-7", "7440-70-2", "7440-43-9", "7439-95-4", "7440-47-3", "7440-48-4", "7440-50-8", "57-12-5", "7439-96-5", "7439-92-1", "7440-09-7", "7429-90-5", "7439-97-6", "7440-02-0", "7440-23-5", "7782-49-2", "7440-22-4", "7440-42-8", "18496-25-8", "7440-28-0", "7440-31-5", "7440-62-2", "7440-66-6")

# tvoc chems -------------------------------------------------------------------
#                              "PCE", "TCE", "cis-1,2-DCE", "trans-1,2-DCE", "1,1-DCE", "Vinyl Chloride", "Ethene", "Ethane", "Acetylene"
tvoc_default_chems_cas_rn <- c("127-18-4", "79-01-6", "156-59-2", "156-60-5", "75-35-4", "75-01-4", "74-85-1", "74-84-0", "74-86-2")

# colour palettes to choose from where there is a choice --------------------------------
colour_palettes <- c("R4", "Classic Tableau", "Pastel 1", "Dark 2", "Viridis", "Inferno", "Mako", "Plasma", "Rocket")