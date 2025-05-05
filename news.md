# 0.2.0.4.8 - (4 April 2025)
* first order only runs if the chems are there
* fixed single_chem_many_facets_ggg function

# 0.2.0.4.7 - (3 April 2025)
* additional popups for errors and when not all the sheets are there
* screening summary warning when duplicate SYS_SAMPLE_CODE
* selected wells / single param graphs updated so that they show the annotation lines even when outside dataset time frame
* screening summary should run - was missing 'rv$' in some 'scrn_groundwater' objects

# 0.2.0.4.6 - (4 February 2025)
* y axis limits moved to modal in tvoc graphs page
* caption added for tvoc graphs when only detected shown (no rep value)

# 0.2.0.4.5 - (3 February 2025)
* zero values not being filtered out in initial_clean

# 0.2.0.4.4 - (31 January 2025)
* option for representative values for TVOC graphs

# 0.2.0.4.3 - (30 January 2025)
* tvoc graphs with user-defined range. molar concentration graph also only in log scale

# 0.2.0.4.2 - (29 January 2025)
* tvoc filename suffix from well group if it exists - uses subtitle

# 0.2.0.4.1 - (23 January 2025)
* tvoc subtitle based on well group if it exists
* tvoc graph y-axis with minimal maximal values

# 0.2.0.4 - (21 January 2025)
* fixes to graphs on single dataset view page
* added max ND value to app stats
* first order tables and download
* png names from function for selected_wells and single_dataset_view fixed
* TVOC join function for table and graphs fixed

# 0.2.0.3.1 - (17 January 2025)
* added more molecular weights for TVOCs

# 0.2.0.3 - (15 January 2025)
* Issue with coc_geochem infinite rendering fixed
* app stats well filtering issue fixed
* Upload limit is 16MB again
* locale added to screening summary chemical arranging
* removed westlake specific defaults

# 0.2.0.2 - (14 January 2025)
* Upload limit is now 50MB, with progress bars added during upload and loading process
* ph_facet not used in water quality graph

# 0.2.0.1 - (13 January 2025)
* Fix for water quality graphs

# 0.2.0.0 - (9 January 2025)
* App info page updates with new liability and user guide
* Option for substituted values for Mann-Kendall trend test
* Prod version will have the first order module hidden

# 0.1.10.10 - (8 January 2025)
* Chems in App Stats comes from DATA page
* filters out empty rows for annotation lines

# 0.1.10.9 - (19 December 2024)
* Initial first order model page

# 0.1.10.8 - (9 December 2024)
* TVOC module only runs with TOCs group. No defaults.

# 0.1.10.7 - (6 December 2024)
* TVOC graph point sizes. TVOC chems in excel output.
* Warning if CAS_RN that is not in voc_molecular_weights entered

# 0.1.10.6 - (5 December 2024)
* shape override removed from colour in legend. shows line with dots
* TVOC output changed

# 0.1.10.5 - (4 December 2024)
* different legend for the selected wells and inorganics with get_ann_text_df() added
* coc geochem markdown doc doesn't print if nothing there
* TOC graph heading and colours. png and word downloads
* alphabetical output for selected wells word graphs

# 0.1.10.4 - (3 December 2024)
* Summary of chemicals by location table removed from data summary
* screening summary notes changed with conditional formatting also changed
* sidebar font changed

# 0.1.10.3 - (2 December 2024)
* fixed single chem selected well graphs

# 0.1.10.2 - (28 Nov 2024)
* ce graphs print 'no data' instead of blank graph
* function documentation improved

# 0.1.10.1 - (27 Nov 2024)
* IP info added
* larger text size in CSS for tabs in sidebar
* screening summary note formatting orange highlight for MCL fixed
* single chem/well facet legend for linetype fixed
* inorganics colour palettes
* vertical legend boxes for ce graphs
* water quality faceted graph - not using patchwork

# 0.1.10.0 - (22 Nov 2024)
* Substitution for ND for envstats_initial_helper() and envstats_mk_conf_pred()
* Note for Mann Kendall trend test confidence level slider

# 0.1.9.15 - (20 Nov 2024)
* 'select units' removed from single dataset view - was used in Bannock to deal with multiple units
* inorganics graph legend fixed
* option to download one or all inorganics analytes in word doc
* Additional screening summary text
* MAGs added to data screening tables
* legend wrapped on single chem selected well graph
* 'Standard' changed to 'Standard Criterion' in screening summary

# 0.1.9.14 - (15 Nov 2024)
* total voc graphs updated with both using log10 button
* annotation lines on single chem facets
* removed exceeding standards from summary table sheet in screening summary
* TVOC graph and selection changes
* selected wells graphs changes with annotation lines button
* inorganics graphs with log scale and annotation lines
* TOC word output lists chemicals
* all chemicals option in app stats

# 0.1.9.13 - (13 Nov 2024)
* Word output in Letter size instead of A4
* Analyte groups added in data summary
* molar conc vert lines in front of graph. stacked_area is default
* molar mass tables updated 

# 0.1.9.12 - (7 Nov 2024)
* Single chem well shape fixed

# 0.1.9.11 - (1 Nov 2024)
* Coc geochem button fixed for faceted/combined
* single well/chem graph fixed
* Upload template updated
* downloads for molar mass tables

# 0.1.9.10 - (31 Oct 2024)
* App Stats takes in analyte groups in template
* Fraction removed from screening summary
* Total VOC graphs fixed
* cadmium, nickel, thallium, zinc not converted to mg/L

# 0.1.9.9 - (29 Oct 2024)
* Names and CSS on home page
* Faceted geochem graph option
* FD sample type removed

# 0.1.9.8 - (25 Oct 2024)
* Screening summary fixes - FD and use of 'Standard' instead of what was in the CRITIERIA_UNIT

# 0.1.9.7 - (24 Oct 2024)
* Adjusted colour selection with warnings added

# 0.1.9.6 - (22 Oct 2024)
* Temporarily allow duplicate units
* More informative popup dialogue boxes showing the issues.

# 0.1.9.5 - (18 Oct 2024)
* Colour palettes changed
* Updated upload template
* more unit conversions in initial clean

# 0.1.9.4 (15 Oct 2024)
* Changes to single dataset view
* Fixed screening summary server not showing up

# 0.1.9.3 (10 Oct 2024)
* Combined Selected Wells/Single Chemical Graph module with associated changes
* Graph changes for COC geochem charts

# 0.1.9.2 (9 Oct 2024)
* Default chems in screening summary page based on uploaded criteria
* Text on screening summary page
* COC geochem stacked bar graph instead of stacked area

# 0.1.9.1 (4 Oct 2024)
* Data summary sheet has tables and information about uploaded data
* App info updated

# 0.1.9 (23 Sep 2024)
* Updated templated with fewer required columns in DATA sheet
* Total VOC page added

# 0.1.8.3 (20 Sep 2024)
* App stats joins on CAS_RN only, not on CHEMICAL_NAME

# 0.1.8.2 (18 Sep 2024)
* Popup when upload does not contain a 'DATA' sheet

# 0.1.8.1 (10 Sep 2024)
* Colour palette selection for selected wells page

# 0.1.8 (27 Sep 2024)
* Name changed to General Application for Groundwater Analysis (GAGA)
* Log10 Geochemistry chart fixed
* Set up for well groups in upload file
* Split from Westlake app (which will be retired soon)
* Initial version to upload from a template

# 0.1.7.2 (1 Aug 2024)
* Edited water quality chart. Download button on selected wells for single chem. geom_line added. geom_point(size = 3).

# 0.1.7.1 (14 June 2024)
* Sheet selection when uploading data. No standard labels on COC geochem graphs

# 0.1.7 (27 May 2024) 
* Selected wells page added

# 0.1.6 (24 May 2024) 
* File size limit is now 10MB. Geochem graphs indexed by name rather than position. Option to remove vertical timepoint lines. Fraction filter for single chem page.

# 0.1.5.1 (4 Mar 2024) 
* Roxygen documentation and function tidying. Molar conc table order.

# 0.1.5 (20 Feb 2024) 
* Screening summary added

# 0.1.4.1 (15 Feb 2024) 
* Hotfix - inorganics page

# 0.1.4 (9 Feb 2024) 
* Lines for LCM RA, Site Restoration as well as VC and cis,1,2-DCE

# 0.1.3 (7 Feb 2024) 
* Fractions in single dataset view and download fixed, GSI COV moved and renamed, conditional formatting in trends removed.

# 0.1.2.1 (6 Feb 2024) 
* Hotfix - Colours and app stats output from previous release

# 0.1.2 (5 Feb 2024) 
* COC and geochem graphs with names from CHEMICAL_NAME and colours simplified. Action levels from dataset. SelectizeInput() instead of checkboxInput() for wells.

# 0.1.1 (24 Jan 2024)
* Initial application - COC and geochem, inorganics, single chem, single dataset view, and application statistics.
