# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
pacman::p_load(targets, here)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c(
    "tibble",
    "tidyverse",
    "here",
    "linelist",
    "janitor",
    "tidylog",
    "fuzzyjoin",
    "table1",
    "gtsummary",
    "arsenal",
    "readxl",
    "sf",
    "raster",
    "here",
    "tictoc"
  ),
  # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

source(here('scripts', 'labels_levels.R'))

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(risk_factor_working_data,
             risk_factor_processing_fn()),
  tar_target(skin_exam_working_data,
             skin_exam_processing_fn()),
  tar_target(dbs_working_data,
             dbs_processing_fn()),
  tar_target(skin_swab_working_data,
             skin_swab_processing_fn()),
  tar_target(form2_working_data,
             form2_processing_fn()),
  tar_target(form2_working_data_update, 
             form2_data_cleaning(merged_data_all, form2_working_data)),
  tar_target(form3.1_working_data,
             form3.1_processing_fn()),
  tar_target(form10_working_data,
             form10_processing_fn()),
  tar_target(form11_working_data,
             form11_processing_fn()),
  tar_target(stool_sample_working_data,
             stool_sample_processing_fn()),
  tar_target(qpcr_results_working_data,
             qPCR_results_processing_fn()),
  tar_target(
    stool_receipt_qpcr_results_merged,
    stool_sample_working_data %>%
      full_join(qpcr_results_working_data, by = c('mda_code'))
  ),
  tar_target(
    stool_receipt_qpcr_results_form11_merged,
    stool_results_processing_fn(stool_receipt_qpcr_results_merged,form11_working_data)
      ),
  tar_target(
    merged_data_all,
    form2_working_data %>%
      full_join(
        skin_exam_working_data,
        by = c('mda_code',
               'f2_age' = 'skin_exam_age',
               'f2_sex_fct' = 'skin_exam_sex_fct')
      ) %>% 
      full_join(form3.1_working_data, by=c('mda_code')) %>%
      #full_join(skin_exam_working_data, by=c('mda_code')) %>%  
      #                                        'f2_participant_name' = 'participant_name')) %>% 
      # left_join(dbs_working_data, by=c('mda_code')) %>% 
      full_join(form10_working_data, by=c('mda_code')) %>%
      #full_join(form11_working_data, by=c('mda_code')) %>% 
      full_join(stool_receipt_qpcr_results_form11_merged, by=c('mda_code')) %>% 
      mutate(mda_code_copy=mda_code) %>% 
      tidyr::separate(mda_code, into=c('province', 'book', 'page', 'line')) %>% 
      mutate(key=paste(f2_area_council_fct, f2_village_fct, province, book,page,f2_household_id, sep='_')) %>% 
      full_join(risk_factor_working_data,by='key') %>% 
      rename(mda_code=mda_code_copy) %>% 
      ungroup()
  ), 
  tar_target(
    analysis_data,
    form2_working_data_update %>%
      full_join(
        skin_exam_working_data,
        by = c('mda_code',
               'f2_age' = 'skin_exam_age',
               'f2_sex_fct' = 'skin_exam_sex_fct')
      ) %>% 
      full_join(form3.1_working_data, by=c('mda_code')) %>%
      full_join(form10_working_data, by=c('mda_code')) %>%
      full_join(stool_sample_working_data, by=c('mda_code')) %>%
      full_join(qpcr_results_working_data, by = c('mda_code')) %>% 
      full_join(stool_sample_working_data, by = c('mda_code')) %>% 
      full_join(qpcr_results_working_data, by = c('mda_code')) %>% 
      full_join(stool_receipt_qpcr_results_form11_merged, by=c('mda_code')) %>% 
      mutate(mda_code_copy=mda_code) %>% 
      tidyr::separate(mda_code, into=c('province', 'book', 'page', 'line')) %>% 
      mutate(key=paste(f2_area_council_fct, f2_village_fct, province, book,page,f2_household_id, sep='_')) %>% 
      full_join(risk_factor_working_data,by='key') %>% 
      rename(mda_code=mda_code_copy) %>% 
      ungroup()
  ),
  tar_target(skin_exam_results_table,
             skin_exam_summary_table_fn(skin_exam_working_data))
)

#tar_visnetwork(targets_only=T)
