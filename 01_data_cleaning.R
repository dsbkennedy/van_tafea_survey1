library(here)
library(tidyverse)
library(linelist)
library(janitor)
library(readxl)
library(tidylog)
library(fuzzyjoin)
source(here('labels_levels.R'))
pacman::p_load(arsenal)


# Risk factor -------------------------------------------------------------
risk_factor_raw <-
  here('data',
       'input',
       'Tafea_risk factor questionnaire_FOR STATA.xlsx')
risk_factor_raw <- read_xlsx(risk_factor_raw)


(
  risk_factor_processed <- risk_factor_raw %>%
    mutate(
      sex_fct = factor(sex, levels = c('Male', 'Female')),
      province_fct = factor(province, levels = province_names),
      area_council_fct = factor(area_council, levels = area_council_names),
      village_fct = factor(village, levels = c(village_names)),
      across(
        yes_no_factor_names,
        factor,
        levels = c('No', 'Yes'),
        .names = "{.col}_fct"
      ),
      across(
        checked_unchecked_factor,
        factor,
        levels = c('Unchecked', 'Checked'),
        .names = "{.col}_fct"
      ),
      head_education_fct = factor(head_education, levels = education_levels,
                                  labels = education_labels),
      head_occupation_fct = factor(head_occupation, levels = occupation_levels,
                                   labels = occupation_labels),
      head_income_fct = factor(head_income, levels = income_levels,
                               labels = income_labels),
      across(
        bednet_vars,
        factor,
        levels = bednet_levels,
        labels = bednet_labels,
        .names = "{.col}_fct"
      ),
      bed_net_last_night_fct = factor(bed_net_last_night, levels = bednet_lastnight_levels,
                                      labels = bednet_lastnight_labels),
      bed_net_observe_fct = factor(
        bed_net_observe,
        levels = c('No', 'Yes', 'No save checkem'),
        labels = c('No', 'Yes', 'Could not observe')
      ),
      across(
        drinking_water_vars,
        factor,
        levels = drinking_water_levels,
        labels = drinking_water_labels,
        .names = "{.col}_fct"
      ),
      time_no_water_recode = case_when(
        washing_water_all_year_time_specify == "Wik" ~ washing_water_all_year_time *
          7,
        washing_water_all_year_time_specify == "Dei" ~ washing_water_all_year_time *
          1,
        washing_water_all_year_time_specify == "Manis" ~ washing_water_all_year_time *
          30
      ),
      hyg_hw_obs_fct = factor(hyg_hw_obs, levels = hyg_levels,
                              labels =),
      san_hh_toilet_obs_fct = factor(san_hh_toilet_obs, levels = toilet_levels,
                                     labels = toilet_labels),
      san_hh_toilet_obs_pit_slab_fct = factor(
        san_hh_toilet_obs_pit_slab,
        levels = c('Nokat slab', 'Wetem slab'),
        labels = c('Without slab', 'With slab')
      ),
      san_hh_toilet_obs_pit_flush_fct = factor(
        san_hh_toilet_obs_pit_flush,
        levels = pit_levels,
        labels = pit_labels
      ),
      complete_fct = factor(
        complete,
        levels = c(0, 1),
        labels = c('Incomplete', 'Complete')
      )
    )
)



# Skin exam ---------------------------------------------------------------

skin_exam_raw <-
  here('data', 'input', 'Tafea_skin exam form_FOR STATA.xlsx')
skin_exam_raw <- read_xlsx(skin_exam_raw)

(
  skin_exam_processed <- skin_exam_raw %>%
    mutate(
      sex_fct = factor(sex, levels = c('Male', 'Female')),
      province_fct = factor(province, levels = province_names),
      area_council_fct = factor(area_council, levels = area_council_names),
      village_fct = factor(village, levels = c(village_names)),
      across(
        skin_varlist,
        factor,
        levels = c('No', 'Yes'),
        .names = "{.col}_fct"
      ),
      across(
        skin_checklist,
        factor,
        levels = c('Unchecked', 'Checked'),
        .names = "{.col}_fct"
      ),
      complete_fct = factor(
        complete,
        levels = c(0, 1),
        labels = c('Incomplete', 'Complete')
      ),
      yaws_first_ulcer_fct = factor(
        yaws_first_ulcer,
        levels = c('Yes (first taem)',
                   'No'),
        labels = c('Yes', 'No')
      ),
      yaws_previous_treatment_drug_fct = factor(yaws_previous_treatment_drug,
                                                levels = drug_fct_levels),
      yaws_ulcer_location_fct = factor(yaws_ulcer_location,
                                       levels = ulcer_location_levels,
                                       labels = ulcer_location_labels),
      yaws_dpp_result_fct = factor(yaws_dpp_result,
                                   levels = dpp_result_levels,
                                   labels = dpp_result_labels)
    )
)




# DBS ---------------------------------------------------------------------
dbs_raw <-
  here('data', 'input', 'Tafea_DBS sample IDs_FOR STATA.xlsx')
dbs_raw <- read_xlsx(dbs_raw)


# Form 2 ------------------------------------------------------------------

form2_processing_fn <- function() {
form2_1_raw <-
  here('data',
       'input',
       'Tafea_form 2_data entry person 1_FOR STATA.xlsx')
form2_1_raw <- read_xlsx(form2_1_raw)

(
  form2_1_processed <- form2_1_raw %>%
    mutate(
      f2_1_sex_fct = factor(
        f2_1_sex,
        levels = c('M', 'F'),
        labels = c("Male", "Female")
      ),
      f2_1_area_council_fct = factor(f2_1_area_council, levels = area_council_names),
      f2_1_village_fct = factor(f2_1_village, levels = c(village_names)),
      across(
        form2_1_yesno,
        factor,
        levels = c('N', 'Y'),
        labels = c('No', "Yes"),
        .names = "{.col}_fct"
      ),
      data_entry_person=1
    )  %>% 
    select(-c(f2_1_sex, f2_1_area_council, f2_1_village, 
              all_of(form2_1_yesno))) %>% 
    clean_data() %>% 
    remove_empty('rows') %>% 
    filter(!is.na(mda_code) & !is.na(f2_1_participant_name))
)
  

names(form2_1_processed) <- str_replace(names(form2_1_processed), '_1_', '_')
names(form2_1_processed)

form2_2_raw <-
  here('data',
       'input',
       'Tafea_form 2_data entry person 2_FOR STATA.xlsx')
form2_2_raw <- read_xlsx(form2_2_raw)

name_update <- readRDS(here('data', 'robjects', 'name_updates.Rds')) %>% 
  distinct(mda_code, f2_participant_name_upd)

(
  form2_2_processed <- form2_2_raw %>%
    mutate(
      f2_2_sex_fct = factor(
        f2_2_sex,
        levels = c('M', 'F'),
        labels = c("Male", "Female")
      ),
      f2_2_area_council_fct = factor(f2_2_area_council, levels = area_council_names),
      f2_2_village_fct = factor(f2_2_village, levels = c(village_names)),
      across(
        form2_2_yesno,
        factor,
        levels = c('N', 'Y'),
        labels = c('No', "Yes"),
        .names = "{.col}_fct"
      ),
      data_entry_person=2
    ) %>% 
    select(-c(f2_2_sex, f2_2_area_council, f2_2_village, 
              all_of(form2_2_yesno))) %>% 
    clean_data() %>% remove_empty('rows') %>% 
    filter(!is.na(mda_code) & !is.na(f2_2_participant_name)) %>% 
    left_join(name_update, by='mda_code') %>%
    mutate(f2_participant_name_upd=case_when(
      f2_2_participant_name=='orely_nawawin' ~ 'oreley_nawawin',
      f2_2_participant_name=='marius_nawawin' ~ 'mareius_nawawin',
      TRUE ~ f2_participant_name_upd
    )) %>% 
    mutate(f2_2_participant_name=coalesce(f2_participant_name_upd, f2_2_participant_name)) %>%
    select(-f2_participant_name_upd)
)

names(form2_2_processed) <- str_replace(names(form2_2_processed), '_2_', '_')

# Check for consistency between data entered by person 1 & 2


# Where data is missing, replace with non-missing values

form2_combine_fill <- form2_1_processed %>% mutate(data_entry=1) %>% 
  bind_rows(form2_2_processed %>% mutate(data_entry=2)) %>% 
  group_by(mda_code,f2_participant_name) %>% 
  arrange(mda_code, f2_participant_name) %>% 
  fill(everything(), .direction = "downup")

correct_names_fn <- function() {
  name_cleaning <- form2_combine_fill %>% 
    ungroup() %>% 
    count(f2_participant_name) %>% 
    filter(n!=2) %>% select(-n) %>% 
    left_join(.,form2_combine_fill %>% 
                ungroup() %>% 
                select(mda_code,f2_participant_name, data_entry), 
              by=c('f2_participant_name')) %>% 
    mutate(data_entry=case_when(data_entry==1 ~ 'data_entry1',
                                data_entry==2 ~ 'data_entry2')) %>% 
    pivot_wider(names_from=data_entry, 
                values_from=f2_participant_name) 
  
  name_cleaning_fuzzy <- name_cleaning %>% 
    select(mda_code, data_entry1) %>% 
    filter(!is.na(data_entry1)) %>% 
    stringdist_inner_join(
      x %>% select(mda_code, data_entry2) %>% 
        filter(!is.na(data_entry2)),
      by = c("data_entry1" = "data_entry2"),
      max_dist = 3,
      distance_col = "distance"
    ) %>% 
    mutate(f2_participant_name_upd=coalesce(data_entry1, data_entry2)) %>% 
    select(mda_code=mda_code.x, f2_participant_name_upd)
  
  saveRDS(name_cleaning_fuzzy, here('data', 'robjects', 'name_updates.Rds'))
}

coorect_mda_fn <- function() {
mda_cleaning <- form2_combine_fill %>% 
     ungroup() %>% 
     count(mda_code) %>% 
     filter(n!=2) %>% 
     #select(-n) %>% 
     left_join(.,form2_combine_fill %>% 
                 ungroup() %>% 
                 select(mda_code,f2_participant_name, data_entry), 
               by=c('mda_code')) %>% 
     mutate(data_entry=case_when(data_entry==1 ~ 'data_entry1',
                                 data_entry==2 ~ 'data_entry2')) %>% 
     pivot_wider(names_from=data_entry, 
                 values_from=mda_code) 
}

form2_1_processed_upd <- form2_combine_fill %>% filter(data_entry==1) 
form2_2_processed_upd <- form2_combine_fill %>% filter(data_entry==2)

form2_compare <- summary(comparedf(form2_1_processed_upd %>% 
                                     select(-c(data_entry_person,
                                               data_entry, 
                                               f2_date_day1
                                     )), 
                                   form2_2_processed_upd %>% 
                                     select(-c(data_entry_person,
                                               data_entry, 
                                               f2_date_day1
                                     )), 
                                   by=c('mda_code','f2_participant_name'),
                                   int.as.num = TRUE))

form2_differences_list <- form2_compare$diffs.table
form2_differences_summary <- form2_compare$comparison.summary.table
return(form2_working_data)
}

form2_working_data <- form2_processing_fn()
# Form 3 ------------------------------------------------------------------

form3_1_raw <-
  here('data',
       'input',
       'Tafea_form 3_data entry person 1_FOR STATA.xlsx')
form3_1_raw <- read_xlsx(form3_1_raw)

(
  form3_1_processed <- form3_1_raw %>%
    mutate(
      f3_1_sex_fct = factor(
        f3_1_sex,
        levels = c('M', 'F'),
        labels = c("Male", "Female")
      ),
      f3_1_area_council_fct = factor(f3_1_area_council, levels = area_council_names),
      f3_1_village_fct = factor(f3_1_village_school, levels = c(village_names)),
      across(
        form3_1_yesno,
        factor,
        levels = c('N', 'Y'),
        labels = c('No', "Yes"),
        .names = "{.col}_fct"
      )
    ) %>% remove_empty('rows') %>% 
    filter(!is.na(mda_code) & !is.na(f3_1_participant_name))
)

names(form3_1_processed) <- str_replace(names(form3_1_processed), '_1_', '_')
names(form3_1_processed)

form3_2_raw <-
  here('data',
       'input',
       'Tafea_form 3_data entry person 2_FOR STATA.xlsx')
form3_2_raw <- read_xlsx(form3_2_raw)

(
  form3_2_processed <- form3_2_raw %>%
    mutate(
      f3_2_sex_fct = factor(
        f3_2_sex,
        levels = c('M', 'F'),
        labels = c("Male", "Female")
      ),
      f3_2_area_council_fct = factor(f3_2_area_council, levels = area_council_names),
      f3_2_village_fct = factor(f3_2_village_school, levels = c(village_names)),
      across(
        form3_2_yesno,
        factor,
        levels = c('N', 'Y'),
        labels = c('No', "Yes"),
        .names = "{.col}_fct"
      )
    ) %>% remove_empty('rows') %>% 
    filter(!is.na(mda_code) & !is.na(f3_2_participant_name))
)

names(form3_2_processed) <- str_replace(names(form3_2_processed), '_2_', '_')
names(form3_2_processed)


cmp_form3 <- (comparedf(form3_1_processed, form3_2_processed, by=c('mda_code','f3_participant_name')))

form3_compare <- summary(comparedf(form3_1_processed, form3_2_processed, by=c('mda_code','f3_participant_name')))

form3_differences_list <- form3_compare$diffs.table
form3_differences_summary <- form3_compare$comparison.summary.table

library(openxlsx)

pacman::p_load("xlsx")

write.csv(form2_differences_list, here('data', 'form2_differences_220607.csv'), row.names=FALSE)
write.csv(form3_differences_list, here('data', 'form3_differences_220607.csv'), row.names=FALSE)

write.xlsx(form2_differences_list %>% as_tibble(), 
           file = here('data', 'Comparing_survey_data_entry_220607.xlsx'), 
           sheetName = "form2_differences",
           rowNames = TRUE)

library(writexl)

list_of_dfs <- list(form2_differences_summary, form3_differences_summary,
                    form2_differences_list)
write_xlsx(list_of_dfs, "out.xlsx")


dataset_names <- list('form2_summary' = form2_differences_summary, 
                      'form3_summary' = form3_differences_summary, 
                      'form2_differences' = form2_differences_list,
                      'form3_differences' = form3_differences_list)
write.xlsx(dataset_names, file = here('data', 'Comparing_survey_data_entry_220607.xlsx'))

writexl::write_xlsx(list(form2_summary = form2_differences_summary,
                         form3_summary = form3_differences_summary,
                         form2_differences = form2_differences_list %>% as_tibble(),
                         form3_differences = form3_differences_list), 
                    here('data', 'Comparing_survey_data_entry_220607.xlsx'))





# Form 3.1 ----------------------------------------------------------------

form3.1_raw <- here('data', 'input', 'Tafea_form 3.1_FOR STATA.xlsx')
form3.1_raw <- read_xlsx(form3.1_raw)


(
  form3.1_processed <- form3.1_raw %>%
    mutate(
      f31_area_council_fct = factor(f31_area_council, levels = area_council_names),
      f31_village_fct = factor(f31_village, levels = c(village_names)),
      across(
        ethanol_f31_vars,
        factor,
        levels = c('N', 'Y'),
        labels = c('No', "Yes"),
        .names = "{.col}_fct"
      )
    )
)




# Form 10 -----------------------------------------------------------------

form10_raw <- here('data', 'input', 'Tafea_form 10_FOR STATA.xlsx')
form10_raw <- read_xlsx(form10_raw)


(
  form10_processed <- form10_raw %>%
    mutate(
      f10_area_council_fct = factor(f10_area_council, levels = area_council_names),
      f10_village_fct = factor(f10_village, levels = c(village_names)),
      across(
        ethanol_f10_vars,
        factor,
        levels = c('N', 'Y'),
        labels = c('No', "Yes"),
        .names = "{.col}_fct"
      )
    )
)



# Form 11 -----------------------------------------------------------------

form11_raw <- here('data', 'input', 'Tafea_form 11_FOR STATA.xlsx')
form11_raw <- read_xlsx(form11_raw)

(
  form11_processed <- form11_raw %>%
    mutate(
      f11_area_council_fct = factor(f11_area_council, levels = area_council_names),
      f11_village_fct = factor(f11_village, levels = c(village_names))
    )
)


# Stool sample list -------------------------------------------------------

tool_list_raw <-
  here('data',
       'input',
       'Tafea_Stool samples sent to Melbourne_FOR STATA.xlsx')
tool_list_raw <- read_xlsx(tool_list_raw)

tool_list_processed <- tool_list_raw



# Data checking -----------------------------------------------------------


library(skimr)
skim(form11_processed)

risk_factor_processed %>% tabyl(roof_tiles_fct)

summary(risk_factor_processed)

risk_factor_processed$washing_water_all_year_time_specify
