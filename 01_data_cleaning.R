library(here)
library(tidyverse)
library(linelist)
library(janitor)
library(readxl)
library(tidylog)
library(fuzzyjoin)
library(table1)
library(gtsummary)
source(here('labels_levels.R'))
pacman::p_load(arsenal)


# Risk factor -------------------------------------------------------------

risk_factor_processing_fn <- function() {
  
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
      area_council_fct = factor(str_trim(area_council), levels = area_council_names),
      village_fct = factor(str_trim(tolower(village)), levels = c(village_names)),
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
    ) %>% 
    tidyr::separate(mda_code, into=c('province', 'book', 'page', 'line')) %>% 
    mutate(area_council_fct=factor(case_when(area_council_fct=='south_west_t' ~ 'south_west_tanna', 
                                             TRUE ~ as.character(area_council_fct)))) %>% 
    mutate(key=paste(area_council_fct, village_fct, province, book,page,household_id, sep='_')) %>% 
    mutate(flag=1) %>%
    dplyr::rename_with(~ paste0("risk_factor_", .), -c(key)) 
)

return(risk_factor_processed)
}

risk_factor_working_data <- risk_factor_processing_fn() %>% clean_data()

# Skin exam ---------------------------------------------------------------

skin_exam_processing_fn <- function() {
  
skin_exam_raw <-
  here('data', 'input', 'Tafea_skin exam form_FOR STATA.xlsx')
skin_exam_raw <- read_xlsx(skin_exam_raw)

skin_exam_update_name_mda_code <- readRDS(here('data', 'robjects', 'skin_exam_update_name_mda_code.Rds'))

  skin_exam_processed <- skin_exam_raw %>%
    mutate(
      sex_fct = factor(sex, levels = c('M', 'F'), 
                       labels=c('male', 'female')),
      province_fct = factor(province, levels = province_names),
      area_council_fct = factor(tolower(area_council), levels = area_council_names),
      area_council_fct=factor(case_when(area_council_fct=='south west t' ~ 'south_west_tanna', 
                                               TRUE ~ as.character(area_council_fct))),
      # village=case_when(village=='Ishia' ~ 'Karimasanga', 
      #                   village=='Yakunaus' ~ 'Imarkakak', 
      #                   village=='Loearfi' ~ 'Ipau', 
      #                   village=='Lounapektuan' ~ 'Lounelapen', 
      #                   village=='Loueao' ~ 'Lowkaru', 
      #                   TRUE ~ village),
      village_fct = factor(tolower(str_trim(village)), levels = c(village_names)),
      across(
        skin_varlist,
        factor,
        levels = c('Yes', 'No'),
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
      yaws_ulcer_location_fct = factor(yaws_ulcer_location,
                                       levels = ulcer_location_levels,
                                       labels = ulcer_location_labels),
      yaws_dpp_result_fct = factor(yaws_dpp_result,
                                   levels = dpp_result_levels,
                                   labels = dpp_result_labels)
    ) %>% 
    clean_data() %>% 
    left_join(skin_exam_update_name_mda_code, by=c('participant_name', 'village', 'sex', 'age'))  %>%
    mutate(participant_name = case_when(!is.na(participant_name_upd) ~ participant_name_upd,
                                        TRUE ~ participant_name)) %>%
    mutate(mda_code = case_when(!is.na(mda_code_upd) ~ mda_code_upd,
                                      TRUE ~ mda_code)) %>%
    select(-c(participant_name_upd, mda_code_upd)) %>%
    mutate(mda_code=case_when(mda_code=='9_003_07_139' ~ '9_003_07_13',
                              TRUE ~ mda_code)) %>% 
    select(-c(sex, province, area_council,village, skin_varlist, skin_checklist, yaws_first_ulcer)) %>% 
    mutate(flag=1) %>%
    dplyr::rename_with(~ paste0("skin_exam_", .), -c(mda_code)) %>% 
    mutate(skin_exam_age_group=factor(case_when(skin_exam_age ==0 ~ '<1 YOA',
                                                skin_exam_age %in% c(1:4) ~ 'PSAC 1-4',
                                                skin_exam_age %in% c(5:14) ~ 'SAC 5-14',
                                                skin_exam_age >=15 ~ 'ADULTS >15'), levels=c('<1 YOA','PSAC 1-4', 'SAC 5-14','ADULTS >15'))) %>% 
    ###############SCABIES
    mutate(skin_exam_scabies_lesions_more_10_fct=factor(case_when(
      skin_exam_scabies_typical_lesions_fct=='no' ~ NA_character_, 
      #is.na(skin_exam_scabies_lesions_more_10_fct) ~ 'question not relevant', 
      TRUE ~ as.character(skin_exam_scabies_lesions_more_10_fct)), 
      levels=c('yes', 'no')))  %>% 
    mutate(skin_exam_scabies_skin_infection_fct=factor(case_when(
      skin_exam_scabies_typical_lesions_fct=='no' ~ 'no scabies lesions observed', 
      #is.na(skin_exam_scabies_skin_infection_fct) ~ 'question not relevant', 
      TRUE ~ as.character(skin_exam_scabies_skin_infection_fct)), levels=c('yes', 'no', 'no scabies lesions observed')))  %>% 
    mutate(skin_exam_yaws_saw_lesion_fct=factor(case_when(skin_exam_yaws_self_report_fct=='yes' ~ NA_character_,
                                                          TRUE ~as.character(skin_exam_yaws_saw_lesion_fct)), levels=c('yes', 'no'))) %>% 
    mutate(skin_exam_yaws_dpp_result_fct=factor(skin_exam_yaws_dpp_result_fct, levels=c('positive_active_yaws',
                                                                                        'old_treated_yaws', 
                                                                                        'negative_no_yaws', 
                                                                                        'false_positive'))) %>% 
    mutate(skin_exam_leprosy_saw_lesion_fct = case_when(skin_exam_leprosy_self_report_fct=='yes' ~ 'leprosy lesion self-reported',
                                                        skin_exam_leprosy_saw_lesion_fct=='yes' ~ 'yes', 
                                                        skin_exam_leprosy_suspected_fct=='yes' ~ 'yes',
                                                        skin_exam_leprosy_saw_lesion_fct=='no' ~ 'no')) %>% 
    mutate(skin_exam_leprosy_saw_lesion_fct=factor(skin_exam_leprosy_saw_lesion_fct, levels=c('yes', 'no', 'leprosy lesion self-reported'))) %>% 
    mutate(skin_exam_leprosy_suspected_fct=factor(if_else((skin_exam_leprosy_self_report_fct=='no' & 
                                                             skin_exam_leprosy_saw_lesion_fct =='no'),
                                                          'no leprosy lesion detected', 
                                                          as.character(skin_exam_leprosy_suspected_fct)))) %>% 
    mutate(skin_exam_leprosy_suspected_fct=factor(skin_exam_leprosy_suspected_fct, levels=c('yes', 'no', 'no leprosy lesion detected'))) %>% 
    mutate(across(c(skin_exam_ssd_none_fct,
                    skin_exam_ssd_abscess_boil_fct,
                    skin_exam_ssd_cellulitis_fct ,
                    skin_exam_ssd_crusted_scabies,
                    skin_exam_ssd_other), factor, levels=c('checked', 'unchecked'))) %>% 
    mutate(skin_exam_ssd_other_specify=factor(if_else(skin_exam_ssd_other=='unchecked', 'no other skin disease suspected', 
                                                      as.character(skin_exam_ssd_other_specify)), 
                                              levels=c('rash', 'ringworm', 'no other skin disease suspected')))
  

 
return(skin_exam_processed)
}

skin_exam_working_data <- skin_exam_processing_fn() 
saveRDS(skin_exam_working_data, here('skin_exam_data.Rds'))

# DBS ---------------------------------------------------------------------

dbs_processing_fn <- function() {
dbs_raw <-
  here('data', 'input', 'Tafea_DBS sample IDs_FOR STATA.xlsx')
dbs_raw <- read_xlsx(dbs_raw)
dbs_processed <- dbs_raw  %>% clean_data()
return(dbs_processed)
}

dbs_working_data <- dbs_processing_fn()

# Skin swab ---------------------------------------------------------------

skin_swab_processing_fn <- function() {
  
skin_swab_raw <-
  here('data', 'input', 'sore swab DBS samples.xlsx')
skin_swab_raw <- read_xlsx(skin_swab_raw, sheet='Sore swap reshaped') 

skin_swab_processed <- skin_swab_raw %>% clean_data()

return(skin_swab_processed)
}

skin_swab_working_data <- skin_swab_processing_fn()

# Form 2 ------------------------------------------------------------------


form2_processing_fn <- function() {
  
form2_1_raw <-
  here('data',
       'input',
       'Tafea_form 2_data entry person 1_FOR STATA.xlsx')
form2_1_raw <- read_xlsx(form2_1_raw)

  form2_1_processed <- form2_1_raw %>%
    mutate(
      f2_1_sex_fct = factor(
        f2_1_sex,
        levels = c('M', 'F'),
        labels = c("Male", "Female")
      ),
      f2_1_area_council_fct = factor(tolower(f2_1_area_council), levels = c('aneityum', 
                                                                            'south west tanna', 
                                                                            'whitesands', 
                                                                            'south tanna', 
                                                                            'middle bush', 
                                                                            'futuna', 
                                                                            'west tanna')),
      f2_1_village_fct = factor(tolower(f2_1_village), levels = c(village_names)),
      across(
        form2_1_yesno,
        factor,
        levels = c('Y', 'N'),
        labels = c('Yes', "No"),
        .names = "{.col}_fct"
      ),
      data_entry_person=1
    )  %>% 
    select(-c(f2_1_sex, f2_1_area_council, f2_1_village, 
              all_of(form2_1_yesno))) %>% 
    clean_data() %>% 
    remove_empty('rows') %>% 
    filter(!is.na(mda_code) & !is.na(f2_1_participant_name))

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
      f2_2_area_council_fct = factor(tolower(f2_2_area_council), levels = c('aneityum', 
                                                                            'south west tanna', 
                                                                            'whitesands', 
                                                                            'south tanna', 
                                                                            'middle bush', 
                                                                            'futuna', 
                                                                            'west tanna')),
      f2_2_village_fct = factor(tolower(f2_2_village), levels = c(village_names)),
      across(
        form2_2_yesno,
        factor,
        levels = c('Y', 'N'),
        labels = c('Yes', "No"),
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

form2_combine_fill <- form2_1_processed %>% 
  mutate(data_entry=1) %>% 
  bind_rows(form2_2_processed %>% mutate(data_entry=2)) %>% 
  group_by(mda_code,f2_participant_name) %>% 
  arrange(mda_code, f2_participant_name) %>% 
  fill(everything(), .direction = "downup") %>% 
  mutate(f2_flag=1) %>% 
  # mutate(f2_consent_fct=factor(case_when(f2_consent_fct=='yes' & f2_present_fct== 'yes' ~ 'yes-present',
  #                                 f2_consent_fct=='yes' & f2_present_fct== 'no' ~ 'yes-not present',
  #                                 f2_consent_fct=='yes' & is.na(f2_present_fct) ~ 'yes-missing',
  #                                 f2_consent_fct=='no' & f2_present_fct== 'yes' ~ 'no-present',
  #                                 f2_consent_fct=='no' & f2_present_fct== 'no' ~ 'no-not present',
  #                                 f2_consent_fct=='no' & is.na(f2_present_fct) ~ 'no-missing'), 
  #                              levels=c('yes-present', 'yes-not present','yes-missing',
  #                                       'no-present', 'no-not present', 'no-missing'))) %>% 
  # mutate(f2_stool_container_fct=factor(case_when(f2_stool_container_fct=='yes' & f2_present_fct== 'yes' ~ 'yes-present',
  #                                                f2_stool_container_fct=='yes' & f2_present_fct== 'no' ~ 'yes-not present',
  #                                                f2_stool_container_fct=='yes' & is.na(f2_present_fct) ~ 'yes-missing',
  #                                                f2_stool_container_fct=='no' & f2_present_fct== 'yes' ~ 'no-present',
  #                                                f2_stool_container_fct=='no' & f2_present_fct== 'no' ~ 'no-not present',
  #                                                f2_stool_container_fct=='no' & is.na(f2_present_fct) ~ 'no-missing'), 
  #                              levels=c('yes-present', 'yes-not present','yes-missing',
  #                                       'no-present', 'no-not present', 'no-missing'))) %>% 
  # mutate(across(c(f2_stool_container_fct,
  #                 f2_questionnaire_fct,
  #                 f2_stool_sample_fct,
  #                 f2_leprosy_suspected_fct,
  #                 f2_yaws_suspected_fct,
  #                 f2_ssd_suspected_fct), ~ factor(case_when(f2_consent_fct=='no' ~ 'consent not obtained',
  #                                                           TRUE ~ as.character(.)), 
  #                                                 levels=c('yes', 'no', 'consent not obtained')))) %>% 
 # mutate(f2_age_group = age_group_fn(f2_age)) %>% 
  mutate(f2_age_group=factor(case_when(f2_age ==0 ~ '<1 YOA',
                                       f2_age %in% c(1:4) ~ 'PSAC 1-4',
                                       f2_age %in% c(5:14) ~ 'SAC 5-14',
                                       f2_age >=15 ~ 'ADULTS >15', 
                                       TRUE ~ 'AGE MISSING'), levels=c('<1 YOA','PSAC 1-4', 'SAC 5-14','ADULTS >15', 'AGE MISSING'))) %>% 
  # mutate(f2_sex_fct=case_when(!is.na(f2_sex_fct) ~ 'Sex missing', 
  #                             TRUE ~ f2_sex_fct)) %>% 
  #mutate(f2_sex_fct=fct_explicit_na(f2_sex_fct, 'Sex missing')) %>% 
  #mutate(f2_age_group=fct_explicit_na(f2_age_group, 'Age missing')) %>% 
  mutate(f2_azith_any_fct = case_when(f2_azith_1tab_fct == 'yes' ~ 'yes',
                                      f2_azith_2tab_fct == 'yes' ~ 'yes', 
                                      f2_azith_3tab_fct == 'yes' ~ 'yes')) %>% 
  mutate(f2_azith_any_fct = case_when((is.na(f2_azith_any_fct) & f2_flag==1) ~ 'no', 
                                      TRUE ~ f2_azith_any_fct)) %>% 
  mutate(f2_alb_any_fct = case_when(f2_alb_0_5tab_fct == 'yes' ~ 'yes',
                                    f2_alb_sac_1tab_fct == 'yes' ~ 'yes', 
                                    f2_alb_wcba_1tab_fct == 'yes' ~ 'yes')) %>% 
  mutate(f2_alb_any_fct = case_when((is.na(f2_alb_any_fct) & f2_flag==1) ~ 'no', 
                                    TRUE ~ f2_alb_any_fct)) %>% 
  mutate(f2_ivm_any_fct = case_when(f2_ivm_1tab_fct == 'yes' ~ 'yes',
                                    f2_ivm_2tab_fct == 'yes' ~ 'yes', 
                                    f2_ivm_3tab_fct == 'yes' ~ 'yes',
                                    f2_ivm_4tab_fct == 'yes' ~ 'yes')) %>% 
  mutate(f2_ivm_any_fct = case_when((is.na(f2_ivm_any_fct) & f2_flag==1) ~ 'no', 
                                    TRUE ~ f2_ivm_any_fct)) %>% 
  mutate(across(f2_azith_any_fct:f2_ivm_any_fct, factor, levels=c('yes', 'no'))) %>% 
  mutate(f2_pm_any_fct = case_when(f2_pm_2m_fct == 'yes' ~ 'yes',
                                   f2_pm_2m_12yr_fct == 'yes' ~ 'yes',
                                   f2_pm_other_fct == 'yes' ~ 'yes')) %>% 
  mutate(f2_pm_any_fct = case_when(f2_ivm_any_fct=='yes' & f2_pm_any_fct=='yes' ~ 'yes-received ivermectin',
                                   is.na(f2_pm_any_fct) & (f2_flag==1 & f2_ivm_any_fct=='no') ~ 'no',
                                   f2_ivm_any_fct=='yes' & f2_pm_any_fct=='no' ~ 'no-received ivermectin',
                                   TRUE ~ f2_pm_any_fct)) %>% 
  mutate(f2_pm_any_fct=factor(f2_pm_any_fct, levels=c('yes','yes-received ivermectin',
                                                      'no-received ivermectin', 'no')))

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
  
  #saveRDS(name_cleaning_fuzzy, here('data', 'robjects', 'name_updates.Rds'))
}

correct_mda_fn <- function() {
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
write.csv(form2_differences_list, here('data', 'form2_differences_list.csv'))
form2_differences_summary <- form2_compare$comparison.summary.table
write.csv(form2_differences_summary, here('data', 'form2_differences_summmary.csv'))



return(form2_1_processed_upd)
}

form2_working_data <- form2_processing_fn() %>% ungroup()

saveRDS(form2_working_data, here('form2_working_data.Rds'))


# Form 3 ------------------------------------------------------------------

form3_processing_fn <- function(){
form3_1_raw <-
  here('data',
       'input',
       'Tafea_form 3_data entry person 1_FOR STATA.xlsx')
form3_1_raw <- read_xlsx(form3_1_raw)


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
    filter(!is.na(mda_code) & !is.na(f3_1_participant_name)) %>% 
  clean_data()

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
) %>% clean_data()

names(form3_2_processed) <- str_replace(names(form3_2_processed), '_2_', '_')
names(form3_2_processed)

return(form3_1_processed)
}

form3_working_data <- form3_processing_fn() %>% mutate(f2_flag=1)

# Form 3.1 ----------------------------------------------------------------

form3.1_processing_fn<- function(){
  
form3.1_raw <- here('data', 'input', 'Tafea_form 3.1_FOR STATA.xlsx')
form3.1_raw <- read_xlsx(form3.1_raw)

form3.1_processed <- form3.1_raw %>%
  mutate(
    f31_area_council_fct = factor(
      tolower(f31_area_council),
      levels = c(
        'aneityum',
        'south west tanna',
        'whitesands',
        'south tanna',
        'middle bush',
        'futuna',
        'west tanna'
      )
    ),
    f31_village = case_when(f31_village=='Imarkak' ~ 'Imarkakak', 
                            TRUE ~ f31_village),
    f31_village_fct = factor(tolower(f31_village), levels = c(village_names)),
    across(
      ethanol_f31_vars,
      factor,
      levels = c('N', 'Y'),
      labels = c('No', "Yes"),
      .names = "{.col}_fct"
    )) %>%
  clean_data() %>% 
  mutate(f31_flag=1)

return(form3.1_processed)
}

form3.1_working_data <- form3.1_processing_fn() 


form3_3.1_merged <- form3_working_data %>% left_join(form3.1_working_data, 
                                 by='mda_code')


# Form 10 -----------------------------------------------------------------

form10_processing_fn <- function() {
form10_raw <- here('data', 'input', 'Tafea_form 10_FOR STATA.xlsx')
form10_raw <- read_xlsx(form10_raw)


  form10_processed <- form10_raw %>%
    clean_data() %>% 
    mutate(f10_village = case_when(f10_village=='imarkak' ~ 'imarkakak', 
                                   TRUE ~ f10_village)) %>% 
    mutate(
      f10_area_council_fct = factor(f10_area_council, levels = c('aneityum', 
                                                                 'south_west_tanna', 
                                                                 'whitesands', 
                                                                 'south_tanna', 
                                                                 'middle_bush', 
                                                                 'futuna', 
                                                                 'west_tanna')),
      f10_village_fct = factor(f10_village, levels = (lower_village_names)),
      across(
        f10_ethanol,
        factor,
        levels = c('n', 'y'),
        labels = c('no', "yes"),
        .names = "{.col}_fct"
      )
    ) %>% 
    mutate(f10_flag=1) %>% 
    mutate(drop_dup=case_when((f10_village_fct=='imarkakak' & mda_code=='9_003_03_11') ~ 1)) %>% 
   # filter(!drop_dup==1) %>% 
    group_by(mda_code) %>% 
    mutate(myorder = 1:n()) %>% 
    filter(myorder==1) 
    #select(-c(count,drop_dup))
  
  
  

return(form10_processed)
}

form10_working_data <- form10_processing_fn()

# Form 11 -----------------------------------------------------------------

form11_processing_fn <- function() {
form11_raw <- here('data', 'input', 'Tafea_form 11_FOR STATA.xlsx')
form11_raw <- read_xlsx(form11_raw)

form11_processed <- form11_raw %>%
  clean_data() %>% 
  mutate(
    f11_area_council_fct = factor(f11_area_council, levels = c('aneityum', 
                                                               'south_west_tanna', 
                                                               'whitesands', 
                                                               'south_tanna', 
                                                               'middle_bush', 
                                                               'futuna', 
                                                               'west_tanna')),
    f11_village_fct = factor(f11_village, levels = (lower_village_names)),
    results_flag=1
  ) %>% 
  # mutate(mda_code=ifelse((mda_code=='9_003_03_11' &  f11_village_fct=='imarkak'), NA, 
  #                        mda_code)) %>% 
  # mutate(mda_code=ifelse((mda_code=='9_004_14_16' &  f11_village_fct=='lownasunan'), NA, 
  #                        mda_code)) %>% 
  # mutate(mda_code=case_when(mda_code=='8_001_2_01' ~ '8_001_20_01', 
  #                           TRUE ~ mda_code)) %>% 
  # mutate(mda_code=case_when((f11_village=='imaru' & mda_code =='8_001_01_13') ~ '8_001_01_03', 
  #                           (f11_village=='koraioken' & mda_code =='8_002_25_09') ~ '8_001_26_09', 
  #                           (f11_village=='yakunaus' & mda_code =='9_003_02_10') ~ '9_004_02_10', 
  #                           TRUE ~ mda_code)) %>% 
  group_by(mda_code) %>% 
  mutate(count = n()) %>% 
 # filter(count==1) %>% 
  #select(-count) %>% 
  dplyr::rename_with(~ paste0("f11_", .), -c(1:7,26:27))


return(form11_processed)
}

form11_working_data <- form11_processing_fn()

# Stool sample list -------------------------------------------------------

stool_sample_processing_fn <- function() {
stool_list_raw <-
  here('data',
       'input',
       'Tafea_Stool samples sent to Melbourne_FOR STATA.xlsx')
stool_list_raw <- read_xlsx(stool_list_raw)

stool_list_processed <- stool_list_raw %>% clean_data()

return(stool_list_processed)
}

stool_sample_working_data <- stool_sample_processing_fn()


# qPCR results ------------------------------------------------------------

qPCR_results_processing_fn <- function() {

qPCR_results_raw <-
  here('data',
       'input',
       'Tafea_Vanuatu_STH qPCR results.xlsx')

qPCR_results_raw <- read_xlsx(qPCR_results_raw,  
                              sheet='reshaped')

qPCR_results_processed <- qPCR_results_raw %>% 
    clean_data() %>% 
    mutate(across(human_ct1:human_ct2, as.character)) %>% 
    pivot_longer(-c(1:4)) %>% 
    mutate(test_pos_neg=case_when(value=='neg' ~ 'neg',
                                  sample_available=='not_available' ~ 'sample_not_available',
                                  TRUE ~ 'pos')) %>% 
    # mutate(test_pos_neg=case_when(!is.na(remark) ~ paste(test_pos_neg, remark, sep='_'),
    #                               TRUE ~ test_pos_neg)) %>% 
    mutate(value_integer=str_replace(value, 'neg', '')) %>% 
    mutate(value_integer=str_replace(value_integer, '_', '.')) %>% 
    mutate(value_integer=as.numeric(value_integer)) %>% 
    select(unimelb_id, mda_code=sample_id, pathogen=name, result=test_pos_neg, ct_value=value_integer) %>% 
    mutate(ct_value = case_when((mda_code == "8_001_01_09" & pathogen == "trichuris_ct2") ~ 35.64, 
                                TRUE ~ ct_value)) %>% 
  mutate(results_flag =1 ) 

qPCR_results_processed_wide <- qPCR_results_processed %>% 
  pivot_wider(names_from=pathogen, values_from=c('result', 'ct_value')) %>% 
  dplyr::rename_with(~ paste0("qPCR_", .), -mda_code)


return(qPCR_results_processed_wide)
}

qpcr_results_working_data <- qPCR_results_processing_fn()

# Data checking -----------------------------------------------------------

# Data merging ------------------------------------------------------------
############### QUESTIONNAIRES 

#### Merge form 2 and skin exam data
# Correct some names and mda codes in skin exam data 
correct_skin_data_fn <- function() {
  form2_skin_exam_anti_join <- skin_exam_working_data %>% anti_join(form2_working_data, by='mda_code', 
                                                                    'skin_exam_participant_name' = 'f2_participant_name') %>% 
    select(mda_code, skin_exam_participant_name, skin_exam_village,skin_exam_sex,skin_exam_age)
  
  skin_exam_update_name_mda_code <- form2_working_data %>% select(mda_code, f2_participant_name) %>% 
    ungroup() %>% 
    stringdist_inner_join(
      form2_skin_exam_anti_join,
      by = c("f2_participant_name" = "skin_exam_participant_name"),
      max_dist = 3,
      distance_col = "distance"
    ) %>% 
    #filter(distance!=0) %>% 
    mutate(participant_name_upd = case_when(distance %in% c(0:2) ~ f2_participant_name)) %>% 
    mutate(mda_code_upd = case_when(distance %in% c(0:2) ~ mda_code.x)) %>% 
    select(participant_name=skin_exam_participant_name, 
           village=skin_exam_village,
           sex=skin_exam_sex,
           age=skin_exam_age, 
           participant_name_upd, 
           mda_code_upd) %>% 
    filter(!is.na(participant_name_upd))
  
  #saveRDS(skin_exam_update_name_mda_code, here('data', 'robjects', 'skin_exam_update_name_mda_code.Rds'))
}

form2_skin_exam_merged <- form2_working_data %>% 
  full_join(skin_exam_working_data, by=c('mda_code', 
                                 #'f2_participant_name' = 'skin_exam_participant_name',
            #'f2_sex_fct' = 'skin_exam_sex_fct',
            'f2_village_fct' = 'skin_exam_village_fct'))

### 1192 matches, 302 in x, 18 in y 

#### Merge swab data to form2-skin exam data

form2_skin_exam_merged %>% inner_join(skin_swab_working_data, by=c('mda_code' = 'sore_swap_samples'))

#### 0 matches 

######### DIAGNOSTICS

######Merge stool sample data with qPCR results

stool_receipt_qpcr_results <- stool_sample_working_data %>% 
  full_join(qpcr_results_working_data, by='mda_code')

# 1 record from qPCR results file does not merge, but this is the record marked as "sample not available"
# There is 1 record (mda code 8_001_01_19) that has a "0" for sample_yn in  stool_sample_working_data, but has results in qpcr_results_working_data 

# Merge form 3.1 and 10

form3.1_10_merge <- form3.1_working_data %>% 
  full_join(form10_working_data, by='mda_code') %>% 
  filter(is.na(f10_flag) | is.na(f31_flag)) %>% 
  mutate(village=coalesce(f31_village_fct, f10_village_fct)) %>% 
  mutate(area_council=coalesce(f31_area_council_fct,f10_area_council_fct )) %>% 
  select(mda_code, village, area_council, f10_flag, f31_flag) %>% 
  arrange(area_council,village,mda_code)

# Merge form 10 and 11

form3.1_10_11_merge <- form3.1_10_merge %>% 
  full_join(form11_working_data, by='mda_code')

# mda_id_sample_merge <- form3.1_working_data %>% mutate(mda_code_3.1=mda_code) %>% 
#   full_join(form10_working_data %>% mutate(mda_code_10=mda_code), by='mda_code') %>% 
#   full_join(form11_working_data %>% mutate(mda_code_11=mda_code), by='mda_code') %>% 
#   full_join(qpcr_results_working_data %>% mutate(mda_code_qpcr=mda_code),by='mda_code') %>% 
#   select(contains('mda_code'))
#   
# 
# mda_id_sample_merge %>% pivot_longer(-mda_code) %>% filter(!is.na(value)) %>% 
#   count(mda_code)

##### Merge with form 11 for SNF results
stool_receipt_qpcr_results_form11 <-  stool_receipt_qpcr_results %>%
 filter(sample_yn == 1 | qPCR_results_flag == 1) %>%
  full_join(form11_working_data, by = 'mda_code') %>% 
  select(qPCR_unimelb_id,
    mda_code,
    f11_area_council_fct,
    f11_village_fct,
    sample_yn,
    qPCR_results_flag,
    f11_results_flag,
    contains('ascaris'),
    contains('trichuris'),
    contains('strongyloides'),
    contains('hookworm'),
    contains('other_sth'),
    contains('necator'),
    contains('a_cey'),
    contains('a_duod'),
    contains('human')
  ) %>% 
  mutate(f11_result_ascaris=case_when(f11_ascaris_epg>0 ~ 'pos',
                                      f11_ascaris_epg==0 ~ 'neg',
                                      TRUE ~ 'no result')) %>% 
  mutate(f11_result_trichuris=case_when(f11_trichuris_epg>0 ~ 'pos',
                                        f11_trichuris_epg==0 ~ 'neg',
                                        TRUE ~ 'no result')) %>% 
  mutate(f11_result_hookworm=case_when(f11_hookworm_epg>0 ~ 'pos', 
                                       f11_hookworm_epg==0 ~ 'neg',
                                       TRUE ~ 'no result')) %>% 
  mutate(qPCR_result_hookworm = case_when(rowSums(across(c('qPCR_result_necator_ct1', 'qPCR_result_necator_ct2', 
                                                        'qPCR_result_a_cey_ct1', 'qPCR_result_a_cey_ct2', 
                                                        'qPCR_result_a_duod_ct1', 'qPCR_result_a_duod_ct2'), ~ .x == "pos"), 
                                               na.rm = T)>0 ~ 'pos', 
                                       #   is.na(qPCR_results_flag) ~ 'no qPCR match',
                                          qPCR_result_necator_ct1== 'sample_not_available' ~ 'sample_not_available', 
                                          TRUE ~ 'neg')) 

### 10 records from qPCR don't match to form 11
### 18 records from form 11 don't match to qPCR
### There are 6 records with duplicate mda_code in form 11. 2 have been recoded to NA temporarily. 2 have been dropped


merged_data_all <- form2_skin_exam_merged %>% 
#   left_join(form3_working_data, by=c('mda_code',
#                                      'f2_participant_name' = 'f3_participant_name'))
  full_join(form3.1_working_data, by=c('mda_code')) %>%
  # left_join(skin_exam_working_data, by=c('mda_code', 
  #                                        'f2_participant_name' = 'participant_name')) %>% 
  # left_join(dbs_working_data, by=c('mda_code')) %>% 
  full_join(form10_working_data, by=c('mda_code')) %>%
 #full_join(form11_working_data, by=c('mda_code')) %>% 
  full_join(stool_receipt_qpcr_results_form11, by=c('mda_code')) %>% 
  mutate(mda_code_copy=mda_code) %>% 
  tidyr::separate(mda_code, into=c('province', 'book', 'page', 'line')) %>% 
  mutate(key=paste(f2_area_council_fct, f2_village_fct, province, book,page,f2_household_id, sep='_')) %>% 
  full_join(risk_factor,by='key') %>% 
  rename(mda_code=mda_code_copy) %>% 
  ungroup()

#### Diagnostics
mda_id_sample_merge <- form3.1_working_data %>% 
  mutate(mda_code_3.1=mda_code) %>% 
  full_join(form10_working_data %>% mutate(mda_code_10=mda_code), by='mda_code') %>% 
  full_join(form11_working_data %>% mutate(mda_code_11=mda_code), by='mda_code') %>% 
  full_join(qpcr_results_working_data %>% mutate(mda_code_qpcr=mda_code),by='mda_code') %>% 
  select(contains('mda_code'))



# Cleaning analysis dataset -----------------------------------------------



analysis_data <- merged_data_all 
  
saveRDS(analysis_data, here('analysis_data.Rds'))
  
 

# x %>% tabyl( skin_exam_yaws_suspected_fct, skin_exam_yaws_saw_lesion_fct)
# 
# x <- merged_data_all %>% filter(skin_exam_yaws_dpp_result_fct %in% c('positive_active_yaws', 'negative_no_yaws'))
# 
# table(skin_exam_working_data$skin_exam_yaws_village_arrive_6m_fct)
#   # mutate(skin_exam_scabies_typical_lesions_fct = ifelse(skin_exam_scabies_scratching_24_fct=='no', 
#   #                                                       'no scabies lesions detected',
#   #                                                       skin_exam_scabies_typical_lesions_fct ))
#   
#                                     
# mutate(Revenue = replace(Revenue, is.na(Revenue) & Date <= max.date, 0))

