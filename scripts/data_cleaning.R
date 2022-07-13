
# FORM 2 CORRECTIONS ------------------------------------------------------

f2_present_update <- analysis_data %>% filter(is.na(f2_present_fct) & f2_flag==1) %>% 
  mutate(f2_present_fct_update=factor(case_when(f10_flag==1 | f11_results_flag==1 | skin_exam_flag==1 ~ 'yes', 
                                         TRUE ~ as.character(f2_present_fct)), 
                               levels=c('yes', 'no'))) %>% 
  dplyr::select(mda_code, f2_present_fct_update) %>% 
  filter(!is.na(f2_present_fct_update))

f2_consent_update <- analysis_data %>% filter((is.na(f2_consent_fct) | f2_consent_fct=='no') & f2_flag==1) %>% 
  mutate(f2_consent_fct_update=factor(case_when(f10_flag==1 | f11_results_flag==1 | skin_exam_flag==1 ~ 'yes', 
                                                TRUE ~ as.character(f2_consent_fct)), 
                                      levels=c('yes', 'no'))) %>% 
  dplyr::select(mda_code, f2_consent_fct_update) %>% 
  filter(!is.na(f2_consent_fct_update))

f2_questionnaire_update <- analysis_data %>% 
  filter((is.na(f2_questionnaire_fct) | f2_questionnaire_fct=='no') & f2_flag==1) %>% 
  mutate(f2_questionnaire_fct_update=factor(case_when(risk_factor_flag==1 ~ 'yes', 
                                                      TRUE ~ as.character(f2_questionnaire_fct)), 
                                            levels=c('yes', 'no'))) %>% 
  dplyr::select(mda_code, f2_questionnaire_fct) %>% 
  filter(!is.na(f2_questionnaire_fct))
#### Nothing changes, recode missings as missing


form2_working_data <- form2_working_data %>% 
  mutate(f2_sex_fct=factor(case_when(f2_participant_name=='masaka_natonga' & f2_age==53 ~ 'male', 
                              TRUE ~ as.character(f2_sex_fct)), 
                           levels = c("male", "female"))) %>% 
  mutate(f2_age = case_when(f2_participant_name=='sterol_ansen' & f2_sex_fct=='male' ~ 3,
                            f2_participant_name=='nawau_joseph' & f2_sex_fct=='male' ~ 30,
                            f2_participant_name=='naiwa_roroveka' & f2_sex_fct=='male' ~ 49,
                            TRUE ~ f2_age)) %>% 
  left_join(f2_present_update, by='mda_code') %>% 
  mutate(f2_present_updated=coalesce(f2_present_fct_update, f2_present_fct)) %>% 
  mutate(f2_present_updated=fct_explicit_na(f2_present_updated, na_level = "Missing")) %>% 
  left_join(f2_consent_update, by='mda_code') %>% 
  mutate(f2_consent_updated=coalesce(f2_consent_fct_update, f2_consent_fct)) %>% 
  mutate(f2_consent_updated=fct_explicit_na(f2_consent_updated, na_level = "Missing")) %>% 
  mutate(f2_questionnaire_fct=fct_explicit_na(f2_questionnaire_fct, na_level = "Missing"))




# SKIN EXAM CORRECTIONS ---------------------------------------------------

