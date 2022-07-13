





(risk_factor <- risk_factor_working_data %>% dplyr::select(area_council_fct=risk_factor_area_council_fct, 
                                                   village_fct=risk_factor_village_fct, 
                                                   participant_name=risk_factor_participant_name, 
                                                   mda_code, 
                                                   household_id=risk_factor_household_id) %>% 
  tidyr::separate(mda_code, into=c('province', 'book', 'page', 'line')) %>% 
    mutate(area_council_fct=factor(case_when(area_council_fct=='south_west_t' ~ 'south_west_tanna', 
                                             TRUE ~ as.character(area_council_fct)))) %>% 
  mutate(key=paste(area_council_fct, village_fct, province, book,page,household_id, sep='_')) %>% 
  group_by(key) %>% 
  mutate(count=n()) %>% 
  filter(count==1) %>% 
  mutate(risk_factor_flag=1)
)

risk_factor_hh <- risk_factor %>% group_by(area_council_fct, village_fct) %>% summarise(risk_factor_hh=n())

(form2 <- form2_working_data %>% select(area_council_fct=f2_area_council_fct,
                                       village_fct=f2_village_fct, 
                                       participant_name=f2_participant_name, 
                                       mda_code=mda_code, 
                                       present=f2_present_fct,
                                       consent=f2_consent_fct,
                                       household_id=f2_household_id) %>% 
    tidyr::separate(mda_code, into=c('province', 'book', 'page', 'line')) %>% 
  mutate(key=paste(area_council_fct, village_fct, province, book,page,household_id, sep='_')) %>% 
    mutate(form2_flag=1)
)
form2_ind <- form2 %>% group_by(area_council_fct, village_fct) %>% summarise(form2_ind=n())
form2_hh <- form2 %>% group_by(area_council_fct, village_fct, key) %>% summarise(count=n()) %>% group_by(area_council_fct, village_fct) %>% summarise(form2_hh=n())



form2_risk_factor_hh_merge <- form2 %>% 
  count(area_council_fct, village_fct, key) %>% 
  inner_join(risk_factor,by='key') %>%
  group_by(area_council_fct=area_council_fct.x, village_fct=village_fct.x) %>% 
  summarise(form2_risk_factor_hh=n())

form2_risk_factor_hh_anti_merge <- form2 %>% count(area_council_fct, village_fct, key) %>% 
  anti_join(risk_factor,by='key') %>% pull(key)

form2 %>% filter(key %in% form2_risk_factor_hh_anti_merge) %>% tabyl(present,consent)



unmatched_risk_factor <- form2 %>% full_join(risk_factor,by='key') %>% count(key, risk_factor_flag, form2_flag) %>% filter(is.na(risk_factor_flag) | is.na(form2_flag))

skin_exam_hh <- skin_exam_working_data %>% 
  group_by(area_council_fct=skin_exam_area_council_fct, village_fct=skin_exam_village_fct) %>% 
  summarise(skin_exam_ind=n())

form2_skin_exam_merge <- form2_working_data %>% 
  inner_join(skin_exam_working_data, by=c('mda_code'='mda_code',
                                          'f2_village_fct'='skin_exam_village_fct')) %>% 
  group_by(area_council_fct=skin_exam_area_council_fct, village_fct=f2_village_fct) %>% 
  summarise(skin_exam_form2_merged_ind=n()) 

merge_summary_table <- form2_ind %>% full_join(form2_hh, by=c('area_council_fct', 'village_fct')) %>% 
  full_join(risk_factor_hh, by=c('area_council_fct', 'village_fct')) %>% 
  full_join(form2_risk_factor_hh_merge, by=c('area_council_fct', 'village_fct')) %>% 
  mutate(form2_risk_factor_hh_prop=form2_risk_factor_hh/form2_hh) %>% 
  full_join(skin_exam_hh, by=c('area_council_fct', 'village_fct')) %>% 
  full_join(form2_skin_exam_merge, by=c('area_council_fct', 'village_fct')) %>% 
  mutate(form2_skin_exam_ind_prop=skin_exam_form2_merged_ind/form2_ind) 
  

form2_skin_exam_merged %>% count(f2_sex_fct, skin_exam_sex_fct) 
  filter(f2_sex_fct!=skin_exam_sex_fct)

analysis_data %>%   filter(f2_sex_fct!=skin_exam_sex_fct) %>% select(mda_code, f2_participant_name,f2_sex_fct, skin_exam_participant_name,skin_exam_sex_fct)



analysis_data %>% group_by(key) %>% tidyr::fill(f2_questionnaire_fct) %>% tabyl(f2_questionnaire_fct)






