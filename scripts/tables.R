scabies_data <- skin_exam_data_wd %>%
  filter(skin_exam_flag == 1) %>%
  #filter(f2_present_fct == 'yes') %>%
  dplyr::select(
    skin_exam_age_group,
    skin_exam_scabies_scratching_24_fct,
    skin_exam_scabies_typical_lesions_fct,
    skin_exam_scabies_lesions_more_10_fct,
    skin_exam_scabies_skin_infection_fct,
    skin_exam_scabies_finished_fct
  )


scabies_data %>% 
  count(skin_exam_age_group, skin_exam_scabies_scratching_24_fct) %>% 
  group_by(skin_exam_age_group) %>% 
  mutate(total=sum(n)) %>% 
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>% 
  unnest(prop) %>% 
  select(1:5, conf.low, conf.high)

scabies_questions <- c('skin_exam_scabies_scratching_24_fct',
                       'skin_exam_scabies_typical_lesions_fct',
                       'skin_exam_scabies_lesions_more_10_fct',
                       'skin_exam_scabies_skin_infection_fct',
                       'skin_exam_scabies_finished_fct')

scabies_results_tbl <- skin_exam_data_wd %>%
  filter(skin_exam_flag == 1) %>%
  #filter(f2_present_fct == 'yes') %>%
  dplyr::select(
    skin_exam_age_group,
    skin_exam_scabies_scratching_24_fct,
    skin_exam_scabies_typical_lesions_fct,
    skin_exam_scabies_lesions_more_10_fct,
    skin_exam_scabies_skin_infection_fct,
    skin_exam_scabies_finished_fct) %>% pivot_longer(-skin_exam_age_group) %>% 
  count(skin_exam_age_group, name,value) %>% 
  group_by(skin_exam_age_group, name) %>% 
  mutate(total=sum(n))  %>% 
  ungroup() %>%
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% 
  select(1:6, conf.low, conf.high) %>% clean_names() %>% 
  rename(question=name, response=value) %>% 
pivot_longer(-c(skin_exam_age_group, question, response)) %>% 
  mutate(response=fct_explicit_na(response, na_level="Missing")) %>% 
  pivot_wider(names_from=c(skin_exam_age_group,name), values_from=value) %>% 
  clean_names() %>% 
  arrange(question, response)


skin_exam_summary_table_fn <- function() {
skin_exam_results_age_group_tbl <- skin_exam_data_wd %>%
  filter(skin_exam_flag == 1) %>%
  dplyr::select(
    skin_exam_age_group,
    skin_exam_scabies_scratching_24_fct,
    skin_exam_scabies_typical_lesions_fct,
    skin_exam_scabies_lesions_more_10_fct,
    skin_exam_scabies_skin_infection_fct,
    skin_exam_scabies_finished_fct, 
    skin_exam_yaws_self_report_fct,
    skin_exam_yaws_saw_lesion_fct,
    skin_exam_yaws_suspected_fct,
    skin_exam_yaws_first_ulcer_fct ,
    skin_exam_yaws_previous_treatment_fct ,
    skin_exam_yaws_ulcer_location_fct ,
    skin_exam_yaws_dpp_result_fct ,
    skin_exam_yaws_dpp_line1_fct ,
    skin_exam_yaws_dpp_line2_fct ,
    skin_exam_yaws_dpp_linec_fct ,
    skin_exam_yaws_swab_collected_fct ,
    skin_exam_yaws_finished_fct, 
    skin_exam_leprosy_self_report_fct,
    skin_exam_leprosy_saw_lesion_fct,
    skin_exam_leprosy_suspected_fct, 
    skin_exam_ssd_abscess_boil_fct,
    skin_exam_ssd_cellulitis_fct,
    skin_exam_ssd_crusted_scabies,
    skin_exam_ssd_other,
    skin_exam_ssd_other_specify) %>% 
  pivot_longer(-skin_exam_age_group) %>% 
  count(skin_exam_age_group, name,value) %>% 
  group_by(skin_exam_age_group, name) %>% 
  mutate(total=sum(n))  %>% 
  ungroup() %>%
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% 
  select(1:6, conf.low, conf.high) %>% clean_names() %>% 
  rename(question=name, response=value) %>% 
  pivot_longer(-c(skin_exam_age_group, question, response)) %>% 
  mutate(response=fct_explicit_na(response, na_level="Missing")) %>% 
  pivot_wider(names_from=c(skin_exam_age_group,name), values_from=value) %>% 
  clean_names() %>% 
  arrange(question, response)

skin_exam_results_tbl <- skin_exam_data_wd %>%
  filter(skin_exam_flag == 1) %>%
  mutate(skin_exam_age_group='OVERALL') %>% 
  dplyr::select(
    skin_exam_age_group,
    skin_exam_scabies_scratching_24_fct,
    skin_exam_scabies_typical_lesions_fct,
    skin_exam_scabies_lesions_more_10_fct,
    skin_exam_scabies_skin_infection_fct,
    skin_exam_scabies_finished_fct, 
    skin_exam_yaws_self_report_fct,
    skin_exam_yaws_saw_lesion_fct,
    skin_exam_yaws_suspected_fct,
    skin_exam_yaws_first_ulcer_fct ,
    skin_exam_yaws_previous_treatment_fct ,
    skin_exam_yaws_ulcer_location_fct ,
    skin_exam_yaws_dpp_result_fct ,
    skin_exam_yaws_dpp_line1_fct ,
    skin_exam_yaws_dpp_line2_fct ,
    skin_exam_yaws_dpp_linec_fct ,
    skin_exam_yaws_swab_collected_fct ,
    skin_exam_yaws_finished_fct, 
    skin_exam_leprosy_self_report_fct,
    skin_exam_leprosy_saw_lesion_fct,
    skin_exam_leprosy_suspected_fct, 
    skin_exam_ssd_abscess_boil_fct,
    skin_exam_ssd_cellulitis_fct,
    skin_exam_ssd_crusted_scabies,
    skin_exam_ssd_other,
    skin_exam_ssd_other_specify) %>% 
  pivot_longer(-skin_exam_age_group) %>% 
  count(skin_exam_age_group, name,value) %>% 
  group_by(skin_exam_age_group, name) %>% 
  mutate(total=sum(n))  %>% 
  ungroup() %>%
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% 
  select(1:6, conf.low, conf.high) %>% clean_names() %>% 
  rename(question=name, response=value) %>% 
  pivot_longer(-c(skin_exam_age_group, question, response)) %>% 
  mutate(response=fct_explicit_na(response, na_level="Missing")) %>% 
  pivot_wider(names_from=c(skin_exam_age_group,name), values_from=value) %>% 
  clean_names() %>% 
  arrange(question, response)

merged <- skin_exam_results_tbl %>% left_join(skin_exam_results_age_group_tbl, by=c('question', 'response'))
return(merged)
}

skin_exam_table <- skin_exam_summary_table_fn()

write.csv(skin_exam_table, here('data', 'output', 'skin_exam_table.csv'))




form2_table


form2_summary_table_fn <- function() {
form2_results_results_age_group_tbl <- form2_working_data_wd %>%
  dplyr::select(
    f2_sex_fct,
    f2_age_group,
    f2_present_updated,
    f2_consent_updated, 
    f2_stool_container_fct,
    f2_questionnaire_fct,
    f2_stool_sample_fct, 
    f2_leprosy_suspected_fct,
    f2_yaws_suspected_fct, 
    f2_ssd_suspected_fct,
    f2_soap_fct,
    f2_treated_fct, 
    f2_azith_any_fct,
    f2_alb_any_fct,
    f2_ivm_any_fct,
    f2_pm_any_fct) %>% 
  pivot_longer(-f2_age_group) %>% 
  count(f2_age_group, name,value) %>% 
  group_by(f2_age_group, name) %>% 
  mutate(total=sum(n))  %>% 
  ungroup() %>%
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% 
  select(1:6, conf.low, conf.high) %>% clean_names() %>% 
  rename(question=name, response=value) %>% 
  pivot_longer(-c(f2_age_group, question, response)) %>% 
  mutate(response=fct_explicit_na(response, na_level="Missing")) %>% 
  pivot_wider(names_from=c(f2_age_group,name), values_from=value) %>% 
  clean_names() %>% 
  arrange(question, response)


form2_results_tbl <- form2_working_data_wd %>%
  mutate(f2_age_group='OVERALL') %>% 
  dplyr::select(
    f2_sex_fct,
    f2_age_group,
    f2_present_updated,
    f2_consent_updated, 
    f2_stool_container_fct,
    f2_questionnaire_fct,
    f2_stool_sample_fct, 
    f2_leprosy_suspected_fct,
    f2_yaws_suspected_fct, 
    f2_ssd_suspected_fct,
    f2_soap_fct,
    f2_treated_fct, 
    f2_azith_any_fct,
    f2_alb_any_fct,
    f2_ivm_any_fct,
    f2_pm_any_fct) %>% 
  pivot_longer(-f2_age_group) %>% 
  count(f2_age_group, name,value) %>% 
  group_by(f2_age_group, name) %>% 
  mutate(total=sum(n))  %>% 
  ungroup() %>%
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% 
  select(1:6, conf.low, conf.high) %>% clean_names() %>% 
  rename(question=name, response=value) %>% 
  pivot_longer(-c(f2_age_group, question, response)) %>% 
  mutate(response=fct_explicit_na(response, na_level="Missing")) %>% 
  pivot_wider(names_from=c(f2_age_group,name), values_from=value) %>% 
  clean_names() %>% 
  arrange(question, response)


merged <- form2_results_tbl %>% left_join(form2_results_results_age_group_tbl, by=c('question', 'response'))

return(merged)

}

form2_table <- form2_summary_table_fn()

write.csv(form2_table, here('data', 'output', 'form2_table.csv'))

tx_coverage_fn <- function() {
  overall_denominator <- analysis_data_wd %>% 
    filter(f2_flag==1) %>% 
    filter(!is.na(f2_village_fct)) %>% 
    mutate(alb_ivm_denom=case_when(f2_age>=2 ~ 1, TRUE ~ 0)) %>% 
    summarise(total=n(), 
              alb_ivm_denom=sum(alb_ivm_denom, na.rm=TRUE)) %>% 
    mutate(location='overall')
  
  village_denominator <- analysis_data_wd %>% 
    filter(f2_flag==1) %>% 
    filter(!is.na(f2_village_fct)) %>% 
    mutate(alb_ivm_denom=case_when(f2_age>=2 ~ 1, TRUE ~ 0)) %>% 
    group_by(location=f2_village_fct) %>% 
    summarise(total=n(), 
              alb_ivm_denom=sum(alb_ivm_denom, na.rm=TRUE))
  
  village_names <- village_denominator %>% mutate(location=as.character(location)) %>% arrange(location) %>%  pull(location)
  
  village_names <- c('overall', village_names)
  
  combined_denominators <- overall_denominator %>% bind_rows(village_denominator) %>% ungroup()
  
  overall_tx_counts <- analysis_data_wd %>%   
    filter(f2_flag==1) %>% 
    filter(!is.na(f2_village_fct)) %>% 
    dplyr::select(mda_code,f2_azith_any_fct,f2_alb_any_fct,f2_ivm_any_fct, f2_pm_any_fct) %>% 
    pivot_longer(-mda_code) %>% mutate(treat_flag=case_when(value=='yes' ~ 1, 
                                                            TRUE ~ 0)) %>% 
    group_by(name) %>% 
    summarise(tx=sum(treat_flag)) %>% 
    mutate(location='overall') %>% 
    pivot_wider(names_from=name, values_from=tx)
  
  village_tx_counts <- analysis_data_wd %>%   filter(f2_flag==1) %>% 
    filter(!is.na(f2_village_fct)) %>% 
    dplyr::select(f2_village_fct,f2_azith_any_fct,f2_alb_any_fct,f2_ivm_any_fct, f2_pm_any_fct) %>% 
    pivot_longer(-c(f2_village_fct)) %>% mutate(treat_flag=case_when(value=='yes' ~ 1, 
                                                                     TRUE ~ 0)) %>% 
    group_by(location=f2_village_fct,name) %>% 
    summarise(tx=sum(treat_flag)) %>% 
    pivot_wider(names_from=name, values_from=tx)
  
  combined_tx_counts <- overall_tx_counts %>% bind_rows(village_tx_counts) %>% ungroup()
  
  df <- combined_denominators %>% 
    left_join(combined_tx_counts, by='location') %>% 
    mutate(alb_prop = map2(f2_alb_any_fct, alb_ivm_denom, ~ prop.test(.x, .y, conf.level=0.95) %>%
                             broom::tidy())) %>%
    unnest(alb_prop) %>% 
    rename(alb_estimate=estimate,
           alb_conf.low=conf.low,
           alb_conf.high=conf.high) %>% 
    dplyr::select(-c(statistic,p.value,parameter,method, alternative)) %>% 
    mutate(azith_prop = map2(f2_azith_any_fct, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                               broom::tidy())) %>%
    unnest(azith_prop) %>% 
    rename(azith_estimate=estimate,
           azith_conf.low=conf.low,
           azith_conf.high=conf.high) %>% 
    dplyr::select(-c(statistic,p.value,parameter,method, alternative)) %>% 
    mutate(ivm_prop = map2(f2_ivm_any_fct, alb_ivm_denom, ~ prop.test(.x, .y, conf.level=0.95) %>%
                             broom::tidy())) %>%
    unnest(ivm_prop) %>% 
    rename(ivm_estimate=estimate,
           ivm_conf.low=conf.low,
           ivm_conf.high=conf.high) %>% 
    dplyr::select(-c(statistic,p.value,parameter,method, alternative)) %>% 
    mutate(pm_prop = map2(f2_pm_any_fct, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                            broom::tidy())) %>%
    unnest(pm_prop) %>% 
    rename(pm_estimate=estimate,
           pm_conf.low=conf.low,
           pm_conf.high=conf.high) %>% 
    dplyr::select(-c(statistic,p.value,parameter,method, alternative)) %>% 
    dplyr::select(location, contains('estimate'), contains('conf.low'), contains('conf.high')) %>% 
    pivot_longer(-location) %>% 
    mutate(drug=factor(case_when(grepl('alb', name) ~ 'Albendazole',
                                 grepl('azith', name) ~ 'Azithromycin',
                                 grepl('ivm', name) ~ 'Ivermectin',
                                 grepl('pm', name) ~ 'Permethrin'), levels=c('Albendazole', 'Ivermectin', 'Permethrin', 'Azithromycin'))) %>% 
    mutate(measure=str_extract(string = name, pattern = "[^_]+$")) %>% 
    mutate(location=factor(location, levels=village_names)) %>% 
    dplyr::select(location,drug,measure,value) %>% 
    pivot_wider(names_from=measure, values_from=value)
  
  
  
  return(df)
  
}
tx_coverage_tbl <- tx_coverage_fn() %>% pivot_wider(names_from=drug, values_from=c('estimate', 'conf.low', 'conf.high'))

write.csv(tx_coverage_tbl, here('data', 'output', 'tx_coverage_table.csv'))


######NEED TO ANALYSE DISCORDANT RESULTS


sth_result_table_fn <- function() {
  (sth_result_age_group_graph <- sth_result_data %>% 
     count(age_group_sth,pathogen,diag_method,result) %>% 
     pivot_wider(names_from=result, values_from=n) %>% 
     dplyr::select(-sample_not_available) %>% 
     mutate(total=neg+pos) %>% 
     filter(!is.na(age_group_sth)) %>% 
     ungroup() %>% 
     mutate(sth_prev = map2(pos, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                              broom::tidy())) %>% 
     unnest(sth_prev) %>% 
     dplyr::select(-c(statistic,p.value,parameter,method, alternative, 'no result')) %>% 
     pivot_longer(-c('age_group_sth', 'pathogen', 'diag_method')) %>% 
     pivot_wider(names_from=c(age_group_sth,name), values_from=value)
  )
  
  (sth_result_graph <- sth_result_data %>% 
      count(,pathogen,diag_method,result) %>% 
      pivot_wider(names_from=result, values_from=n) %>% 
      dplyr::select(-sample_not_available) %>% 
      mutate(total=neg+pos) %>% 
      #filter(!is.na(age_group_sth)) %>% 
      ungroup() %>% 
      mutate(sth_prev = map2(pos, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                               broom::tidy())) %>% 
      unnest(sth_prev) %>% 
      dplyr::select(-c(statistic,p.value,parameter,method, alternative, 'no result')) %>% 
      pivot_longer(-c( 'pathogen', 'diag_method')) %>% 
      pivot_wider(names_from=c(name), values_from=value)
  )
  
  sth_table  <- sth_result_graph %>% left_join(sth_result_age_group_graph, by=c('pathogen', 'diag_method'))
  
  return(sth_table)
}
sth_table <- sth_result_table_fn()


(sth_result_data <- analysis_data_wd %>% dplyr::select(f2_village_fct,
                                                       f2_age, f2_sex_fct,f2_stool_sample_fct,
                                                       qPCR_result_ascaris = qPCR_result_ascaris_ct1,f11_result_ascaris,
                                                       qPCR_result_trichuris = qPCR_result_trichuris_ct1,f11_result_trichuris,
                                                       qPCR_result_hookworm, f11_result_hookworm) %>% 
    mutate(age_group_sth=factor(case_when(f2_age %in% c(1:4) ~ 'PSAC 1-4',
                                          f2_age %in% c(5:14) ~ 'SAC 5-14',
                                          f2_age >=15 ~ 'ADULTS >15'), levels=c('PSAC 1-4', 'SAC 5-14','ADULTS >15'))) %>% 
    filter(!is.na(qPCR_result_ascaris)) %>% dplyr::select(-f2_age) %>% 
    dplyr::select(f2_village_fct, f2_sex_fct, age_group_sth, contains('qPCR'), contains('f11')) %>% 
    mutate(id=row_number()) %>% 
    mutate(qPCR_result_any_sth = case_when(
      if_any(starts_with('qPCR'), ~ . == 'pos') ~ "pos",
      TRUE ~ "neg"
    ),
    f11_result_any_sth = case_when(
      if_any(starts_with('f11'), ~ . == 'pos') ~ "pos",
      if_any(starts_with('f11'), ~ . == 'no result') ~ "no result",
      TRUE ~ "neg"
    )) %>% 
    pivot_longer(-c('id', 'f2_village_fct', 'f2_sex_fct', 'age_group_sth')) %>% 
    mutate(diag_method=str_extract(string = name, pattern = "[^_]+")) %>% 
    mutate(diag_method=factor(diag_method, levels=c('f11', 'qPCR'), labels=c('Sodium nitrate flotation', 'qPCR'))) %>% 
    mutate(pathogen=str_extract(string = name, pattern = "[^_]+$")) %>% 
    mutate(pathogen=factor(pathogen, levels=c('ascaris', 'trichuris', 'hookworm', 'sth'))) %>% 
    dplyr::select(id,f2_village_fct, f2_sex_fct, age_group_sth, pathogen,diag_method,result=value)
)

x <- sth_result_data %>% 
  pivot_wider(names_from=c(age_group_sth, pathogen, diag_method), values_from=result)

sth_result_data %>% 
  count(age_group_sth,pathogen,diag_method,result) 
pivot_wider(names_from=diag_method, values_from=n)





summary_stats_fn <- function(question_number) {
  
  question_number_enquo <- sym(question_number)
  
  
  scabies_data %>% 
    count(!!question_number_enquo,skin_exam_age_group) %>% 
    group_by(skin_exam_age_group) %>%
    mutate(total=sum(n)) %>%
    ungroup() %>%
    mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                         broom::tidy())) %>%
    unnest(prop) %>%
    select(1:5, conf.low, conf.high)
  
}  

summary_stats_fn('skin_exam_scabies_scratching_24_fct')

map_dfr(scabies_questions, summary_stats_fn)


foo <- function(x, y) count(tibble(x), !!y)
imap(scabies_data, skin_exam_age_group)



lapply(scabies_questions, summary_stats)

