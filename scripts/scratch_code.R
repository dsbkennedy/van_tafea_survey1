

form11_working_data %>% 
  select(mda_code,
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
  )
         

stool_receipt_qpcr_results_form11 %>% 
  mutate(qPCR_result_hookworm = case_when(f11_results_flag!=1 ~ 'no match', 
                                             TRUE ~ qPCR_result_hookworm)) %>% 
  tabyl(f11_result_hookworm,qPCR_result_hookworm)


  mutate(f11_result_hookworm = case_when(qPCR_results_flag!=1 ~ 'no match', 
                                         f11_results_flag))
  

filter(qPCR_results_flag==1 & f11_results_flag==1) %>% 
  tabyl(f11_result_hookworm,qPCR_result_hookworm)

  
duplicate_record <-  form11_working_data %>%
  group_by(mda_code) %>% 
  mutate(count = n()) %>% 
  filter(count!=1)
  

(form3.1_ids <- form3.1_working_data %>% 
  select(date_sample_collected, f31_area_council_fct, f31_village_fct, mda_code)
)


form3.1_ids %>% filter(f31_area_council_fct == 'whitesands' &
                         f31_village_fct == 'ikurakau') %>% 
inner_join(
  form11_working_data %>% filter(f11_area_council == 'whitesands' &
                                f11_village == 'ikurakau'),
  by = c('mda_code'
  )
) %>% 
  select(mda_code)


qpcr_nomatch_nafloat <-   stool_receipt_qpcr_results %>%
  filter(sample_yn == 1 | qPCR_results_flag == 1) %>%
  anti_join(form11_working_data, by = 'mda_code') %>% 
  select(mda_code) %>% mutate(data='qpcr_nomatch_nafloat')



nafloat_no_match_qpcr <- form11_working_data %>% 
  anti_join(stool_receipt_qpcr_results %>%
              filter(sample_yn == 1 | qPCR_results_flag == 1), by='mda_code') %>% 
  select(mda_code) %>% mutate(data='nafloat_no_match_qpcr')

x <- qpcr_nomatch_nafloat %>% bind_rows(nafloat_no_match_qpcr) %>% filter(!is.na(mda_code)) %>% 
  separate(mda_code, c("A", "B", "C", "D"))




form2_nomatch_skin_exam <- form2_working_data %>% anti_join(skin_exam_working_data, by='mda_code') %>% 
  select(mda_code, participant_name=f2_participant_name, area_council=f2_area_council_fct,village=f2_village_fct) %>% 
  mutate(origin='form2')
skin_exam_nomatch_form2 <- skin_exam_working_data %>% anti_join(form2_working_data, by='mda_code') %>% 
  select(mda_code, participant_name=skin_exam_participant_name, area_council = skin_exam_area_council,village=skin_exam_village) %>% 
  mutate(origin='skinexam')

unmatched <- form2_nomatch_skin_exam %>% bind_rows(skin_exam_nomatch_form2) %>% 
  arrange(area_council, village, mda_code)


form2_skin_exam_merged %>% ungroup() %>% filter(f2_flag==1 & skin_exam_flag==1) 
  count(f2_area_council_fct, skin_exam_area_council_fct)


  comparing_qpcr_naflotation <-
    stool_receipt_qpcr_results_form11 %>% 
    mutate(f11_result_hookworm = case_when(is.na(f11_results_flag) ~ 'no match',
                                          TRUE ~ f11_result_hookworm)) %>%
    mutate(qPCR_result_hookworm = case_when(
      is.na(qPCR_results_flag) ~ 'no match',
      TRUE ~ qPCR_result_hookworm
    )) %>%
    mutate(f11_result_hookworm = factor(f11_result_hookworm, levels = c('pos', 'neg', 'no match'))) %>%
    mutate(qPCR_result_hookworm = factor(
      qPCR_result_hookworm,
      levels = c('pos', 'neg', 'no match', 'sample_not_available')
    )) %>%
    tabyl(f11_result_hookworm, qPCR_result_hookworm) 
  
  
  
  (discordant_results <- stool_receipt_qpcr_results_form11 %>% filter(qPCR_results_flag==1 & f11_results_flag==1) %>% 
      select(qPCR_unimelb_id,mda_code, 
             qPCR_result_ascaris = qPCR_result_ascaris_ct1,f11_result_ascaris,f11_ascaris_total_egg_count, f11_ascaris_epg,
             qPCR_result_trichuris = qPCR_result_trichuris_ct1,f11_result_trichuris, f11_trichuris_total_egg_count, f11_trichuris_epg,
             qPCR_result_hookworm, f11_result_hookworm, f11_hookworm_total_egg_count, f11_hookworm_epg) %>% 
      pivot_longer(-c(qPCR_unimelb_id,mda_code)) %>% 
      mutate(method=str_extract(string = name, pattern = "[^_]+")) %>% 
      mutate(pathogen=str_extract(string = name, pattern = "[^_]+$")) %>% 
      group_by(mda_code, pathogen, value) %>% 
      mutate(count = n()) %>% 
      filter(count!=2) %>% 
      select(unimelb_id=qPCR_unimelb_id,mda_code, method, pathogen, value) %>% 
      pivot_wider(names_from=c('method', 'pathogen'), 
                  values_from=value)
  )
  

  (discordant_results_flag <- stool_receipt_qpcr_results_form11 %>% filter(qPCR_results_flag==1 & f11_results_flag==1) %>% 
      select(qPCR_unimelb_id,mda_code, 
             qPCR_result_ascaris = qPCR_result_ascaris_ct1,f11_result_ascaris,
             qPCR_result_trichuris = qPCR_result_trichuris_ct1,f11_result_trichuris,
             qPCR_result_hookworm, f11_result_hookworm) %>% 
      pivot_longer(-c(qPCR_unimelb_id,mda_code)) %>% 
      mutate(method=str_extract(string = name, pattern = "[^_]+")) %>% 
      mutate(pathogen=str_extract(string = name, pattern = "[^_]+$")) %>% 
      group_by(mda_code, pathogen, value) %>% 
      mutate(count = n()) %>% 
      filter(count!=2) %>% 
      select(-count) 
  )
  
  (discordant_results_count <- stool_receipt_qpcr_results_form11 %>% 
      filter(qPCR_results_flag==1 & f11_results_flag==1) %>% 
      select(qPCR_unimelb_id,mda_code, 
             f11_ascaris_total_egg_count, f11_ascaris_epg,
             f11_trichuris_total_egg_count, f11_trichuris_epg,
             f11_hookworm_total_egg_count, f11_hookworm_epg) %>% 
      pivot_longer(-c(qPCR_unimelb_id,mda_code))  %>% 
      rename(counts=value) %>% 
      mutate(method=str_extract(string = name, pattern = "[^_]+")) %>% 
      mutate(pathogen=case_when(grepl('ascaris', name) ~ 'ascaris',
             grepl('trichuris', name ) ~ 'trichuris',
             grepl('hookworm', name) ~ 'hookworm')) %>% 
      mutate(indicator=case_when(grepl('epg', name) ~ 'epg',
                                 TRUE ~ 'egg_count')) %>% 
      select(qPCR_unimelb_id, mda_code, method, pathogen, indicator,counts)
  )
  
 ( x <- discordant_results %>% 
    inner_join(discordant_results_count, by=c('qPCR_unimelb_id', 'mda_code', 'method','pathogen' )) %>% 
     select(qPCR_unimelb_id, mda_code, pathogen, indicator,counts) %>% 
     pivot_wider(names_from=indicator, values_from=counts) %>% 
     filter(value=='pos') %>% 
     pivot_wider(names_from=pathogen, values_from=c(egg_count, epg))
  )
  
  
  
  
  stool_receipt_qpcr_results_form11 %>% 
    filter(qPCR_result_ascaris_ct1 %in% c('pos', 'neg')) %>% 
    dplyr::select(qPCR_result_ascaris_ct1, f11_ascaris_epg ) %>% 
    ggplot(aes(x=log10(f11_ascaris_epg), color=qPCR_result_ascaris_ct1)) +
    geom_density()
  
  
x <-   stool_receipt_qpcr_results_form11 %>% 
    select(mda_code, qPCR_result_ascaris_ct1, qPCR_result_hookworm, qPCR_result_trichuris_ct1,
           f11_ascaris_epg,f11_hookworm_epg, f11_trichuris_epg) 

(results <- stool_receipt_qpcr_results_form11 %>% 
    mutate(f11_result_hookworm = case_when(is.na(f11_results_flag) ~ 'no match',
                                           TRUE ~ f11_result_hookworm)) %>%
    mutate(qPCR_result_hookworm = case_when(
      is.na(qPCR_results_flag) ~ 'no match',
      TRUE ~ qPCR_result_hookworm
    )) %>% filter(f11_result_hookworm!='no match') %>% 
    filter(qPCR_result_hookworm!='no match') %>% 
    select(mda_code, qPCR_result_ascaris_ct1, qPCR_result_hookworm, qPCR_result_trichuris_ct1) %>% 
  pivot_longer(-mda_code) %>% 
  rename(pos_neg=value) %>% 
  mutate(pathogen=case_when(grepl('ascaris', name) ~ 'ascaris',
                            grepl('hookworm', name) ~ 'hookworm',
                            grepl('trichuris', name) ~ 'trichuris'))
)
  

(epg <- stool_receipt_qpcr_results_form11 %>% 
    mutate(f11_result_hookworm = case_when(is.na(f11_results_flag) ~ 'no match',
                                           TRUE ~ f11_result_hookworm)) %>%
    mutate(qPCR_result_hookworm = case_when(
      is.na(qPCR_results_flag) ~ 'no match',
      TRUE ~ qPCR_result_hookworm
    )) %>% filter(f11_result_hookworm!='no match') %>% 
    filter(qPCR_result_hookworm!='no match') %>% 
  select(mda_code,f11_ascaris_epg,f11_hookworm_epg, f11_trichuris_epg) %>% 
  pivot_longer(-mda_code) %>% 
  rename(epg=value) %>% 
  mutate(pathogen=case_when(grepl('ascaris', name) ~ 'ascaris',
                            grepl('hookworm', name) ~ 'hookworm',
                            grepl('trichuris', name) ~ 'trichuris'))
)

  

combined <- results %>% left_join(epg, by=c('mda_code', 'pathogen'))

library(ggbeeswarm)

library(devtools)
pacman::p_load('easyGgplot2')
install_github("easyGgplot2", "kassambara")

pacman::p_load('ggpubr')

combined %>% 
  filter(pos_neg %in% c('pos', 'neg')) %>% 
  filter(!is.na(epg)) %>% 
  ggplot(aes(x=pos_neg,y=log10(epg),color=pos_neg)) +
  geom_boxplot() +
  geom_jitter() +
  labs(x='qPCR pos/neg')+
  facet_wrap(~ pathogen)

combined %>% 
  filter(pos_neg %in% c('pos', 'neg')) %>% 
  filter(!is.na(epg)) %>% 
  mutate(log10epg=log10(epg)) %>% 
  mutate(pathogen=factor(pathogen,levels=c('ascaris', 'trichuris', 'hookworm'))) %>% 
ggboxplot(., x = "pos_neg", y = "log10epg",
          color = "pos_neg", palette = "jco",
          add = "jitter") +
  #stat_compare_means(method = "t.test") +
  labs(x='qPCR result', title='Comparing eggs per gram from SNF by qPCR result')+
  theme(legend.position = 'none') +
  facet_wrap(~ pathogen)


combined %>% 
  filter(pos_neg %in% c('pos', 'neg')) %>% 
  filter(!is.na(epg)) %>% 
  filter(epg>0) %>% 
  group_by(pathogen,pos_neg) %>% 
  summarise(mean=mean(epg, na.rm=TRUE),
            min=min(epg, na.rm=TRUE),
            max=max(epg, na.rm=TRUE)) %>% 
  mutate(across(c(mean,min,max), round,0)) %>% 
  pivot_wider(names_from=pos_neg, values_from=c(mean, min, max)) %>% 
  mutate(neg_mean_range=paste0(mean_neg, ' (', min_neg,' - ' ,max_neg, ')')) %>% 
  mutate(pos_mean_range=paste0(mean_pos, ' (', min_pos,' - ' ,max_pos, ')')) %>% 
  select(pathogen,neg_mean_range, pos_mean_range)


combined %>% 
  filter(pos_neg %in% c('pos', 'neg')) %>% 
  filter(!is.na(epg)) %>% 
  filter(epg>0) %>% 
  tabyl(pathogen,pos_neg) 
  
  comparing_qpcr_naflotation <-
    stool_receipt_qpcr_results_form11 %>% 
    mutate(f11_result_hookworm = case_when(is.na(f11_results_flag) ~ 'no match',
                                           TRUE ~ f11_result_hookworm)) %>%
    mutate(qPCR_result_hookworm = case_when(
      is.na(qPCR_results_flag) ~ 'no match',
      TRUE ~ qPCR_result_hookworm
    )) %>%
    mutate(f11_result_hookworm = factor(f11_result_hookworm, levels = c('pos', 'neg', 'no match'))) %>%
    mutate(qPCR_result_hookworm = factor(
      qPCR_result_hookworm,
      levels = c('pos', 'neg', 'no match', 'sample_not_available')
    )) %>%
    tabyl(f11_result_hookworm, qPCR_result_hookworm) 
  
mda_code_issues <- stool_receipt_qpcr_results_form11 %>% 
    filter(is.na(qPCR_results_flag) | is.na(f11_results_flag)) %>% 
    select(mda_code, qPCR_results_flag, f11_results_flag, qPCR_result_ascaris_ct1)  %>% 
    filter(!is.na(mda_code))


sth_prevalence_gph_fn <- function () {
  sth_prevalence_data <- analysis_data %>% select(f2_village_fct,
                                                  f2_age, f2_sex_fct,f2_stool_sample_fct,
                                                  qPCR_result_ascaris = qPCR_result_ascaris_ct1,f11_result_ascaris,
                                                  qPCR_result_trichuris = qPCR_result_trichuris_ct1,f11_result_trichuris,
                                                  qPCR_result_hookworm, f11_result_hookworm) %>% 
    mutate(age_group_sth=factor(case_when(f2_age %in% c(1:4) ~ 'PSAC 1-4',
                                          f2_age %in% c(5:14) ~ 'SAC 5-14',
                                          f2_age >=15 ~ 'ADULTS >15'), levels=c('PSAC 1-4', 'SAC 5-14','ADULTS >15'))) %>% 
    filter(!is.na(qPCR_result_ascaris)) %>% select(-f2_age) %>% 
    select(f2_village_fct, f2_sex_fct, age_group_sth, contains('qPCR'), contains('f11')) %>% 
    pivot_longer(-c('f2_village_fct', 'f2_sex_fct', 'age_group_sth')) %>% 
    mutate(diag_method=str_extract(string = name, pattern = "[^_]+")) %>% 
    mutate(diag_method=factor(diag_method, levels=c('f11', 'qPCR'), labels=c('Sodium nitrate flotation', 'qPCR'))) %>% 
    mutate(pathogen=str_extract(string = name, pattern = "[^_]+$")) %>% 
    select(f2_village_fct, f2_sex_fct, age_group_sth, pathogen,diag_method,result=value)
  
  
  sth_prevalence_graph <- sth_result_data %>% 
    count(age_group_sth,pathogen,diag_method,result) %>% 
    pivot_wider(names_from=result, values_from=n) %>% 
    select(-sample_not_available) %>% 
    mutate(total=neg+pos) %>% 
    filter(!is.na(age_group_sth)) %>% 
    ungroup() %>% 
    mutate(sth_prev = map2(pos, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                             broom::tidy())) %>% 
    unnest(sth_prev) %>% 
    ggplot(aes(x=age_group_sth, y=estimate, group=1)) +
    geom_point(aes(colour=diag_method), size=4) +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) +
    geom_line() +
    theme_classic() +
    theme(legend.position = 'none') +
    scale_y_continuous(labels = scales::percent) +
    labs(x='Age group', y='% prevalence') +  
    facet_wrap(~ pathogen + diag_method, ncol=2)
  
  return(sth_prevalence_graph)
}
sth_prevalence_gph_fn()


sth_intensity_tbl_fn <- function() {
  sth_intenstiy_data <- analysis_data %>% select(f2_village_fct,f2_age, f2_sex_fct,f2_stool_sample_fct,
                                                 f11_ascaris_epg,
                                                 f11_trichuris_epg,
                                                 f11_hookworm_epg) %>% 
    mutate(age_group_sth=factor(case_when(f2_age %in% c(1:4) ~ 'PSAC 1-4',
                                          f2_age %in% c(5:14) ~ 'SAC 5-14',
                                          f2_age >=15 ~ 'ADULTS >15'), levels=c('PSAC 1-4', 'SAC 5-14','ADULTS >15'))) %>% 
    filter(!is.na(f11_ascaris_epg)) %>% 
    select(-f2_age) %>% 
    select(f2_village_fct, f2_sex_fct, age_group_sth,  contains('f11')) %>% 
    pivot_longer(-c('f2_village_fct', 'f2_sex_fct', 'age_group_sth')) %>% 
    rename(epg=value) %>% 
    mutate(pathogen=case_when(grepl('ascaris', name) ~ 'ascaris',
                              grepl('trichuris', name ) ~ 'trichuris',
                              grepl('hookworm', name) ~ 'hookworm'))  %>% 
    mutate(intensity=case_when(pathogen=='ascaris'  & (epg >=1 & epg<5000) ~ 'light',
                               pathogen=='ascaris'  & (epg >=5000 & epg<50000) ~ 'moderate',
                               pathogen=='ascaris' & epg >=50000 ~ 'heavy',
                               pathogen=='trichuris'  & (epg >=1 & epg<1000) ~ 'light',
                               pathogen=='trichuris'  & (epg >=1000 & epg<=10000) ~ 'moderate',
                               pathogen=='trichuris' & epg >=10000 ~ 'heavy',
                               pathogen=='hookworm'  & (epg >=1 & epg<2000) ~ 'light',
                               pathogen=='hookworm'  & (epg >=2000 & epg<4000) ~ 'moderate',
                               pathogen=='hookworm' & epg >=4000 ~ 'heavy',
                               epg==0 ~ 'negative')) %>% 
    mutate(pathogen=factor(pathogen, levels=c('ascaris', 'trichuris', 'hookworm'))) %>% 
    mutate(intensity=factor(intensity, levels=c('negative', 'light', 'moderate', 'heavy'))) %>% 
    select(f2_village_fct, f2_sex_fct, age_group_sth, pathogen,intensity)
  
  sth_intenstiy_tbl <- table1(~ intensity|pathogen, data=sth_intenstiy_data)
  
  return(sth_intenstiy_tbl)
}




glimpse(risk_factor_working_data)


risk_factor_working_data %>% 
  ggplot(aes(x=risk_factor_household_occupants)) +
  geom_histogram() +
  facet_wrap(~ risk_factor_area_council)

risk_factor_working_data %>% ungroup() %>% 
  summarise(mean_household_occupants=mean(risk_factor_household_occupants, na.rm=TRUE))


risk_factor_working_data %>% ungroup() %>% 
  group_by(risk_factor_area_council) %>% 
  dplyr::summarise(
    avg = mean(risk_factor_household_occupants,na.rm=TRUE),
    lci = t.test(risk_factor_household_occupants, conf.level = 0.95)$conf.int[1],
    uci = t.test(risk_factor_household_occupants, conf.level = 0.95)$conf.int[2]) %>% 
  ungroup() %>% 
  bind_rows(summarize(., region = "Overall Avg", 
                      avg = sum(avg * n) / sum(n), 
                      n = sum(n))) %>%
  select(-n)

scabies_location_data <- skin_exam_data_wd %>%
  filter(skin_exam_flag == 1) %>%
  #filter(f2_present_fct == 'yes') %>%
  dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct,
                skin_exam_age_group,
                skin_exam_scabies_scratching_24_fct,
                skin_exam_scabies_typical_lesions_fct,
                skin_exam_scabies_lesions_more_10_fct,
                skin_exam_scabies_skin_infection_fct,
                skin_exam_scabies_finished_fct
  )

scabies_location_data %>% 
  dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct, skin_exam_age_group,skin_exam_scabies_scratching_24_fct ) %>% 
  group_by(skin_exam_area_council_fct, skin_exam_village_fct) %>% 
  mutate(total=sum(n()))


scabies_location_data %>% 
  dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct, skin_exam_age_group,skin_exam_scabies_scratching_24_fct ) %>% 
  count(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_scabies_scratching_24_fct) %>% filter(skin_exam_scabies_scratching_24_fct=='yes') %>% 
  left_join(scabies_location_data %>% 
              dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct, skin_exam_age_group,skin_exam_scabies_scratching_24_fct ) %>% 
              group_by(skin_exam_area_council_fct, skin_exam_village_fct) %>% 
              summarise(total=sum(n())), by=c('skin_exam_area_council_fct', 'skin_exam_village_fct')) %>% 
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% 
  ggplot(aes(x=skin_exam_village_fct, y=estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) +
  labs(y='Reported itching in last 24 hours') +
  #theme_classic() +
  facet_wrap(~skin_exam_area_council_fct, scales='free_x', nrow=1)
####Break down by age group




overall_prop_fn <- function() {
  
  overall_scabies_village_itching_propotions <- scabies_location_data %>% 
    dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_scabies_scratching_24_fct ) %>% 
    count(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_scabies_scratching_24_fct) %>% filter(skin_exam_scabies_scratching_24_fct=='yes') %>% 
    full_join(scabies_location_data %>% 
                dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_scabies_scratching_24_fct ) %>% 
                group_by(skin_exam_area_council_fct, skin_exam_village_fct) %>% 
                summarise(total=sum(n())), by=c('skin_exam_area_council_fct', 'skin_exam_village_fct')) %>% replace_na(list(n=0))
  
  a <- overall_scabies_village_itching_propotions %>% filter(n>0) %>% 
    mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                         broom::tidy())) %>% 
    unnest(prop) %>% dplyr::select(skin_exam_area_council_fct,skin_exam_village_fct, n, total, estimate, conf.low, conf.high)
  
  b <- overall_scabies_village_itching_propotions %>% filter(n==0) %>% dplyr::select(-skin_exam_scabies_scratching_24_fct)
  
  c <- a %>% bind_rows(b) %>% arrange(skin_exam_area_council_fct, skin_exam_village_fct) %>% replace_na(list(estimate=0, conf.low=0, conf.high=0))
  
  return(c)
  
}

overall_scabies_village_proportion <- overall_prop_fn()



overall_scabies_village_proportion %>% 
  ggplot(aes(x=skin_exam_village_fct, y=estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) +
  labs(y='Reported itching in last 24 hours') +
  #theme_classic() +
  facet_wrap(~skin_exam_area_council_fct, scales='free_x', nrow=1)


####Break down by age group

######################OVERALL- NO AGE GROUP
all_population <- scabies_location_data %>%
  dplyr::select(skin_exam_scabies_scratching_24_fct ) %>%
  summarise(total=sum(n())) %>% 
  mutate(skin_exam_area_council_fct='Overall')

all_scabies_cases_population_merge <- scabies_location_data %>%
  dplyr::select(skin_exam_scabies_scratching_24_fct ) %>%
  count(skin_exam_scabies_scratching_24_fct) %>%
  filter(skin_exam_scabies_scratching_24_fct=='yes') %>%
  dplyr::select(-skin_exam_scabies_scratching_24_fct) %>%
  mutate(skin_exam_area_council_fct='Overall') %>% 
  full_join(all_population,by=c('skin_exam_area_council_fct')) %>%
  replace_na(list(n=0))


all_scabies_propotions <- all_scabies_cases_population_merge %>% filter(n>0) %>%
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% dplyr::select(skin_exam_area_council_fct, n, total, estimate, conf.low, conf.high)

all_age_scabies_itching_propotions <- all_scabies_cases_population_merge %>% filter(n==0) %>%
  bind_rows(all_scabies_propotions) %>% 
  pivot_longer(-c(skin_exam_area_council_fct)) %>%
  pivot_wider(names_from=c(name), values_from=value)


######BY VILLAGE - AGE GROUP  
village_age_population <- scabies_location_data %>% 
  dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_age_group, skin_exam_age_group,skin_exam_scabies_scratching_24_fct ) %>% 
  group_by(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_age_group) %>% 
  summarise(total=sum(n()))

scabies_cases_population_merge <- scabies_location_data %>% 
  dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct, skin_exam_age_group,skin_exam_scabies_scratching_24_fct ) %>% 
  count(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_age_group,skin_exam_scabies_scratching_24_fct) %>% 
  filter(skin_exam_scabies_scratching_24_fct=='yes') %>% 
  dplyr::select(-skin_exam_scabies_scratching_24_fct) %>% 
  full_join(village_age_population,by=c('skin_exam_area_council_fct', 'skin_exam_village_fct', 'skin_exam_age_group')) %>% 
  replace_na(list(n=0))


scabies_propotions <- scabies_cases_population_merge %>% filter(n>0) %>% 
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct, skin_exam_age_group, n, total, estimate, conf.low, conf.high)

scabies_itching_propotions <- scabies_cases_population_merge %>% filter(n==0) %>% 
  bind_rows(scabies_propotions) %>% arrange(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_age_group) %>% 
  pivot_longer(-c(skin_exam_area_council_fct, skin_exam_village_fct, skin_exam_age_group)) %>% 
  pivot_wider(names_from=c(name, skin_exam_age_group), values_from=value)

######ALL POPULATION - AGE GROUP
all_age_population <- scabies_location_data %>%
  dplyr::select(skin_exam_age_group, skin_exam_scabies_scratching_24_fct ) %>%
  group_by(skin_exam_age_group) %>%
  summarise(total=sum(n()))

all_age_scabies_cases_population_merge <- scabies_location_data %>%
  dplyr::select(skin_exam_age_group,skin_exam_scabies_scratching_24_fct ) %>%
  count(skin_exam_age_group,skin_exam_scabies_scratching_24_fct) %>%
  filter(skin_exam_scabies_scratching_24_fct=='yes') %>%
  dplyr::select(-skin_exam_scabies_scratching_24_fct) %>%
  full_join(all_age_population,by=c('skin_exam_age_group')) %>%
  replace_na(list(n=0))


all_age_scabies_propotions <- scabies_cases_population_merge %>% filter(n>0) %>%
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% dplyr::select(skin_exam_age_group, n, total, estimate, conf.low, conf.high)

all_age_scabies_itching_propotions <- all_age_scabies_cases_population_merge %>% filter(n==0) %>%
  bind_rows(all_age_scabies_propotions) %>% arrange(skin_exam_age_group) %>%
  pivot_longer(-c(skin_exam_age_group)) %>%
  pivot_wider(names_from=c(name, skin_exam_age_group), values_from=value)


glimpse(risk_factor_working_data)


risk_factor_working_data %>% 
  ggplot(aes(x=risk_factor_household_occupants)) +
  geom_histogram() +
  facet_wrap(~ risk_factor_area_council)

risk_factor_working_data %>% ungroup() %>% 
  summarise(mean_household_occupants=mean(risk_factor_household_occupants, na.rm=TRUE))


risk_factor_working_data %>% ungroup() %>% 
  group_by(risk_factor_area_council) %>% 
  dplyr::summarise(
    avg = mean(risk_factor_household_occupants,na.rm=TRUE),
    lci = t.test(risk_factor_household_occupants, conf.level = 0.95)$conf.int[1],
    uci = t.test(risk_factor_household_occupants, conf.level = 0.95)$conf.int[2]) %>% 
  ungroup() %>% 
  bind_rows(summarize(., region = "Overall Avg", 
                      avg = sum(avg * n) / sum(n), 
                      n = sum(n))) %>%
  select(-n)

scabies_location_data <- skin_exam_data_wd %>%
  filter(skin_exam_flag == 1) %>%
  #filter(f2_present_fct == 'yes') %>%
  dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct,
                skin_exam_age_group,
                skin_exam_scabies_scratching_24_fct,
                skin_exam_scabies_typical_lesions_fct,
                skin_exam_scabies_lesions_more_10_fct,
                skin_exam_scabies_skin_infection_fct,
                skin_exam_scabies_finished_fct
  )


overall_prop_fn <- function() {
  
  overall_scabies_village_itching_propotions <- scabies_location_data %>% 
    dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_scabies_scratching_24_fct ) %>% 
    count(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_scabies_scratching_24_fct) %>% filter(skin_exam_scabies_scratching_24_fct=='yes') %>% 
    full_join(scabies_location_data %>% 
                dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_scabies_scratching_24_fct ) %>% 
                group_by(skin_exam_area_council_fct, skin_exam_village_fct) %>% 
                summarise(total=sum(n())), by=c('skin_exam_area_council_fct', 'skin_exam_village_fct')) %>% replace_na(list(n=0))
  
  a <- overall_scabies_village_itching_propotions %>% filter(n>0) %>% 
    mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                         broom::tidy())) %>% 
    unnest(prop) %>% dplyr::select(skin_exam_area_council_fct,skin_exam_village_fct, n, total, estimate, conf.low, conf.high)
  
  b <- overall_scabies_village_itching_propotions %>% filter(n==0) %>% dplyr::select(-skin_exam_scabies_scratching_24_fct)
  
  c <- a %>% bind_rows(b) %>% arrange(skin_exam_area_council_fct, skin_exam_village_fct) %>% replace_na(list(estimate=0, conf.low=0, conf.high=0))
  
  return(c)
  
}

overall_scabies_village_proportion <- overall_prop_fn()



overall_scabies_village_proportion %>% 
  ggplot(aes(x=skin_exam_village_fct, y=estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) +
  labs(y='Reported itching in last 24 hours') +
  scale_y_continuous(labels = scales::percent) +
  #theme_classic() +
  facet_wrap(~skin_exam_area_council_fct, scales='free_x', nrow=1)


####Break down by age group

######################OVERALL- NO AGE GROUP
all_population <- scabies_location_data %>%
  dplyr::select(skin_exam_scabies_scratching_24_fct ) %>%
  summarise(total=sum(n())) %>% 
  mutate(skin_exam_area_council_fct='Overall')

all_scabies_cases_population_merge <- scabies_location_data %>%
  dplyr::select(skin_exam_scabies_scratching_24_fct ) %>%
  count(skin_exam_scabies_scratching_24_fct) %>%
  filter(skin_exam_scabies_scratching_24_fct=='yes') %>%
  dplyr::select(-skin_exam_scabies_scratching_24_fct) %>%
  mutate(skin_exam_area_council_fct='Overall') %>% 
  full_join(all_population,by=c('skin_exam_area_council_fct')) %>%
  replace_na(list(n=0))


all_scabies_propotions <- all_scabies_cases_population_merge %>% filter(n>0) %>%
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% dplyr::select(skin_exam_area_council_fct, n, total, estimate, conf.low, conf.high)

all_age_scabies_itching_propotions <- all_scabies_cases_population_merge %>% filter(n==0) %>%
  bind_rows(all_scabies_propotions) %>% 
  pivot_longer(-c(skin_exam_area_council_fct)) %>%
  pivot_wider(names_from=c(name), values_from=value)


######BY VILLAGE - AGE GROUP  
village_age_population <- scabies_location_data %>% 
  dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_age_group, skin_exam_age_group,skin_exam_scabies_scratching_24_fct ) %>% 
  group_by(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_age_group) %>% 
  summarise(total=sum(n()))

scabies_cases_population_merge <- scabies_location_data %>% 
  dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct, skin_exam_age_group,skin_exam_scabies_scratching_24_fct ) %>% 
  count(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_age_group,skin_exam_scabies_scratching_24_fct) %>% 
  filter(skin_exam_scabies_scratching_24_fct=='yes') %>% 
  dplyr::select(-skin_exam_scabies_scratching_24_fct) %>% 
  full_join(village_age_population,by=c('skin_exam_area_council_fct', 'skin_exam_village_fct', 'skin_exam_age_group')) %>% 
  replace_na(list(n=0))


scabies_propotions <- scabies_cases_population_merge %>% filter(n>0) %>% 
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% dplyr::select(skin_exam_area_council_fct, skin_exam_village_fct, skin_exam_age_group, n, total, estimate, conf.low, conf.high)

scabies_itching_propotions <- scabies_cases_population_merge %>% filter(n==0) %>% 
  bind_rows(scabies_propotions) %>% arrange(skin_exam_area_council_fct, skin_exam_village_fct,skin_exam_age_group) %>% 
  pivot_longer(-c(skin_exam_area_council_fct, skin_exam_village_fct, skin_exam_age_group)) %>% 
  pivot_wider(names_from=c(name, skin_exam_age_group), values_from=value)

######ALL POPULATION - AGE GROUP
all_age_population <- scabies_location_data %>%
  dplyr::select(skin_exam_age_group, skin_exam_scabies_scratching_24_fct ) %>%
  group_by(skin_exam_age_group) %>%
  summarise(total=sum(n()))

all_age_scabies_cases_population_merge <- scabies_location_data %>%
  dplyr::select(skin_exam_age_group,skin_exam_scabies_scratching_24_fct ) %>%
  count(skin_exam_age_group,skin_exam_scabies_scratching_24_fct) %>%
  filter(skin_exam_scabies_scratching_24_fct=='yes') %>%
  dplyr::select(-skin_exam_scabies_scratching_24_fct) %>%
  full_join(all_age_population,by=c('skin_exam_age_group')) %>%
  replace_na(list(n=0))


all_age_scabies_propotions <- scabies_cases_population_merge %>% filter(n>0) %>%
  mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% dplyr::select(skin_exam_age_group, n, total, estimate, conf.low, conf.high)

all_age_scabies_itching_propotions <- all_age_scabies_cases_population_merge %>% filter(n==0) %>%
  bind_rows(all_age_scabies_propotions) %>% arrange(skin_exam_age_group) %>%
  pivot_longer(-c(skin_exam_age_group)) %>%
  pivot_wider(names_from=c(name, skin_exam_age_group), values_from=value)










