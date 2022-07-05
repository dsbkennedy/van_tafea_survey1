# Analysis ----------------------------------------------------------------

theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

source(here('variable_labels.R'))

pacman::p_load(gtsummary)

label(form2_working_data$f2_sex_fct) <- "Sex"
label(form2_working_data$f2_age) <- "Age"
label(form2_working_data$f2_age_group) <- "Age group"
label(form2_working_data$f2_present_fct) <- "Is person present?"
label(form2_working_data$f2_consent_fct) <- "Does person consent?"
label(form2_working_data$f2_stool_container_fct) <- "Is stool container provided?"
label(form2_working_data$f2_questionnaire_fct) <- "Is questionnaire completed?"
label(form2_working_data$f2_stool_sample_fct) <- " Was stool sample provided?"

form2_working_data %>% filter(f2_flag==1) %>% 
  # mutate(
  #   f2_consent_fct = case_when(
  #     f2_present_fct == 'yes' ~
  #       as.character(f2_consent_fct),
  #     TRUE ~ NA_character_
  #   )
  # ) %>%
  select(f2_sex_fct, 
           f2_age_group,
           f2_present_fct,
           f2_consent_fct, 
         f2_stool_container_fct,
         f2_questionnaire_fct,
         f2_stool_sample_fct) %>% 
  tbl_summary(by = f2_age_group, 
                statistic = list(all_continuous() ~ "{mean} ({sd})",        
                                 all_categorical() ~ "{n} / {N} ({p}%)"),   
                digits = all_continuous() ~ 1,                             
                type   = all_categorical() ~ "categorical",                 
                missing_text = "Missing")   %>% 
  add_overall() 

form2_working_data %>% filter(f2_flag==1) %>% 
  select(f2_age_group,
         f2_leprosy_suspected_fct,
         f2_yaws_suspected_fct, 
         f2_ssd_suspected_fct) %>% 
  tbl_summary(by = f2_age_group, 
              statistic = list(all_continuous() ~ "{mean} ({sd})",        
                               all_categorical() ~ "{n} / {N} ({p}%)"),   
              digits = all_continuous() ~ 1,                             
              type   = all_categorical() ~ "categorical",                 
              missing_text = "Missing")   %>% 
  add_overall()
  


analysis_data %>% filter(f2_flag==1) %>% 
  filter(f2_present_fct=='yes') %>% 
  select(f2_age_group,
         f2_soap_fct,
         f2_treated_fct, 
         f2_azith_any_fct,
         f2_alb_any_fct,
         f2_ivm_any_fct,
         f2_pm_any_fct) %>% 
  tbl_summary(by = f2_age_group, 
              statistic = list(all_continuous() ~ "{mean} ({sd})",        
                               all_categorical() ~ "{n} / {N} ({p}%)"),   
              digits = all_continuous() ~ 1,                             
         #     type   = all_categorical() ~ "categorical",                 
              missing_text = "Missing")   %>% 
  add_overall() 
  #add_ci()




analysis_data %>% filter(f2_flag==1) %>% 
  select(f2_sex_fct, 
         f2_age_group,
         f2_present_fct,
         f2_consent_fct, 
         f2_stool_container_fct,
           f2_questionnaire_fct,
           f2_stool_sample_fct) %>% 
  tbl_strata(
    strata = f2_age_group,
    ~.x %>%
tbl_summary(     
  by = f2_sex_fct ,                                               # stratify entire table by outcome
  statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                   all_categorical() ~ "{n} / {N} ({p}%)"),   # stats and format for categorical columns
  digits = all_continuous() ~ 1,                              # rounding for continuous columns
  type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
  missing_text = "Missing"                                    # how missing values should display
) %>%   add_overall()
) %>%   add_overall()
  




table1(~ f2_age + 
         f2_age_group +
         f2_present_fct +
         f2_consent_fct +
         f2_stool_container_fct +
         f2_questionnaire_fct +
         f2_stool_sample_fct +
         f2_leprosy_suspected_fct +
         f2_yaws_suspected_fct +
         f2_ssd_suspected_fct +
         f2_soap_fct +
         f2_treated_fct +
         f2_azith_any_fct +
         f2_alb_any_fct +
         f2_ivm_any_fct +
         f2_pm_any_fct +
         f2_no_treat_a_fct +
         f2_no_treat_b_fct +
         f2_no_treat_c_fct | f2_sex_fct, 
       data=analysis_data %>% mutate(f2_sex_fct=fct_explicit_na(f2_sex_fct, 'Missing sex')))

table1(~ f2_age + 
         f2_age_group +
         f2_present_fct +
         f2_consent_fct +
         f2_stool_container_fct +
         f2_questionnaire_fct +
         f2_stool_sample_fct +
         f2_leprosy_suspected_fct +
         f2_yaws_suspected_fct +
         f2_ssd_suspected_fct +
         f2_soap_fct +
         f2_treated_fct +
         f2_azith_any_fct +
         f2_alb_any_fct +
         f2_ivm_any_fct +
         f2_pm_any_fct +
         f2_no_treat_a_fct +
         f2_no_treat_b_fct +
         f2_no_treat_c_fct | f2_sex_fct, 
       data=analysis_data %>% filter(f2_sex_fct %in% c('male', 'female')), 
       overall=F, extra.col=list(`P-value`=pvalue))

table1(~ f2_sex_fct +
         f2_present_fct +
         f2_consent_fct +
         f2_stool_container_fct +
         f2_questionnaire_fct +
         f2_stool_sample_fct +
         f2_leprosy_suspected_fct +
         f2_yaws_suspected_fct +
         f2_ssd_suspected_fct +
         f2_soap_fct +
         f2_treated_fct +
         f2_azith_any_fct +
         f2_alb_any_fct +
         f2_ivm_any_fct +
         f2_pm_any_fct +
         f2_no_treat_a_fct +
         f2_no_treat_b_fct +
         f2_no_treat_c_fct | f2_age_group, 
       topclass="Rtable1-grid Rtable1-zebra",
       data=analysis_data)



table1(~ skin_exam_scabies_scratching_24_fct +
         skin_exam_scabies_typical_lesions_fct +
         skin_exam_scabies_lesions_more_10_fct +
         skin_exam_scabies_skin_infection_fct +
         skin_exam_scabies_finished_fct, 
       data=analysis_data)


# Scabies -----------------------------------------------------------------

scabies_data <- skin_exam_working_data %>%
  filter(skin_exam_flag == 1) %>%
  #filter(f2_present_fct == 'yes') %>%
  select(
    skin_exam_age_group,
    skin_exam_scabies_scratching_24_fct,
    skin_exam_scabies_typical_lesions_fct,
    skin_exam_scabies_lesions_more_10_fct,
    skin_exam_scabies_skin_infection_fct,
    skin_exam_scabies_finished_fct
  )
  # mutate(skin_exam_scabies_lesions_more_10_fct=factor(case_when(
  #   skin_exam_scabies_scratching_24_fct=='no' ~ 'no itching/scratching reported', 
  #   is.na(skin_exam_scabies_lesions_more_10_fct) ~ 'question not relevant', 
  #        TRUE ~ as.character(skin_exam_scabies_lesions_more_10_fct)), levels=c('yes', 'no', 'question not relevant', 'no itching/scratching reported')))  %>% 
  # mutate(skin_exam_scabies_skin_infection_fct=factor(case_when(
  #   skin_exam_scabies_scratching_24_fct=='no' ~ 'no itching/scratching reported', 
  #   is.na(skin_exam_scabies_skin_infection_fct) ~ 'question not relevant', 
  #   TRUE ~ as.character(skin_exam_scabies_skin_infection_fct)), levels=c('yes', 'no', 'question not relevant', 'no itching/scratching reported')))  
  # mutate(
  #   skin_exam_scabies_typical_lesions_fct = case_when(
  #     skin_exam_scabies_scratching_24_fct == 'yes' ~
  #       as.character(skin_exam_scabies_typical_lesions_fct),
  #     TRUE ~ NA_character_
  #   )
  # ) %>%
  # mutate(
  #   skin_exam_scabies_lesions_more_10_fct = case_when(
  #     skin_exam_scabies_typical_lesions_fct == 'yes' ~
  #       as.character(skin_exam_scabies_lesions_more_10_fct,
  #                    TRUE ~ NA_character_)
  #   )
  # ) %>%
  # mutate(
  #   skin_exam_scabies_skin_infection_fct = case_when(
  #     skin_exam_scabies_typical_lesions_fct == 'yes' ~
  #       as.character(skin_exam_scabies_skin_infection_fct,
  #                    TRUE ~ NA_character_)
  #   )
  # ) %>%
  #mutate(across(where(is.character), ~ factor(.x, levels = c('yes', 'no'))))

label(scabies_data$skin_exam_scabies_scratching_24_fct) <- "Scabies: 1. Itching/scratching in last 24 hours"
label(scabies_data$skin_exam_scabies_typical_lesions_fct) <- "Scabies: 2. Are there typical scabies lesions"
label(scabies_data$skin_exam_scabies_lesions_more_10_fct) <- "Scabies: 3. Are there >10 typical scabies lesions"
label(scabies_data$skin_exam_scabies_skin_infection_fct) <- "Scabies: 4. Is there impetigo or infected scabies"
label(scabies_data$skin_exam_scabies_finished_fct) <- "Scabies: 5. Examination complete"

scabies_data %>% 
  tbl_summary(by = skin_exam_age_group, 
              statistic = list(all_continuous() ~ "{mean} ({sd})",        
                               all_categorical() ~ "{n} / {N} ({p}%)"),   
              #digits = all_continuous() ~ 1,                             
                   type   = all_categorical() ~ "categorical",                 
              missing_text = "no scabies lesions observed")   %>% 
  add_overall() 
  #add_ci()


# Leprosy -----------------------------------------------------------------
leprosy_data <- skin_exam_working_data %>%
  filter(skin_exam_flag == 1) %>%
  select(
    skin_exam_age_group,
    skin_exam_leprosy_self_report_fct,
    skin_exam_leprosy_saw_lesion_fct,
    skin_exam_leprosy_suspected_fct
  ) %>%
  mutate(skin_exam_leprosy_saw_lesion_fct=case_when(
    skin_exam_leprosy_self_report_fct=='yes' ~ NA_character_, 
    TRUE ~ as.character(skin_exam_leprosy_saw_lesion_fct),
  )) %>% 
  mutate(skin_exam_leprosy_suspected_fct = case_when(
      (skin_exam_leprosy_self_report_fct == 'yes' | skin_exam_leprosy_saw_lesion_fct =='yes') ~
        as.character(skin_exam_leprosy_suspected_fct),
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(across(where(is.character), ~ factor(.x, levels = c('yes', 'no'))))

label(leprosy_data$skin_exam_leprosy_self_report_fct) <- "Leprosy: 1. Self-reported white spot"
label(leprosy_data$skin_exam_leprosy_saw_lesion_fct) <- "Leprosy: 1A. If not self-reported, did nurse see signs of leprosy"
label(leprosy_data$skin_exam_leprosy_suspected_fct) <- "Leprosy: 2. Suspected"

leprosy_data %>% 
  tbl_summary(by = skin_exam_age_group, 
              statistic = list(all_continuous() ~ "{mean} ({sd})",        
                               all_categorical() ~ "{n} / {N} ({p}%)"),   
              digits = all_continuous() ~ 1,                             
              type   = all_categorical() ~ "categorical",                 
              missing_text = "Question not relevant/No response")   %>% 
  add_overall() 

# Leprosy
table1(~ skin_exam_leprosy_self_report_fct+
         skin_exam_leprosy_saw_lesion_fct+
         skin_exam_leprosy_suspected_fct,
       data=leprosy_data)

skin_exam_working_data  %>% count(skin_exam_leprosy_self_report_fct, skin_exam_leprosy_saw_lesion_fct,skin_exam_leprosy_suspected_fct)


# Severe skin disease -----------------------------------------------------

ssd_data <- skin_exam_working_data %>%
  filter(skin_exam_flag == 1) %>%
  select(skin_exam_age_group,
         skin_exam_ssd_none_fct,
           skin_exam_ssd_abscess_boil_fct,
           skin_exam_ssd_cellulitis_fct,
           skin_exam_ssd_crusted_scabies,
           skin_exam_ssd_other,
           skin_exam_ssd_other_specify)

ssd_data %>% count(skin_exam_ssd_none_fct, skin_exam_ssd_abscess_boil_fct, skin_exam_ssd_cellulitis_fct,
                   skin_exam_ssd_crusted_scabies,skin_exam_ssd_other)

label(ssd_data$f2_ssd_suspected_fct) <- "SSD: 1. Self-reported SSD "
label(ssd_data$skin_exam_ssd_none_fct) <- "SSD: 1. Self-reported SSD - None"
label(ssd_data$skin_exam_ssd_abscess_boil_fct) <- "SSD: 1. Self-reported SSD - Abscess or boil"
label(ssd_data$skin_exam_ssd_cellulitis_fct) <- "SSD: 1. Self-reported SSD - Cellulitis"
label(ssd_data$skin_exam_ssd_crusted_scabies) <- "SSD: 1. Self-reported SSD - Crusted scabies"
label(ssd_data$skin_exam_ssd_other) <- "SSD: 1. Self-reported SSD - Other"
label(ssd_data$skin_exam_ssd_other_specify) <- "SSD: 1. Self-reported SSD - Other (specify)"

ssd_data %>% 
  tbl_summary(by = skin_exam_age_group, 
              statistic = list(all_continuous() ~ "{mean} ({sd})",        
                               all_categorical() ~ "{n} / {N} ({p}%)"),   
              digits = all_continuous() ~ 1,                             
              type   = all_categorical() ~ "categorical",                 
              missing_text = "Question not relevant/No response")   %>% 
  add_overall() 



table1(~ f2_ssd_suspected_fct+
         skin_exam_ssd_none_fct+
         skin_exam_ssd_abscess_boil_fct+
         skin_exam_ssd_cellulitis_fct +
       skin_exam_ssd_crusted_scabies+
       skin_exam_ssd_other+
       skin_exam_ssd_other_specify,
       data=analysis_data)

table1(~ qPCR_result_ascaris_ct1+
       qPCR_result_ascaris_ct2+
       qPCR_ct_value_ascaris_ct1+
       qPCR_ct_value_ascaris_ct2+
       f11_ascaris_egg_count1+
       f11_ascaris_egg_count2+
       f11_ascaris_total_egg_count+
       f11_ascaris_epg+
       qPCR_result_trichuris_ct1+
       qPCR_result_trichuris_ct2+
       qPCR_ct_value_trichuris_ct1+
       qPCR_ct_value_trichuris_ct2+
       f11_trichuris_egg_count1+
       f11_trichuris_egg_count2+
       f11_trichuris_total_egg_count+
       f11_trichuris_epg+
       qPCR_result_strongyloides_ct1+
       qPCR_result_strongyloides_ct2+
       qPCR_ct_value_strongyloides_ct1+
       qPCR_ct_value_strongyloides_ct2+
       f11_hookworm_egg_count1+
       f11_hookworm_egg_count2+
       f11_hookworm_total_egg_count+
       f11_hookworm_epg+
       f11_other_sth_egg_count1+
       f11_other_sth_egg_count2+
       f11_other_sth_epg+
       qPCR_result_necator_ct1+
       qPCR_result_necator_ct2+
       qPCR_ct_value_necator_ct1+
       qPCR_ct_value_necator_ct2+
       qPCR_result_a_cey_ct1+
       qPCR_result_a_cey_ct2+
       qPCR_ct_value_a_cey_ct1+
       qPCR_ct_value_a_cey_ct2+
       qPCR_result_a_duod_ct1+
       qPCR_result_a_duod_ct2+
       qPCR_ct_value_a_duod_ct1+
       qPCR_ct_value_a_duod_ct2+
       qPCR_result_human_ct1+
       qPCR_result_human_ct2+
       qPCR_ct_value_human_ct1+
       qPCR_ct_value_human_ct2+
       f11_result_ascaris+
       f11_result_trichuris+
       f11_result_hookworm+
       qPCR_result_hookworm,
       data=analysis_data)


analysis_data %>%
  filter(!is.na(f2_village_fct)) %>% 
  group_by(f2_village_fct) %>%
  rstatix::get_summary_stats(f2_age, type = "common")

pacman::p_load(gtsummary)
analysis_data %>%  filter(f2_sex_fct %in% c('male', 'female')) %>% 
  filter(!is.na(f2_age)) %>% 
  select(f2_age, f2_sex_fct) %>%             # keep variables of interest
  tbl_summary(                               # produce summary table
    statistic = f2_age ~ "{mean} ({sd})", # specify what statistics to show
    by = f2_sex_fct) %>%                        # specify the grouping variable
  add_p(f2_age ~ "t.test")                # specify what tests to perform

format:
  docx:
  toc: true
number-sections: true
reference-doc: custom-reference-doc.docx



(all_results <- stool_receipt_qpcr_results_form11 %>%
    mutate(merge_status=case_when(is.na(f11_results_flag) ~ 'qPCR data only',
                                  is.na(qPCR_results_flag) ~ 'Sodium nitrate flotation data only', 
                                  TRUE ~ 'Data merged')) %>% 
    select(qPCR_unimelb_id,mda_code,qPCR_results_flag,f11_results_flag,
           qPCR_result_ascaris = qPCR_result_ascaris_ct1,f11_result_ascaris,f11_ascaris_total_egg_count, f11_ascaris_epg,
           qPCR_result_trichuris = qPCR_result_trichuris_ct1,f11_result_trichuris, f11_trichuris_total_egg_count, f11_trichuris_epg,
           qPCR_result_hookworm, f11_result_hookworm, f11_hookworm_total_egg_count, f11_hookworm_epg,
           merge_status) %>% 
    mutate(across(where(is.numeric), round, 1)) %>%
    mutate(across(where(is.numeric), as.character))  %>%
    mutate(mda_code=case_when((mda_code=='9_003_03_11' & f11_ascaris_epg==0) ~ '9_003_03_11_1',
                              (mda_code=='9_004_14_16' & f11_ascaris_epg==1064) ~ '9_004_14_16_1',
                              (mda_code=='9_004_17_02' & f11_ascaris_epg==0) ~ '9_004_17_02_1',
                              TRUE ~ mda_code)) %>% 
    pivot_longer(-c(qPCR_unimelb_id,mda_code,merge_status)) %>%
    mutate(method=str_extract(string = name, pattern = "[^_]+")) %>%
    mutate(pathogen=case_when(grepl('ascaris', name) ~ 'ascaris',
                              grepl('hookworm', name) ~ 'hookworm',
                              grepl('trichuris', name) ~ 'trichuris')) %>%
    mutate(indicator=case_when(grepl('result', name) ~ 'result',
                               grepl('total_egg_count', name) ~ 'total_egg_count',
                               grepl('epg', name) ~ 'eggs_per_gram')) %>%
    group_by(mda_code, pathogen,indicator, value) %>%
    select(unimelb_id=qPCR_unimelb_id,mda_code,merge_status, method, pathogen, indicator, value) %>%
    pivot_wider(names_from=c('method', 'pathogen', 'indicator'),
                values_from=value) 
)

write.csv(all_results, here('data', 'output', 'all_results_processed.csv'))