pacman::p_load(table1)
# Analysis ----------------------------------------------------------------

source(here('variable_labels.R'))


table1(~ f2_sex_fct + 
         f2_age  +
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
       f2_no_treat_c_fct, 
       data=analysis_data)

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



table1(~skin_exam_yaws_self_report_fct +
       skin_exam_yaws_saw_lesion_fct+
       skin_exam_yaws_suspected_fct+
         skin_exam_yaws_first_ulcer_fct+
       skin_exam_yaws_previous_treatment_fct+
       #skin_exam_yaws_village_arrive_6m_fct+
       #skin_exam_yaws_village_travel_6m_fct+
         skin_exam_yaws_ulcer_location_fct+
       skin_exam_yaws_dpp_line1_fct+
       skin_exam_yaws_dpp_line2_fct+
       skin_exam_yaws_dpp_linec_fct+
       skin_exam_yaws_swab_collected_fct+
       skin_exam_yaws_dpp_result_fct +
         skin_exam_yaws_finished_fct,
       data=analysis_data)


table1(~ skin_exam_leprosy_self_report_fct+
       skin_exam_leprosy_saw_lesion_fct+
       skin_exam_leprosy_suspected_fct,
       data=analysis_data)

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



