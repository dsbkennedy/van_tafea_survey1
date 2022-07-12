
# Variable labels ---------------------------------------------------------

label(analysis_data$f2_sex_fct) <- "Sex"
label(analysis_data$f2_age) <- "Age"
label(analysis_data$f2_age_group) <- "Age group"
label(analysis_data$f2_present_fct) <- "Is person present?"
label(analysis_data$f2_consent_fct) <- "Does person consent?"
label(analysis_data$f2_stool_container_fct) <- "Is stool container provided?"
label(analysis_data$f2_questionnaire_fct) <- "Is questionnaire completed?"
label(analysis_data$f2_stool_sample_fct) <- " Was stool sample provided?"
label(analysis_data$f2_leprosy_suspected_fct) <- "Is leprosy suspected?"
label(analysis_data$f2_yaws_suspected_fct) <- "Is yaws suspected?"
label(analysis_data$f2_ssd_suspected_fct) <- "Is severe skin disease suspected?"
label(analysis_data$f2_soap_fct) <- "Soap given?"
label(analysis_data$f2_treated_fct) <- "Did participant receive MDA?"
label(analysis_data$f2_azith_any_fct) <- "Did participant receive azithromycin?"
label(analysis_data$f2_alb_any_fct) <- "Did participant receive albendazole?"
label(analysis_data$f2_ivm_any_fct) <- "Did participant receive ivermectin?"
label(analysis_data$f2_pm_any_fct) <- "Did participant receive permethrin?"
label(analysis_data$f2_no_treat_a_fct) <- "Participant excluded (A)"
label(analysis_data$f2_no_treat_b_fct) <- "Participant excluded (B)"
label(analysis_data$f2_no_treat_c_fct) <- "Participant excluded (C)"

label(analysis_data$skin_exam_scabies_scratching_24_fct) <- "Scabies: 1. Itching/scratching in last 24 hours"
label(analysis_data$skin_exam_scabies_typical_lesions_fct) <- "Scabies: 2. Are there typical scabies lesions"
label(analysis_data$skin_exam_scabies_lesions_more_10_fct) <- "Scabies: 3. Are there >10 typical scabies lesions"
label(analysis_data$skin_exam_scabies_skin_infection_fct) <- "Scabies: 4. Is there impetigo or infected scabies"
label(analysis_data$skin_exam_scabies_finished_fct) <- "Scabies: 5. Examination complete"

label(analysis_data$skin_exam_yaws_self_report_fct) <- "Yaws: 1. Self-reported ulcer/bump"
label(analysis_data$skin_exam_yaws_saw_lesion_fct) <- "Yaws: 1A. If not self-reported, did nurse see yaws-like lesion"
label(analysis_data$skin_exam_yaws_suspected_fct) <- "Yaws: 2. Suspected"
label(analysis_data$skin_exam_yaws_first_ulcer_fct) <- "Yaws: 3. First time participant has had ulcer"
label(analysis_data$skin_exam_yaws_previous_treatment_fct) <- "Yaws: 4A. Did participant take treatment for previous episode"
label(analysis_data$skin_exam_yaws_ulcer_location_fct) <- "Yaws: 7. Location of chosen ulcer"
label(analysis_data$skin_exam_yaws_dpp_line1_fct) <- "Yaws: 8a. DPP - Line 1"
label(analysis_data$skin_exam_yaws_dpp_line2_fct) <- "Yaws: 8a. DPP - Line 2"
label(analysis_data$skin_exam_yaws_dpp_linec_fct) <- "Yaws: 8a. DPP - Line C"
label(analysis_data$skin_exam_yaws_swab_collected_fct) <- "Yaws: 9. Swab collected"
label(analysis_data$skin_exam_yaws_dpp_result_fct) <- "Yaws: 9. Reason for no swab collection"
label(analysis_data$skin_exam_yaws_finished_fct) <- "Yaws: 10. Examination complete"

label(analysis_data$skin_exam_leprosy_self_report_fct) <- "Leprosy: 1. Self-reported white spot"
label(analysis_data$skin_exam_leprosy_saw_lesion_fct) <- "Leprosy: 1A. If not self-reported, did nurse see signs of leprosy"
label(analysis_data$skin_exam_leprosy_suspected_fct) <- "Leprosy: 2. Suspected"

label(analysis_data$f2_ssd_suspected_fct) <- "SSD: 1. Self-reported SSD "
label(analysis_data$skin_exam_ssd_none_fct) <- "SSD: 1. Self-reported SSD - None"
label(analysis_data$skin_exam_ssd_abscess_boil_fct) <- "SSD: 1. Self-reported SSD - Abscess or boil"
label(analysis_data$skin_exam_ssd_cellulitis_fct) <- "SSD: 1. Self-reported SSD - Cellulitis"
label(analysis_data$skin_exam_ssd_crusted_scabies) <- "SSD: 1. Self-reported SSD - Crusted scabies"
label(analysis_data$skin_exam_ssd_other) <- "SSD: 1. Self-reported SSD - Other"
label(analysis_data$skin_exam_ssd_other_specify) <- "SSD: 1. Self-reported SSD - Other (specify)"


pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}
