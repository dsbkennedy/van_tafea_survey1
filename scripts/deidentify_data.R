pacman::p_load(targets,digest,dplyr, openxlsx)
devtools::install_github('wilkox/deidentifyr')



##### Merged data
tar_load(analysis_data)
column_names <- names(analysis_data)
identifier <- grep('dob', column_names)

deid_merged_data <- deidentifyr::deidentify(analysis_data, 
                                            f2_participant_name,
                                            risk_factor_participant_name,
                                            risk_factor_dob)
#### Skin exam

tar_load(skin_exam_working_data)

deid_skin_exam <- deidentifyr::deidentify(skin_exam_working_data,
                                          skin_exam_participant_name,
                                          skin_exam_dob)

#### Risk factor

tar_load(risk_factor_working_data)

deid_risk_factor <- deidentifyr::deidentify(risk_factor_working_data, 
                                            risk_factor_participant_name,
                                            risk_factor_dob)

#### form 2

tar_load(form2_working_data)

deid_form2 <- form2_working_data %>% select(-f2_participant_name)

### form 3.1 

tar_load(form3.1_working_data)

deid_form3.1 <- form3.1_working_data


### form 10

tar_load(form10_working_data)

deid_form10 <- form10_working_data

### form 11 

tar_load(form11_working_data)

deid_form11 <- form11_working_data

#### qpcr results

tar_load(qpcr_results_working_data)

deid_qpcr_results <- qpcr_results_working_data

list_of_datasets <- list("Merged_data" = deid_merged_data, 
                         "Form_2" = deid_form2,
                         "Form_3.1" = deid_form3.1,
                         "Form_10" = deid_form10,
                         "Form_11" = deid_form11,
                         "qPCR_results" = deid_qpcr_results,
                         "Skin_exam" = deid_skin_exam,
                         "Risk_factor" = deid_risk_factor)

write.xlsx(list_of_datasets, file = "outputs/data_cut_221130/all.xlsx")

