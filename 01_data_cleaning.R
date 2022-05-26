library(here)
library(tidyverse)
library(linelist)
library(janitor)
library(readxl)

source(here('labels_levels.R'))


# Risk factor -------------------------------------------------------------
risk_factor_raw <- here('data','input', 'Tafea_risk factor questionnaire_FOR STATA.xlsx')
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
                              labels = ),
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

skin_exam_raw <- here('data','input', 'Tafea_skin exam form_FOR STATA.xlsx')
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

  









library(skimr)
skim(risk_factor_processed)
  
risk_factor_processed %>% tabyl(roof_tiles_fct)

summary(risk_factor_processed)

risk_factor_processed$washing_water_all_year_time_specify



# DBS ---------------------------------------------------------------------
dbs_raw <- here('data','input', 'Tafea_DBS sample IDs_FOR STATA.xlsx')
dbs_raw <- read_xlsx(dbs_raw)



# Form 2 ------------------------------------------------------------------

form2_1_raw <- here('data','input', 'Tafea_form 2_data entry person 1_FOR STATA.xlsx')
form2_1_raw <- read_xlsx(form2_1_raw)


(
  form2_1_processed <- form2_1_raw %>%
    mutate(
      f2_1_sex_fct = factor(f2_1_sex, levels = c('M', 'F'), 
                       labels = c("Male", "Female")),
      f2_1_area_council_fct = factor(f2_1_area_council, levels = area_council_names),
      f2_1_village_fct = factor(f2_1_village, levels = c(village_names)),
      across(
        form2_1_yesno,
        factor,
        levels = c('N', 'Y'),
        labels=c('No', "Yes"),
        .names = "{.col}_fct"
      ))
)

form2_2_raw <- here('data','input', 'Tafea_form 2_data entry person 2_FOR STATA.xlsx')
form2_2_raw <- read_xlsx(form2_2_raw)


(
  form2_2_processed <- form2_2_raw %>%
    mutate(
      f2_2_sex_fct = factor(f2_2_sex, levels = c('M', 'F'), 
                            labels = c("Male", "Female")),
      f2_2_area_council_fct = factor(f2_2_area_council, levels = area_council_names),
      f2_2_village_fct = factor(f2_2_village, levels = c(village_names)),
      across(
        form2_2_yesno,
        factor,
        levels = c('N', 'Y'),
        labels=c('No', "Yes"),
        .names = "{.col}_fct"
      ))
)




# Form 3 ------------------------------------------------------------------

form3_2_raw <- here('data','input', 'Tafea_form 3_FOR STATA.xlsx')
form3_2_raw <- read_xlsx(form3_2_raw)


(
  form3_2_processed <- form3_2_raw %>%
    mutate(
      f3_2_sex_fct = factor(f3_2_sex, levels = c('M', 'F'), 
                            labels = c("Male", "Female")),
      f3_2_area_council_fct = factor(f3_2_area_council, levels = area_council_names),
      f3_2_village_fct = factor(f3_2_village_school, levels = c(village_names)),
      across(
        form3_2_yesno,
        factor,
        levels = c('N', 'Y'),
        labels=c('No', "Yes"),
        .names = "{.col}_fct"
      ))
)
