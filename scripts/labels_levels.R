village_names <- c(
  "karimasanga",
  "koraioken",
  "lounapektuan",
  "anowunamlao",
  "ikurakau",
  "imarkakak",
  "imaru",
  "ipau",
  "ishia",
  "isiai",
  "loearfi",
  "loueao",
  "lounelapen",
  "louniel",
  "lowkaru",
  "lownasunan",
  "yakunaus"
)

lower_village_names <- c(
  "karimasanga",
  "koraioken",
  "lounapektuan",
  "anowunamlao",
  "ikurakau",
  "imarkakak",
  "imaru",
  "ipau",
  "ishia",
  "isiai",
  "loearfi",
  "loueao",
  "lounelapen",
  "louniel",
  "lowkaru",
  "lownasunan",
  "yakunaus"
)

area_council_names <- c(
  'aneityum',
  'south west t',
  'whitesands',
  'south tanna',
  'middle bush',
  'futuna',
  'west tanna'
)

province_names <- c('Tafea',
                    'Sanma',
                    'Shefa')

yes_no_factor_names <- c(
  'head_of_household',
  'goods_electricity',
  'goods_radio',
  'goods_television',
  'goods_cellphone',
  'goods_landline',
  'goods_fridge',
  'goods_solar_panel',
  'goods_car_truck',
  'goods_bicycle',
  'goods_motorbike',
  'goods_rowboat_canoe',
  'goods_motorboat',
  'goods_animal_cart',
  'goods_land',
  'animals_horses_fenced',
  'animals_cows_fenced',
  'animals_pigs_fenced',
  'animals_sheepgoat_fenced',
  'animals_chickens_fenced',
  'animals_ducks_fenced',
  'animals_dogs_sleep_indoors',
  'animals_cats_sleep_indoors',
  'drinking_washing_same',
  'washing_water_all_year',
  'san_hh_toilet',
  'san_hh_toilet_use',
  'san_od',
  'hyg_hw_station',
  'hyg_hw_obs_soap',
  'hyg_hw_obs_water'
)

checked_unchecked_factor <- c(
  'walls_strawbamboo',
  'walls_brick',
  'walls_wood',
  'walls_ironsheet',
  'walls_other',
  'roof_thatch_vegetation',
  'roof_corrugated_metal_iron_sheet',
  'roof_tiles',
  'roof_other',
  'floor_earth_ground',
  'floor_concrete',
  'floor_ceramic_tiles',
  'floor_wood',
  'floor_other',
  'san_hh_toilet_no_water',
  'san_hh_toilet_no_dirty',
  'san_hh_toilet_no_broken',
  'san_hh_toilet_no_overflow',
  'san_hh_toilet_no_other',
  'san_hh_toilet_no_refused',
  'san_hh_toilet_obs_clean',
  'san_hh_toilet_obs_water',
  'san_hh_toilet_obs_faeces',
  'san_hh_toilet_obs_flies',
  'san_hh_toilet_ob_oth',
  'san_hh_toilet_obs_nopermission'
)

bednet_vars <-
  c('bed_net_raining_season_all', 'bed_net_raining_season_one')

drinking_water_vars <-
  c('drinking_water_source', 'washing_water_source')

education_levels <- c(
  'Neva go lo skul',
  "Kasem primary skul be no komplitim",
  "Primary skul",
  "Secondary skul",
  "Kolej (e.g nursing, teaching, police or religious)",
  "University",
  "No save"
)

education_labels <- c(
  "Never went to school",
  "Attended primary school but did not complete",
  "Primary school",
  "Secondary school",
  "College (e.g nursing, teaching, police or religious)",
  "University",
  "Don't know"
)

occupation_levels <- c(
  "Farma",
  "Fisherman / Fisherwoman",
  "Wok lo ofis olsem Clerk/administresen ofisa",
  "Helt woka",
  "Tija",
  "Polis",
  "Self-employed/business owner (inkludum salem lo market)",
  "Labour/manual woka (e.g Builder)",
  "Domestic worker (e.g house keeper, home maker)",
  "Skul pikinini",
  "No kat wok/Unemployed",
  "Narafala wok",
  "No save"
)
occupation_labels <-
  c(
    "Farmer",
    "Fisherman/woman" ,
    "Clerk/administration" ,
    "Health worker" ,
    "Teacher",
    "Police",
    "Self-employed/business owner (including selling at the market)" ,
    "Labourer/manual worker (e.g. builder)" ,
    "Domestic worker (e.g house keeper, home maker)" ,
    "Student" ,
    "Unemployed" ,
    "Other" ,
    "Don't know"
  )

income_levels <- c(
  "Less than 50,000 VTU" ,
  "50,000 - 99,999 VTU" ,
  "100,000 - 199,999 VTU" ,
  "Bitim 200,000 VTU" ,
  "No save"
)

income_labels <- c(
  "Less than 50,000 VTU" ,
  "50,000 - 99,999 VTU" ,
  "100,000 - 199,999 VTU" ,
  "200,000 VTU or more" ,
  "Dont know"
)

drinking_water_levels <-
  c(
    'Piped wota lo hao o yard',
    'Public tap o standpaep o Tubewell/borehole',
    'Protected dug well',
    "Protected spring",
    "Rainwater",
    "Unprotected dug well",
    "Unprotected spring",
    "Surface water (e.g, lake, river, stream, pond, canals, irrigation ditches)",
    "Narafala, spesifae:",
    'No save'
  )

drinking_water_labels <-
  c(
    "Piped water to your house or yard" ,
    "Public tap or standpipe or Tubewell/borehole" ,
    "Protected dug well",
    "Protected spring" ,
    "Rainwater (collected into tank)" ,
    "Unprotected dug well" ,
    "Unprotected spring" ,
    "Surface water (e.g, lake, river, stream, pond, canals, irrigation ditches)" ,
    "Other, specify:" ,
    "Don't know"
  )

hyg_levels <-
  c(
    "Sink/tap klosap lo toilet",
    "Sink/tap klosap lo kitchen",
    "Sink/tap stap lo narafala lokesen",
    "Mobile (backet/jag/kettle)",
    "Narafala, spesifae",
    "No save chekem / no allow blo chekem"
  )

hyb_labels <-
  c(
    "Sink/tap close to toilet" ,
    "Sink/tap close to the kitchen" ,
    "Sink/tap other location",
    "Mobile (bucket/jug/kettle)" ,
    "Other (Specify)" ,
    "None/no permission to see"
  )

toilet_levels <- c(
  'Flash toilet',
  'Pit latrine',
  'Compos toilet',
  'Backet latrine',
  'Hanging latrine',
  'Narafala (Spesifae):',
  'No allow blo chekem',
  'Nokat toilet'
)

toilet_labels <-
  c(
    "Flush toilet",
    "Pit latrine",
    "Composting toilet" ,
    "Bucket latrine",
    "Hanging latrine" ,
    "Other (Specify)" ,
    "No permission to see" ,
    "No toilet"
  )

pit_levels <- c(
  'No flash (direct pit)',
  'Flash wetem wota (offset pit)',
  'Flash wetem wota (flash lo narapls)'
)

pit_labels <- c(
  'Does not flush (direct pit)',
  'Flush with water (offset pit)',
  'Flush with water (flush elsewhere)'
)

bednet_levels <- c('Neva', 'Sam naet', 'Fulap naet', 'Everi naet')
bednet_labels <-
  c("Never" , "Some nights" , "Most nights" , "Every night")

bednet_lastnight_levels <-
  c('No - nokat wan', 'Yes - samfala', 'Yes  - everiwan', 'Nosave')
bednet_lastnight_labels <-
  c("No - none of us" , "Yes - some of us" , "Yes - all of us" , "Don't know'")

skin_varlist <- c(
  'scabies_scratching_24',
  'scabies_typical_lesions',
  'scabies_lesions_more_10',
  'scabies_skin_infection',
  'scabies_finished',
  'yaws_self_report',
  'yaws_saw_lesion',
  'yaws_suspected',
  'yaws_previous_treatment',
  'yaws_dpp_line1',
  'yaws_dpp_line2',
  'yaws_dpp_linec',
  'yaws_swab_collected',
  'yaws_finished',
  'leprosy_self_report',
  'leprosy_saw_lesion',
  'leprosy_suspected'
)

skin_checklist <- c('ssd_none',
                    'ssd_abscess_boil', 'ssd_cellulitis')

drug_fct_levels <- c('Azithromycin',
                     'BP',
                     'Other (Specify)',
                     'Dont know')

ulcer_location_levels <- c(
  "Left upper arm",
  "Left lower arm",
  "Left upper leg",
  "Left lower leg",
  "Left ankle" ,
  "Right upper arm",
  "Right lower arm",
  "Right upper leg",
  "Right lower leg",
  "Right ankle" ,
  "Other (specify)"
)

ulcer_location_labels <- c(
  "Left upper arm",
  "Left lower arm",
  "Left upper leg",
  "Left lower leg",
  "Left ankle",
  "Right upper arm",
  "Right lower arm" ,
  "Right upper leg" ,
  "Right lower leg" ,
  "Right ankle",
  "Other (specify)"
)

dpp_result_levels <-
  c(
    'Positif: Aktive Yaws (3 line ikam aot: 1, 2 & C)',
    "Old/trited yaws infeksen -  2 lines ikam aot (1 & C)",
    "Negatif: No Yaws (Line C  nomo ikam aot)",
    "Fols Positif (2 lines ikam aot: 2 & C)"
  )

dpp_result_labels <- c("Positive (active Yaws)",
                       "Old/treated Yaws" ,
                       "Negative (No Yaws)",
                       "False positive")

form2_1_yesno <- c(
  'f2_1_hh',
  'f2_1_present',
  'f2_1_consent',
  'f2_1_stool_container',
  'f2_1_questionnaire',
  'f2_1_stool_sample',
  'f2_1_leprosy_suspected',
  'f2_1_yaws_suspected',
  'f2_1_ssd_suspected',
  'f2_1_soap',
  'f2_1_treated',
  'f2_1_azith_1tab',
  'f2_1_azith_2tab',
  'f2_1_azith_3tab',
  'f2_1_azith_4tab',
  'f2_1_alb_0.5tab',
  'f2_1_alb_sac_1tab',
  'f2_1_alb_wcba_1tab',
  'f2_1_ivm_1tab',
  'f2_1_ivm_2tab',
  'f2_1_ivm_3tab',
  'f2_1_ivm_4tab',
  'f2_1_pm_<2m',
  'f2_1_pm_2m-12yr',
  'f2_1_pm_other',
  'f2_1_no_treat_a',
  'f2_1_no_treat_b',
  'f2_1_no_treat_c'
)

form2_1_yesno_corrected <- c(
  'f2_hh_fct',
  'f2_consent_fct',
  'f2_stool_container_fct',
  'f2_stool_sample_fct',
  'f2_leprosy_suspected_fct',
  'f2_yaws_suspected_fct',
  'f2_ssd_suspected_fct',
  'f2_soap_fct',
  'f2_treated_fct',
  'f2_alb_0_5tab_fct',
  'f2_alb_sac_1tab_fct',
  'f2_alb_wcba_1tab_fct',
  'f2_ivm_1tab_fct',
  'f2_ivm_2tab_fct',
  'f2_ivm_3tab_fct',
  'f2_ivm_4tab_fct',
  'f2_pm_2m_12yr_fct',
  'f2_no_treat_a_fct'
)

form2_2_yesno <- c(
  'f2_2_hh',
  'f2_2_present',
  'f2_2_consent',
  'f2_2_stool_container',
  'f2_2_questionnaire',
  'f2_2_stool_sample',
  'f2_2_leprosy_suspected',
  'f2_2_yaws_suspected',
  'f2_2_ssd_suspected',
  'f2_2_soap',
  'f2_2_treated',
  'f2_2_azith_1tab',
  'f2_2_azith_2tab',
  'f2_2_azith_3tab',
  'f2_2_azith_4tab',
  'f2_2_alb_0.5tab',
  'f2_2_alb_sac_1tab',
  'f2_2_alb_wcba_1tab',
  'f2_2_ivm_1tab',
  'f2_2_ivm_2tab',
  'f2_2_ivm_3tab',
  'f2_2_ivm_4tab',
  'f2_2_pm_<2m',
  'f2_2_pm_2m-12yr',
  'f2_2_pm_other',
  'f2_2_no_treat_a',
  'f2_2_no_treat_b',
  'f2_2_no_treat_c'
)


form3_1_yesno <- c('f3_1_ssd_suspected',
                   'f3_1_scabies_suspected',
                   'f3_1_yaws_suspected')

form3_2_yesno <- c('f3_2_ssd_suspected',
                   'f3_2_scabies_suspected',
                   'f3_2_yaws_suspected')

ethanol_f31_vars <- c('f31_ethanol', 'f31_pd')




# Risk factor variable labels  --------------------------------------------

# lab var record_id "Record ID"
# lab var interviewer "1. Interviewer name"
# lab var interview_date "2. Interview date"
# lab var latitude "3. Latitude"
# lab var longitude "3. Longitude"
# lab var province "4. Province"
# lab var  area_council "5. Area Council"
# lab var village "6. Village"
# lab var participant_name "7. Participant name"
# lab var mda_code "8. MDA ID"
# lab var household_id "9. Household ID"
# lab var dob "10. Date of birth"
# lab var age "11. Age (years)"
# lab var sex "12. Sex"
# lab var household_occupants "13. number household occupants"
# lab var head_of_household "14. HH: is person HH"
# lab var relationship_to_head "15. HH: respondent's relationship to HH"
# lab var head_education "16. HH: education level completed"
# lab var head_occupation "17. HH: occupation"
# lab var head_occupation_other "17. HH: occupation (other)"
# lab var head_income "18. HH: Income"
# lab var goods_electricity "19. HH goods: electricity"
# lab var goods_radio "19. HH goods: radio"
# lab var goods_television "19. HH goods: television"
# lab var goods_cellphone "19. HH goods: cellphone"
# lab var goods_smartphone "19. HH goods: smartphone"
# lab var goods_landline "19. HH goods: landline"
# lab var goods_fridge "19. HH goods: fridge"
# lab var goods_solar_panel "19. HH goods: solar panel"
# lab var goods_car_truck "19. HH goods: car or truck"
# lab var goods_bicycle "19. HH goods: bicycle"
# lab var goods_motorbike "19. HH goods: motorbike"
# lab var goods_rowboat_canoe "19. HH goods: rowboat/canoe"
# lab var goods_motorboat "19. HH goods: motorboat"
# lab var goods_animal_cart "19. HH goods: animal-drawn cart"
# lab var goods_land "19. HH goods: agricultural lands"
# lab var animals_horses "20. Animals: horses"
# lab var animals_horses_fenced "20. Animals: horses (fenced?)"
# lab var animals_cows "20. Animals: cows"
# lab var animals_cows_fenced "20. Animals: cows (fenced?)"
# lab var animals_pigs "20. Animals: pigs"
# lab var animals_pigs_fenced "20. Animals: pigs (fenced?)"
# lab var animals_sheepgoat "20. Animals: sheep or goats"
# lab var animals_sheepgoat_fenced "20. Animals: sheep or goats (fenced?)"
# lab var animals_chickens "20. Animals: chickens"
# lab var animals_chickens_fenced "20. Animals: chickens (fenced?)"
# lab var animals_ducks "20. Animals: ducks"
# lab var animals_ducks_fenced "20. Animals: ducks (fenced?)"
# lab var animals_dogs "20. Animals: dogs"
# lab var animals_dogs_sleep_indoors "20. Animals: dogs (sleep indoors?)"
# lab var animals_cats "20. Animals: cats"
# lab var animals_cats_sleep_indoors "20. Animals: cats (sleep indoors?)"
# lab var walls_strawbamboo "21. House: walls = straw/bamboo"
# lab var walls_brick "21. House: walls = brick"
# lab var walls_wood "21. House: walls = wood"
# lab var walls_ironsheet "21. House: walls = iron sheet"
# lab var walls_other "21. House: walls = other"
# lab var walls_other_specify "21. House: walls = other (specify)"
# lab var roof_thatch_vegetation "21. House: roof = vegetation"
# lab var roof_corrugated_metal_iron_sheet "21. House: roof = corrugated metal iron"
# lab var roof_tiles "21. House: roof = tiles"
# lab var roof_other "21. House: roof = other"
# lab var roof_other_specify "21. House: roof = other (specify)"
# lab var floor_earth_ground "21. House: floor = earth/ground"
# lab var floor_concrete "21. House: floor = concrete"
# lab var floor_ceramic_tiles "21. House: floor = tiles"
# lab var floor_wood "21. House: floor = wood/timber"
# lab var floor_other "21. House: floor = other"
# lab var floor_other_specify "21. House: floor = other (specify)"
# lab var sleeping_places "22. Number sleeping places "
# lab var bed_net_total "23. Bednets: total in household"
# lab var bed_net_llit "24. Bednets: total LLINs"
# lab var bed_net_raining_season_all "25. Bednets: raining season - how often all sleep under bednet"
# lab var bed_net_raining_season_one "26. Bednets: raining season - how often at least one sleep under bednet"
# lab var bed_net_last_night "27. Bednets: last night - how many sleep under"
# lab var bed_net_observe "28. Bednets: observe - can interviewer see at least one"
# lab var drinking_water_source "29. Water: drinking - main source"
# lab var drinking_water_source_specify "29. Water: drinking - main source (specify)"
# lab var drinking_water_time "30. Water: drinking - time to collect"
# lab var drinking_washing_same "31. Water: drinking and washing source the same"
# lab var washing_water_source "32. Water: washing - main source"
# lab var washing_water_source_specify "32. Water: washing - main source (specify)"
# lab var washing_water_time "33. Water: washing - time to collect"
# lab var washing_water_all_year "34. Water: washing - available throughout the year"
# lab var washing_water_all_year_time "35. Water: washing - amount not available"
# lab var washing_water_all_year_time_spec "35. Water: washing - months/days/weeks not available"
# lab var san_hh_toilet "36. Sanitation: does household have toilet"
# lab var san_hh_toilet_use "37. Sanitation: is toilet in use"
# lab var san_hh_toilet_no_water "38. Sanitation: toilet not in use - no water"
# lab var san_hh_toilet_no_dirty "38. Sanitation: toilet not in use - dirty"
# lab var san_hh_toilet_no_broken "38. Sanitation: toilet not in use - broken"
# lab var san_hh_toilet_no_overflow "38. Sanitation: toilet not in use - overflowing"
# lab var san_hh_toilet_no_other "38. Sanitation: toilet not in use - other"
# lab var san_hh_toilet_no_refused "38. Sanitation: toilet not in use - refused"
# lab var san_hh_toilet_other_specify "38. Sanitation: toilet not in use - other (specify)"
# lab var san_od "39: Sanitation: practices open defecation"
# lab var hyg_hw_station "40. Hygiene: household handwashing station"
# lab var hyg_hw_obs "41. Hygiene: observation - handwashing type"
# lab var hyg_hw_obs_spe "41. Hygiene: observation - handwashing type (other)"
# lab var hyg_hw_obs_soap "42. Hygiene: observation - soap"
# lab var hyg_hw_obs_water "43. Hygiene: observation - water"
# lab var san_hh_toilet_obs "44. Sanitation: observation - toilet type"
# lab var san_hh_toilet_obs_other "44. Sanitation: observation - toilet type (other)"
# lab var san_hh_toilet_obs_pit_slab "44. Sanitation: observation - pit latrine slab"
# lab var san_hh_toilet_obs_pit_flush "44. Sanitation: observation - pit latrine flush"
# lab var san_hh_toilet_obs_clean "45. Sanitation: observation - toilet clean"
# lab var san_hh_toilet_obs_water "45. Sanitation: observation - toilet water available"
# lab var san_hh_toilet_obs_faeces "45. Sanitation: observation - urine/feces around"
# lab var san_hh_toilet_obs_flies "45. Sanitation: observation - flies present"
# lab var san_hh_toilet_ob_oth "45. Sanitation: observation - toilet obs (other)"
# lab var san_hh_toilet_obs_nopermission "45. Sanitation: observation - no permission to see toilet"
# lab var san_hh_toilet_obs_other_spec "45. Sanitation: observation - toilet obs (specify)"
# lab var complete "Interview complete"



# Skin exam variable labels -----------------------------------------------

# lab var record_id "Record ID"
# lab var interviewer "1. Interviewer name"
# lab var interview_date "2. Interview date"
# lab var latitude "3. Latitude"
# lab var longitude "3. Longitude"
# lab var province "4. Province"
# lab var  area_council "5. Area Council"
# lab var village "6. Village"
# lab var participant_name "7. Participant name"
# lab var mda_code "8. MDA ID"
# lab var household_id "9. Household ID"
# lab var dob "10. Date of birth"
# lab var age "11. Age (years)"
# lab var sex "12. Sex"
# lab var household_occupants "13. number household occupants"
# lab var head_of_household "14. HH: is person HH"
# lab var relationship_to_head "15. HH: respondent's relationship to HH"
# lab var head_education "16. HH: education level completed"
# lab var head_occupation "17. HH: occupation"
# lab var head_occupation_other "17. HH: occupation (other)"
# lab var head_income "18. HH: Income"
# lab var goods_electricity "19. HH goods: electricity"
# lab var goods_radio "19. HH goods: radio"
# lab var goods_television "19. HH goods: television"
# lab var goods_cellphone "19. HH goods: cellphone"
# lab var goods_smartphone "19. HH goods: smartphone"
# lab var goods_landline "19. HH goods: landline"
# lab var goods_fridge "19. HH goods: fridge"
# lab var goods_solar_panel "19. HH goods: solar panel"
# lab var goods_car_truck "19. HH goods: car or truck"
# lab var goods_bicycle "19. HH goods: bicycle"
# lab var goods_motorbike "19. HH goods: motorbike"
# lab var goods_rowboat_canoe "19. HH goods: rowboat/canoe"
# lab var goods_motorboat "19. HH goods: motorboat"
# lab var goods_animal_cart "19. HH goods: animal-drawn cart"
# lab var goods_land "19. HH goods: agricultural lands"
# lab var animals_horses "20. Animals: horses"
# lab var animals_horses_fenced "20. Animals: horses (fenced?)"
# lab var animals_cows "20. Animals: cows"
# lab var animals_cows_fenced "20. Animals: cows (fenced?)"
# lab var animals_pigs "20. Animals: pigs"
# lab var animals_pigs_fenced "20. Animals: pigs (fenced?)"
# lab var animals_sheepgoat "20. Animals: sheep or goats"
# lab var animals_sheepgoat_fenced "20. Animals: sheep or goats (fenced?)"
# lab var animals_chickens "20. Animals: chickens"
# lab var animals_chickens_fenced "20. Animals: chickens (fenced?)"
# lab var animals_ducks "20. Animals: ducks"
# lab var animals_ducks_fenced "20. Animals: ducks (fenced?)"
# lab var animals_dogs "20. Animals: dogs"
# lab var animals_dogs_sleep_indoors "20. Animals: dogs (sleep indoors?)"
# lab var animals_cats "20. Animals: cats"
# lab var animals_cats_sleep_indoors "20. Animals: cats (sleep indoors?)"
# lab var walls_strawbamboo "21. House: walls = straw/bamboo"
# lab var walls_brick "21. House: walls = brick"
# lab var walls_wood "21. House: walls = wood"
# lab var walls_ironsheet "21. House: walls = iron sheet"
# lab var walls_other "21. House: walls = other"
# lab var walls_other_specify "21. House: walls = other (specify)"
# lab var roof_thatch_vegetation "21. House: roof = vegetation"
# lab var roof_corrugated_metal_iron_sheet "21. House: roof = corrugated metal iron"
# lab var roof_tiles "21. House: roof = tiles"
# lab var roof_other "21. House: roof = other"
# lab var roof_other_specify "21. House: roof = other (specify)"
# lab var floor_earth_ground "21. House: floor = earth/ground"
# lab var floor_concrete "21. House: floor = concrete"
# lab var floor_ceramic_tiles "21. House: floor = tiles"
# lab var floor_wood "21. House: floor = wood/timber"
# lab var floor_other "21. House: floor = other"
# lab var floor_other_specify "21. House: floor = other (specify)"
# lab var sleeping_places "22. Number sleeping places "
# lab var bed_net_total "23. Bednets: total in household"
# lab var bed_net_llit "24. Bednets: total LLINs"
# lab var bed_net_raining_season_all "25. Bednets: raining season - how often all sleep under bednet"
# lab var bed_net_raining_season_one "26. Bednets: raining season - how often at least one sleep under bednet"
# lab var bed_net_last_night "27. Bednets: last night - how many sleep under"
# lab var bed_net_observe "28. Bednets: observe - can interviewer see at least one"
# lab var drinking_water_source "29. Water: drinking - main source"
# lab var drinking_water_source_specify "29. Water: drinking - main source (specify)"
# lab var drinking_water_time "30. Water: drinking - time to collect"
# lab var drinking_washing_same "31. Water: drinking and washing source the same"
# lab var washing_water_source "32. Water: washing - main source"
# lab var washing_water_source_specify "32. Water: washing - main source (specify)"
# lab var washing_water_time "33. Water: washing - time to collect"
# lab var washing_water_all_year "34. Water: washing - available throughout the year"
# lab var washing_water_all_year_time "35. Water: washing - amount not available"
# lab var washing_water_all_year_time_spec "35. Water: washing - months/days/weeks not available"
# lab var san_hh_toilet "36. Sanitation: does household have toilet"
# lab var san_hh_toilet_use "37. Sanitation: is toilet in use"
# lab var san_hh_toilet_no_water "38. Sanitation: toilet not in use - no water"
# lab var san_hh_toilet_no_dirty "38. Sanitation: toilet not in use - dirty"
# lab var san_hh_toilet_no_broken "38. Sanitation: toilet not in use - broken"
# lab var san_hh_toilet_no_overflow "38. Sanitation: toilet not in use - overflowing"
# lab var san_hh_toilet_no_other "38. Sanitation: toilet not in use - other"
# lab var san_hh_toilet_no_refused "38. Sanitation: toilet not in use - refused"
# lab var san_hh_toilet_other_specify "38. Sanitation: toilet not in use - other (specify)"
# lab var san_od "39: Sanitation: practices open defecation"
# lab var hyg_hw_station "40. Hygiene: household handwashing station"
# lab var hyg_hw_obs "41. Hygiene: observation - handwashing type"
# lab var hyg_hw_obs_spe "41. Hygiene: observation - handwashing type (other)"
# lab var hyg_hw_obs_soap "42. Hygiene: observation - soap"
# lab var hyg_hw_obs_water "43. Hygiene: observation - water"
# lab var san_hh_toilet_obs "44. Sanitation: observation - toilet type"
# lab var san_hh_toilet_obs_other "44. Sanitation: observation - toilet type (other)"
# lab var san_hh_toilet_obs_pit_slab "44. Sanitation: observation - pit latrine slab"
# lab var san_hh_toilet_obs_pit_flush "44. Sanitation: observation - pit latrine flush"
# lab var san_hh_toilet_obs_clean "45. Sanitation: observation - toilet clean"
# lab var san_hh_toilet_obs_water "45. Sanitation: observation - toilet water available"
# lab var san_hh_toilet_obs_faeces "45. Sanitation: observation - urine/feces around"
# lab var san_hh_toilet_obs_flies "45. Sanitation: observation - flies present"
# lab var san_hh_toilet_ob_oth "45. Sanitation: observation - toilet obs (other)"
# lab var san_hh_toilet_obs_nopermission "45. Sanitation: observation - no permission to see toilet"
# lab var san_hh_toilet_obs_other_spec "45. Sanitation: observation - toilet obs (specify)"
# lab var complete "Interview complete"


# Form 3 variable labels --------------------------------------------------

# lab var f3_2_area_council "Area Council"
# lab var f3_2_health_zone "Health Zone"
# lab var f3_2_team_no "Team number"
# lab var f3_2_date "Date (not sure what of!)"
# lab var f3_2_page "MDA book - page number"
# lab var mda_code "Demographic: MDA ID"
# lab var f3_2_participant_name "Demographic: Participant name"
# lab var f3_2_sex "Demographic: Sex"
# lab var f3_2_age "Demographic: Age (years)"
# lab var f3_2_phone "Demographic: Phone number"
# lab var f3_2_village_school "Demographic: Name of village or school"
# lab var f3_2_occupation "Demographic: Occupation (adult/child/student)"
# lab var f3_2_leprosy_suspected "Leprosy: Is leprosy suspected"
# lab var f3_2_ssd_suspected "SSD: Is severe skin disease suspected"
# lab var f3_2_yaws_suspected "Yaws: Is yaws suspected"
# lab var f3_2_scabies_suspected "Scabies: Is scabies suspected"
# lab var f3_2_dpp_result "Yaws: DPP result"
# lab var f3_2_sore_location "Yaws: Sore location"
# lab var f3_2_swabcollected "Yaws: Swab collected"
# lab var f3_2_referral_name "Referral: Facility name"
# lab var f3_2_appointment_date "Referral: Appointment date"
# lab var f3_2_remarks "Form 3: Remarks"


# Form 10 variable labels -------------------------------------------------

# lab var f10_area_council "Area Council"
# lab var f10_health_zone "Health Zone"
# lab var f10_village "Village"
# lab var f10_laboratory "Laboratory"
# lab var f10_received_by "Stool samples received by"
# lab var mda_code "MDA ID"
# lab var f10_ethanol "Sample prepared in ethanol"
# lab var f10_pd "Sample prepared in PD"
# lab var f10_notes "Form 10: Remarks"


# Form 11 variable labels -------------------------------------------------

# lab var f11_laboratory "Form 11: Laboratory"
# lab var f11_microscopist "Form 11: Microscopist"
# lab var f11_date_analysed "Form 11: Date analysed"
# lab var f11_area_council "Area Council"
# lab var f11_health_zone "Health Zone"
# lab var f11_village "Village"
# lab var mda_code "MDA ID"
# lab var f11_pellet_volume "Pellet volume (mL)"
# lab var ascaris_egg_count1 "Ascaris: Egg count (coverslip 1)"
# lab var ascaris_egg_count2 "Ascaris: Egg count (coverslip 2)"
# lab var ascaris_totalegg_count "Ascaris: Total egg count"
# lab var trichuris_egg_count1 "Trichuris: Egg count (coverslip 1)"
# lab var trichuris_egg_count2 "Trichuris: Egg count (coverslip 2)"
# lab var trichuris_totalegg_count "Trichuris: Total egg count"
# lab var hookworm_egg_count1 "Hookworm: Egg count (coverslip 1)"
# lab var hookworm_egg_count2 "Hookworm: Egg count (coverslip 2)"
# lab var hookworm_totalegg_count "Hookworm: Total egg count"
# lab var other_sth_egg_count1 "Other STH: Egg count (coverslip 1)"
# lab var other_sth_egg_count2 "Other STH: Egg count (coverslip 2)"
# lab var other_totalegg_count "Other STH: Total egg count"
# lab var ascaris_epg "Ascaris: EPG"
# lab var trichuris_epg "Trichuris: EPG"
# lab var hookworm_epg "Hookworm: EPG"
# lab var other_sth_epg "Other STH: EPG"
# lab var qc_sample "Sample QC'd?"
