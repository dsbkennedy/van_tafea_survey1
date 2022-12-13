library(tidyverse)
library(targets)
tar_load(analysis_data)

# Prevalence by area council and village ----------------------------------
prevalence_fn <- function(...) {
  prevalence <- analysis_data %>% 
    filter(f11_results_flag==1) %>% 
    mutate(dummy=1) %>% 
    mutate(ascaris_result=case_when(f11_ascaris_epg>0 ~ 1),
           hookworm_result=case_when(f11_hookworm_epg>0 ~ 1),
           trichuris_result=case_when(f11_trichuris_epg>0 ~ 1)) %>% 
    group_by(...) %>% 
    summarise(total_ascaris=sum(ascaris_result, na.rm=TRUE),
              total_hookworm=sum(hookworm_result, na.rm=TRUE),
              total_trichuris=sum(trichuris_result, na.rm=TRUE), 
              total_samples=n()) %>% 
    pivot_longer(-c(..., total_samples)) %>% 
    mutate(prop = map2(value, total_samples, ~ prop.test(.x, .y, conf.level=0.95) %>%
                         broom::tidy())) %>%
    unnest(prop) %>% 
    select(..., name, value,total_samples,estimate,conf.low,conf.high) %>% 
    mutate(n_N=paste0('*',value, "/", total_samples), 
           est_conf = paste0(round(estimate*100,1), ' (', round(conf.low*100,1), ',', round(conf.high*100,1), ')')) %>% 
    select(..., name, n_N, est_conf) %>% 
    pivot_wider(names_from=name, values_from=c('n_N', 'est_conf')) %>% 
    select(..., contains('ascaris'), contains('trichuris'), contains('hookworm')) 
}

area_council_village_prevalence_fn <- function() {
  
area_council_prevalence <- prevalence_fn(f11_area_council_fct)
village_prevalence <- prevalence_fn(f11_area_council_fct,f11_village_fct) 
overall_prevalence <- prevalence_fn(dummy)

combined_prevalence <- area_council_prevalence %>% bind_rows(village_prevalence)  %>% 
  arrange(f11_area_council_fct) %>% 
  bind_rows(overall_prevalence) %>% 
  select(f11_area_council_fct, f11_village_fct, everything())
}
combined_prevalence <- area_council_village_prevalence_fn() %>% 
  select(-dummy)

# Intensity by age group --------------------------------------------------

sth_intensity_data <- analysis_data %>%   filter(f11_results_flag==1) %>% 
  dplyr::select(f11_area_council_fct, f11_village_fct,f2_age, f2_sex_fct,f2_stool_sample_fct,
                f11_ascaris_epg,
                f11_trichuris_epg,
                f11_hookworm_epg) %>% 
  mutate(age_group_sth=factor(case_when(f2_age ==0 ~ '<1 YOA',
                                        f2_age %in% c(1:4) ~ 'PSAC 1-4',
                                        f2_age %in% c(5:14) ~ 'SAC 5-14',
                                        f2_age >=15 ~ 'ADULTS >15', 
                                        TRUE ~ 'AGE MISSING'), 
                              levels=c('<1 YOA','PSAC 1-4', 'SAC 5-14','ADULTS >15', 'AGE MISSING'))) %>% 
  filter(!is.na(f11_ascaris_epg)) %>% 
  dplyr::select(-f2_age) %>% 
  dplyr::select(f11_area_council_fct,f11_village_fct, f2_sex_fct, age_group_sth,  contains('f11')) %>% 
  pivot_longer(-c('f11_area_council_fct', 'f11_village_fct', 'f2_sex_fct', 'age_group_sth')) %>% 
  rename(epg=value) %>% 
  mutate(pathogen=case_when(grepl('ascaris', name) ~ 'ascaris',
                            grepl('trichuris', name ) ~ 'trichuris',
                            grepl('hookworm', name) ~ 'hookworm'))  %>% 
  mutate(intensity=case_when(pathogen=='ascaris'  & (epg >=1 & epg<5000) ~ 'light',
                             pathogen=='ascaris'  & (epg >=5000) ~ 'moderate/heavy',
                             pathogen=='trichuris'  & (epg >=1 & epg<1000) ~ 'light',
                             pathogen=='trichuris'  & (epg >=1000) ~ 'moderate/heavy',
                             pathogen=='hookworm'  & (epg >=1 & epg<2000) ~ 'light',
                             pathogen=='hookworm'  & (epg >=2000) ~ 'moderat/heavy',
                             epg==0 ~ 'negative')) %>% 
  mutate(pathogen=factor(pathogen, levels=c('ascaris', 'trichuris', 'hookworm'))) %>% 
  mutate(intensity=factor(intensity, levels=c('negative', 'light', 'moderate/heavy'))) %>%
  dplyr::select(f11_area_council_fct,f11_village_fct,f2_sex_fct, age_group_sth, pathogen,intensity) %>% 
  mutate(f11_village_fct=fct_explicit_na(f11_village_fct, na_level="(missing)"))


intensity_prevalence_fn <- function(...) {
  intensity <- sth_intensity_data %>% 
    mutate(dummy=1) %>% 
    count(..., pathogen, intensity) %>% 
    group_by(..., pathogen) %>% 
    mutate(total=sum(n)) %>% 
    mutate(prop = map2(n, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                         broom::tidy())) %>%
    unnest(prop) %>% 
    select(..., pathogen, intensity,n,total,estimate,conf.low,conf.high) %>% 
    mutate(n_N=paste0('*',n, "/", total), 
           est_conf = paste0(round(estimate*100,1), ' (', round(conf.low*100,1), ',', round(conf.high*100,1), ')')) %>%  
    select(..., pathogen,intensity, n_N, est_conf) %>% 
    pivot_wider(names_from=c(pathogen,intensity), values_from=c('n_N', 'est_conf')) %>% 
    select(..., contains('ascaris'), contains('trichuris'), contains('hookworm'))
}

area_council_village_intensity_fn <- function() {
  
area_council_intensity <- intensity_prevalence_fn(f11_area_council_fct)
village_intensity <- intensity_prevalence_fn(f11_area_council_fct, f11_village_fct)
overall_intensity <- intensity_prevalence_fn(dummy)

combined_intensity <- area_council_intensity %>% bind_rows(village_intensity)  %>% 
  arrange(f11_area_council_fct) %>% 
  bind_rows(overall_intensity) %>% 
  select(f11_area_council_fct, f11_village_fct, everything())
return(combined_intensity)
}

combined_intensity <- area_council_village_intensity_fn() %>% select(-dummy)


prevalence_area_council_gph_data <- analysis_data %>% 
  filter(f11_results_flag==1) %>% 
  mutate(dummy=1) %>% 
  mutate(f11_area_council_fct = case_when(
    f11_area_council_fct=='aneityum' ~ 'Aneityum',
    f11_area_council_fct=='futuna' ~ 'Futuna',
    f11_area_council_fct=='middle_bush' ~ 'Middle bush Tanna', 
    f11_area_council_fct=='south_tanna' ~ 'South Tanna',
    f11_area_council_fct=='south_west_tanna' ~ 'South-west Tanna',
    f11_area_council_fct=='west_tanna' ~ 'West Tanna',
    f11_area_council_fct=='whitesands' ~ 'Whitesands')) %>% 
 
  mutate(ascaris_result=case_when(f11_ascaris_epg>0 ~ 1),
         hookworm_result=case_when(f11_hookworm_epg>0 ~ 1),
         trichuris_result=case_when(f11_trichuris_epg>0 ~ 1)) %>% 
  group_by(f11_area_council_fct) %>% 
  summarise(total_ascaris=sum(ascaris_result, na.rm=TRUE),
            total_hookworm=sum(hookworm_result, na.rm=TRUE),
            total_trichuris=sum(trichuris_result, na.rm=TRUE), 
            total_samples=n()) %>% 
  pivot_longer(-c(f11_area_council_fct, total_samples)) %>% 
  mutate(prop = map2(value, total_samples, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% 
  mutate(pathogen=case_when(grepl('ascaris', name) ~ 'Ascaris', 
                            grepl('hookworm', name) ~ 'Hookworm', 
                            grepl('trichuris', name) ~ 'Trichuris')) %>% 
  mutate(pathogen=factor(pathogen, levels=c('Ascaris', 'Trichuris', 'Hookworm'))) %>% 
  select(f11_area_council_fct, pathogen, value,total_samples,estimate,conf.low,conf.high) 

prevalence_village_gph_data <- analysis_data %>% 
  filter(f11_results_flag==1) %>% 
  mutate(dummy=1) %>% 
  mutate(f11_area_council_fct = case_when(
    f11_area_council_fct=='aneityum' ~ 'Aneityum',
    f11_area_council_fct=='futuna' ~ 'Futuna',
    f11_area_council_fct=='middle_bush' ~ 'Middle bush Tanna', 
    f11_area_council_fct=='south_tanna' ~ 'South Tanna',
    f11_area_council_fct=='south_west_tanna' ~ 'South-west Tanna',
    f11_area_council_fct=='west_tanna' ~ 'West Tanna',
    f11_area_council_fct=='whitesands' ~ 'Whitesands')) %>% 
  mutate(ascaris_result=case_when(f11_ascaris_epg>0 ~ 1),
         hookworm_result=case_when(f11_hookworm_epg>0 ~ 1),
         trichuris_result=case_when(f11_trichuris_epg>0 ~ 1)) %>% 
  group_by(f11_area_council_fct,f11_village_fct) %>% 
  summarise(total_ascaris=sum(ascaris_result, na.rm=TRUE),
            total_hookworm=sum(hookworm_result, na.rm=TRUE),
            total_trichuris=sum(trichuris_result, na.rm=TRUE), 
            total_samples=n()) %>% 
  pivot_longer(-c(f11_area_council_fct,f11_village_fct, total_samples)) %>% 
  mutate(prop = map2(value, total_samples, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop)  %>% 
  mutate(pathogen=case_when(grepl('ascaris', name) ~ 'Ascaris', 
                            grepl('hookworm', name) ~ 'Hookworm', 
                            grepl('trichuris', name) ~ 'Trichuris')) %>% 
  mutate(pathogen=factor(pathogen, levels=c('Ascaris', 'Trichuris', 'Hookworm'))) %>% 
  select(f11_area_council_fct,f11_village_fct, pathogen, value,total_samples,estimate,conf.low,conf.high) 


prevalence_village_gph_data %>% 
  ggplot(aes(x=f11_area_council_fct, y=estimate)) +
  geom_point(stat="summary", fun.y="mean", size=4) +
  geom_jitter(colour="red", alpha=0.8, width=0.2, shape=17, size=3) +
  geom_errorbar(data=prevalence_area_council_gph_data,aes(ymin=conf.low, ymax=conf.high), width=.4) +
  # geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x='Area council', y='% prevalence (mean + 95%CI)') + 
  theme(axis.text.x=element_text(angle=45, hjust=1), text = element_text(size = 20)) +
  facet_wrap(~pathogen)


         