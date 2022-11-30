
analysis_data <- readRDS("~/Library/CloudStorage/OneDrive-UNSW/analysis/van_tafea_survey1/analysis_data.Rds")

library(tidyverse)

area_council_prevalence <- analysis_data %>% 
  filter(f11_results_flag==1) %>% 
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
  select(f11_area_council_fct, name, value,total_samples,estimate,conf.low,conf.high) %>% 
  mutate(n_N=paste0('*',value, "/", total_samples), 
         est_conf = paste0(round(estimate*100,1), ' (', round(conf.low*100,1), ',', round(conf.high*100,1), ')')) %>% 
  select(f11_area_council_fct, name, n_N, est_conf) %>% 
  pivot_wider(names_from=name, values_from=c('n_N', 'est_conf')) %>% 
  select(f11_area_council_fct, contains('ascaris'), contains('trichuris'), contains('hookworm'))

overall_prevalence <- analysis_data %>% 
  filter(f11_results_flag==1) %>% 
  mutate(ascaris_result=case_when(f11_ascaris_epg>0 ~ 1),
         hookworm_result=case_when(f11_hookworm_epg>0 ~ 1),
         trichuris_result=case_when(f11_trichuris_epg>0 ~ 1)) %>% 
  summarise(total_ascaris=sum(ascaris_result, na.rm=TRUE),
            total_hookworm=sum(hookworm_result, na.rm=TRUE),
            total_trichuris=sum(trichuris_result, na.rm=TRUE), 
            total_samples=n()) %>% 
  pivot_longer(-c(total_samples)) %>% 
  mutate(prop = map2(value, total_samples, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(prop) %>% 
  select( name, value,total_samples,estimate,conf.low,conf.high) %>% 
  mutate(n_N=paste0('*',value, "/", total_samples), 
         est_conf = paste0(round(estimate*100,1), ' (', round(conf.low*100,1), ',', round(conf.high*100,1), ')')) %>% 
  select(name, n_N, est_conf) %>% 
  pivot_wider(names_from=name, values_from=c('n_N', 'est_conf')) %>% 
  select( contains('ascaris'), contains('trichuris'), contains('hookworm'))

combined_prevalence <- area_council_prevalence %>% bind_rows(overall_prevalence)  




