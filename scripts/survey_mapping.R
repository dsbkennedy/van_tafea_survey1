
library(here)
library(sf)
library(tidyverse)
library(ggsflabel)
library(rayshader)
library(terra)
library(linelist)
skin_exam_data <- readRDS(here('skin_exam_data.Rds'))


van_shp <- list.files(here('data', 'input', 'shp'),
                      pattern=".shp$", full.names = T, recursive = T)

van_shp <- st_read(van_shp[[1]])

tafea_shp <- van_shp %>% filter(ADM1_EN=="Tafea") %>% clean_data()

skin_exam_geopoints_raw <-
  skin_exam_data %>% dplyr::select(
    skin_exam_latitude,
    skin_exam_longitude,
    skin_exam_area_council_fct,
    skin_exam_village_fct
  ) 


skin_exam_geopoints_raw_count <- skin_exam_geopoints_raw %>% 
  ungroup() %>%  
  count(skin_exam_area_council_fct, skin_exam_village_fct) %>% rename(skin_exam_surveys=n)


skin_exam_geopoints_processed <- skin_exam_geopoints_raw %>% 
  filter(!is.na(skin_exam_longitude)) %>%
  filter(!is.na(skin_exam_latitude)) %>%
  mutate(
    skin_exam_longitude = case_when(
      skin_exam_longitude == 1694124.0000 ~ 1694124.0000 / 10000,
      TRUE ~ skin_exam_longitude
    )
  ) %>%
  mutate(
    skin_exam_latitude = case_when(
      skin_exam_latitude == -1.952915e+06 ~ -1.952915e+06 / 100000,
      skin_exam_longitude == 169.4522 ~ -1.9529150,
      TRUE ~ skin_exam_latitude
    )
  ) %>%
  mutate(
    skin_exam_latitude = case_when(
      skin_exam_latitude > 0 ~ skin_exam_latitude * -1,
      TRUE ~ skin_exam_latitude
    )
  ) %>%
  #count(skin_exam_area_council_fct,skin_exam_village_fct,skin_exam_longitude, skin_exam_latitude) %>%
  #filter(n > 1) %>%
  filter(skin_exam_longitude > 169) %>%
  st_as_sf(coords = c('skin_exam_longitude', 'skin_exam_latitude')) %>%
  st_set_crs(st_crs(tafea_shp))

mapview(skin_exam_geopoints_processed)

skin_exam_geopoints_processed_count <- skin_exam_geopoints_processed %>% 
  as_tibble() %>%  
  ungroup() %>%  
  count(skin_exam_area_council_fct, skin_exam_village_fct) %>% 
  rename(skin_exam_valid_geopoints=n)


skin_exam_map_shp_join <- st_intersection(tafea_shp, skin_exam_geopoints_processed) %>% 
  mutate(area_council_match=case_when(skin_exam_area_council_fct==adm2_en ~ 'YES', 
                                      TRUE ~ 'NO'))

skin_exam_joined_count <- skin_exam_map_shp_join %>% 
  as_tibble() %>%  
  ungroup() %>%  
  count(skin_exam_area_council_fct, skin_exam_village_fct,area_council_match) %>% 
  filter(area_council_match=='YES') %>% 
  rename(skin_exam_correct_area_council=n) %>% 
  dplyr::select(-area_council_match)

skin_exam_geopoints_summary <- skin_exam_geopoints_raw_count %>% 
  full_join(skin_exam_geopoints_processed_count, by=c('skin_exam_area_council_fct', 'skin_exam_village_fct')) %>% 
  full_join(skin_exam_joined_count, by=c('skin_exam_area_council_fct', 'skin_exam_village_fct'))



# get COUNTY data for a given state
counties_spec <- tafea_shp %>% 
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), 
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  mutate(x_nudge=case_when(adm2_en=='aniwa' ~ 0, 
                           adm2_en=='west_tanna' ~ -0.17, 
                           adm2_en=='south_tanna' ~ 0.12, 
                           adm2_en=='south_west_tanna' ~ -0.1, 
                           adm2_en=='middle_bush_tanna' ~ 0.20, 
                           adm2_en=='whitesands' ~ 0.18, 
                           TRUE ~ 0),
         y_nudge=case_when(adm2_en=='aniwa' ~ 0.07, 
                           adm2_en=='futuna' ~ 0.07, 
                           adm2_en=='north_tanna' ~ 0.1, 
                           adm2_en=='south_tanna' ~ -0.04, 
                           
                           adm2_en=='south_west_tanna' ~ -0.1, 
                           adm2_en=='middle_bush_tanna' ~ 0.05, 
                           TRUE ~ 0)) 

village_map_shp_join <- map_shp_join %>%  filter(area_council_match=='YES') %>%
  group_by(skin_exam_village_fct) %>% slice(1) %>% 
  mutate(x_nudge=case_when(skin_exam_village_fct=='ishia' ~ 0.5, 
                           skin_exam_village_fct=='anowunamlao' ~ -0.1, 
                           skin_exam_village_fct=='lounelapen' ~ -0.25, 
                           skin_exam_village_fct=='lounapektuan' ~ -0.25, 
                           skin_exam_village_fct=='koraioken' ~ -0.15, 
                           TRUE ~ 0.35),
         y_nudge=case_when(skin_exam_village_fct=='lounapektuan' ~ 0.05,
                           skin_exam_village_fct=='futuna' ~ 0.07,
                           skin_exam_village_fct=='ishia' ~ -0.1,
                           
                           skin_exam_village_fct=='north_tanna' ~ 0.1,
                           TRUE ~ 0))


height_folder <- here('data/input/elevation')
height_files <- list.files(height_folder, full.names = T)
height_files_import <- map(height_files, raster::raster)

ic <- sprc(lapply(height_files, rast))
#r <- merge(ic)

van_elevation <- raster::merge(ic)

raster::extent()
e <- raster::extent(tafea_shp %>% st_buffer(10000))
r <- crop(van_elevation, e)
van_elevation_mat <- raster_to_matrix(as(r, "Raster"))

van_height <-
  as.data.frame(r, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit() %>% 
  rename(height=S17E168) %>% 
  filter(height>0)

(tafea_map <- ggplot(data=tafea_shp) +
    geom_raster(data=van_height, aes(x = x, y = y, fill = height)) +
    geom_sf(fill=NA) +
    geom_text(data=counties_spec, aes(x=lon, y=lat,label = adm2_en),
              color = "#333333"
              ,size = 4
              ,fontface = 'bold', 
              nudge_x = counties_spec$x_nudge,
              nudge_y = counties_spec$y_nudge) +
    # geom_sf_label_repel(data=counties_spec,aes(label = ADM2_EN),
    #                     force = 100, nudge_x = -0.35, seed = 10) +
    geom_sf_label_repel(data=village_map_shp_join, 
                        aes(label = skin_exam_village_fct),
                        nudge_x = village_map_shp_join$x_nudge,
                        nudge_y = village_map_shp_join$y_nudge)+
    #geom_sf_label_repel(data=counties_spec, aes(x=lon, y=lat, label=ADM2_EN),nudge_x = -0.1, nudge_y = +0.3) +
    geom_sf(data=map_shp_join %>%   group_by(skin_exam_village_fct) %>% slice(1), aes(color=area_council_match), size=2.5) +
    ggsn::north(tafea_shp) +
    ggsn::scalebar(tafea_shp, dist = 25, dist_unit = "km",location='bottomleft',
                   transform = TRUE, model = "WGS84") +
    labs(fill='Elevation (metres)') +
    scale_fill_gradientn(
      colors = c("#9DBF9E", "#FCB97D", "#A84268"),
      na.value = "grey80",
      #limits = c(0, 0.1),
      oob = scales::squish) +
    # labels = scales::percent,
    #name = "Active STRs as share of total dwellings") +
    # Set a completely blank theme, to get rid of all background and axis elements
    theme_void() + theme(legend.position = 'top')
)


map_data <-
  skin_exam_data %>% dplyr::select(
    skin_exam_latitude,
    skin_exam_longitude,
    skin_exam_area_council_fct,
    skin_exam_village_fct
  ) %>%
  filter(!is.na(skin_exam_longitude)) %>%
  filter(!is.na(skin_exam_latitude)) %>%
  mutate(
    skin_exam_longitude = case_when(
      skin_exam_longitude == 1694124.0000 ~ 1694124.0000 / 10000,
      TRUE ~ skin_exam_longitude
    )
  ) %>%
  mutate(
    skin_exam_latitude = case_when(
      skin_exam_latitude == -1.952915e+06 ~ -1.952915e+06 / 100000,
      skin_exam_longitude == 169.4522 ~ -1.9529150,
      TRUE ~ skin_exam_latitude
    )
  ) %>%
  mutate(
    skin_exam_latitude = case_when(
      skin_exam_latitude > 0 ~ skin_exam_latitude * -1,
      TRUE ~ skin_exam_latitude
    )
  ) %>%
  count(skin_exam_area_council_fct,skin_exam_village_fct,skin_exam_longitude, skin_exam_latitude) %>%
  #filter(n > 1) %>%
  filter(skin_exam_longitude > 169) %>%
  st_as_sf(coords = c('skin_exam_longitude', 'skin_exam_latitude')) %>%
  st_set_crs(st_crs(tafea_shp))


# RISK FACTOR -------------------------------------------------------------



risk_factor_geopoints_raw <- risk_factor_working_data %>% 
  dplyr::select(
  risk_factor_longitude,
  risk_factor_latitude,
  risk_factor_area_council_fct,
  risk_factor_village_fct
) 

risk_factor_geopoints_raw_count <- risk_factor_geopoints_raw %>% 
  ungroup() %>%  
  count(risk_factor_area_council_fct, risk_factor_village_fct) %>% rename(risk_factor_surveys=n)


risk_factor_geopoints_processed <- risk_factor_geopoints_raw %>% 
  filter(!is.na(risk_factor_longitude)) %>% 
  st_as_sf(coords = c('risk_factor_longitude', 'risk_factor_latitude')) %>%
  st_set_crs(st_crs(tafea_shp))

risk_factor_geopoints_processed_count <- risk_factor_geopoints_processed %>% 
  as_tibble() %>%  
  ungroup() %>%  
  count(risk_factor_area_council_fct, risk_factor_village_fct) %>% 
  rename(risk_factor_valid_geopoints=n)

risk_factor_map_shp_join <- st_intersection(tafea_shp,risk_factor_geopoints_processed) %>% 
  mutate(area_council_match=case_when(risk_factor_area_council_fct==adm2_en ~ 'YES', 
                                      TRUE ~ 'NO'))

risk_factor_joined_count <- risk_factor_map_shp_join %>% 
  as_tibble() %>%  
  ungroup() %>%  
  count(risk_factor_area_council_fct, risk_factor_village_fct,area_council_match) %>% 
  filter(area_council_match=='YES') %>% 
  rename(risk_factor_correct_area_council=n) %>% 
  dplyr::select(-area_council_match)


risk_factor_geopoints_summary <- risk_factor_geopoints_raw_count %>% 
  full_join(risk_factor_geopoints_processed_count, by=c('risk_factor_area_council_fct', 'risk_factor_village_fct')) %>% 
  full_join(risk_factor_joined_count, by=c('risk_factor_area_council_fct', 'risk_factor_village_fct'))


risk_factor_map_shp_join <- risk_factor_map_shp_join %>%  filter(area_council_match=='YES') %>%
  group_by(risk_factor_village_fct) %>% slice(1) %>% 
  mutate(x_nudge=case_when(risk_factor_village_fct=='ishia' ~ 0.5, 
                           risk_factor_village_fct=='anowunamlao' ~ -0.1, 
                           risk_factor_village_fct=='lounelapen' ~ -0.25, 
                           risk_factor_village_fct=='lounapektuan' ~ -0.25, 
                           risk_factor_village_fct=='koraioken' ~ -0.15, 
                           TRUE ~ 0.35),
         y_nudge=case_when(risk_factor_village_fct=='lounapektuan' ~ 0.05,
                           risk_factor_village_fct=='futuna' ~ 0.07,
                           risk_factor_village_fct=='ishia' ~ -0.1,
                           risk_factor_village_fct=='north_tanna' ~ 0.1,
                           TRUE ~ 0))

library(mapview)
mapview(risk_factor_geopoints_processed)


ggplot(data=tafea_shp) +
  geom_raster(data=van_height, aes(x = x, y = y, fill = height)) +
  geom_sf(fill=NA) +
  geom_text(data=counties_spec, aes(x=lon, y=lat,label = adm2_en),
            color = "#333333"
            ,size = 4
            ,fontface = 'bold', 
            nudge_x = counties_spec$x_nudge,
            nudge_y = counties_spec$y_nudge) +
  geom_sf_label_repel(data=risk_factor_map_shp_join, 
                      aes(label = risk_factor_village_fct),
                      nudge_x = risk_factor_map_shp_join$x_nudge,
                      nudge_y = risk_factor_map_shp_join$y_nudge) +
  geom_sf(data=risk_factor_map_shp_join,aes(color=area_council_match), size=2.5) +
  ggsn::north(tafea_shp) +
  ggsn::scalebar(tafea_shp, dist = 25, dist_unit = "km",location='bottomleft',
                 transform = TRUE, model = "WGS84") +
  labs(fill='Elevation (metres)') +
  scale_fill_gradientn(
    colors = c("#9DBF9E", "#FCB97D", "#A84268"),
    na.value = "grey80",
    #limits = c(0, 0.1),
    oob = scales::squish) +
  # labels = scales::percent,
  #name = "Active STRs as share of total dwellings") +
  # Set a completely blank theme, to get rid of all background and axis elements
  theme_void() + theme(legend.position = 'top')



(tafea_map <- ggplot(data=tafea_shp) +
    geom_raster(data=van_height, aes(x = x, y = y, fill = height)) +
    geom_sf(fill=NA) +
    geom_text(data=counties_spec, aes(x=lon, y=lat,label = adm2_en),
              color = "#333333"
              ,size = 4
              ,fontface = 'bold', 
              nudge_x = counties_spec$x_nudge,
              nudge_y = counties_spec$y_nudge) +
    # geom_sf_label_repel(data=counties_spec,aes(label = ADM2_EN),
    #                     force = 100, nudge_x = -0.35, seed = 10) +
    geom_sf_label_repel(data=village_map_shp_join, 
                        aes(label = skin_exam_village_fct),
                        nudge_x = village_map_shp_join$x_nudge,
                        nudge_y = village_map_shp_join$y_nudge)+
    #geom_sf_label_repel(data=counties_spec, aes(x=lon, y=lat, label=ADM2_EN),nudge_x = -0.1, nudge_y = +0.3) +
    geom_sf(data=map_shp_join,  
              #group_by(skin_exam_village_fct) %>% slice(1), 
            aes(color=area_council_match), size=2.5) +
    ggsn::north(tafea_shp) +
    ggsn::scalebar(tafea_shp, dist = 25, dist_unit = "km",location='bottomleft',
                   transform = TRUE, model = "WGS84") +
    labs(fill='Elevation (metres)') +
    scale_fill_gradientn(
      colors = c("#9DBF9E", "#FCB97D", "#A84268"),
      na.value = "grey80",
      #limits = c(0, 0.1),
      oob = scales::squish) +
    # labels = scales::percent,
    #name = "Active STRs as share of total dwellings") +
    # Set a completely blank theme, to get rid of all background and axis elements
    theme_void() + theme(legend.position = 'top')
)



geopoints_summary <- skin_exam_geopoints_summary %>% full_join(risk_factor_geopoints_summary, by=c('skin_exam_area_council_fct' = 'risk_factor_area_council_fct', 
                                                                              'skin_exam_village_fct' = 'risk_factor_village_fct'))





##########
risk_factor_geopoints_raw <- risk_factor_working_data %>% 
  dplyr::select(
    risk_factor_longitude,
    risk_factor_latitude,
    risk_factor_area_council_fct,
    risk_factor_village_fct
  ) 

risk_factor_geopoints_raw_count <- risk_factor_geopoints_raw %>% 
  ungroup() %>%  
  count(risk_factor_area_council_fct, risk_factor_village_fct) %>% rename(risk_factor_surveys=n)


risk_factor_geopoints_processed <- risk_factor_geopoints_raw %>% 
  filter(!is.na(risk_factor_longitude)) %>% 
  st_as_sf(coords = c('risk_factor_longitude', 'risk_factor_latitude')) %>%
  st_set_crs(st_crs(tafea_shp))

risk_factor_geopoints_processed_count <- risk_factor_geopoints_processed %>% 
  as_tibble() %>%  
  ungroup() %>%  
  count(risk_factor_area_council_fct, risk_factor_village_fct) %>% 
  rename(valid_geopoints=n)

risk_factor_map_shp_join <- st_intersection(tafea_shp,risk_factor_geopoints_processed) %>% 
  mutate(area_council_match=case_when(risk_factor_area_council_fct==adm2_en ~ 'YES', 
                                      TRUE ~ 'NO'))

risk_factor_joined_count <- risk_factor_map_shp_join %>% 
  as_tibble() %>%  
  ungroup() %>%  
  count(risk_factor_area_council_fct, risk_factor_village_fct,area_council_match) %>% 
  filter(area_council_match=='YES') %>% 
  rename(correct_area_council=n) %>% 
  dplyr::select(-area_council_match)


risk_factor_geopoints_summary <- risk_factor_geopoints_raw_count %>% 
  full_join(risk_factor_geopoints_processed_count, by=c('risk_factor_area_council_fct', 'risk_factor_village_fct')) %>% 
  full_join(risk_factor_joined_count, by=c('risk_factor_area_council_fct', 'risk_factor_village_fct'))

##########TO BE CONTINUED
library(here)
library(sf)
library(tidyverse)
library(ggsflabel)
library(rayshader)
library(terra)
library(linelist)

van_shp <- list.files(here('data', 'input', 'shp'),
                      pattern=".shp$", full.names = T, recursive = T) 

van_shp <- st_read(van_shp[[1]])

tafea_shp <- van_shp %>% filter(ADM1_EN=="Tafea") %>% clean_data()

coordinates <- readxl::read_xlsx(here("/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/analysis/van_tafea_survey1/data/input/vanuatu_coordinates.xlsx"), 
                                 sheet='2020 Consolidated Listing') %>% 
  clean_names() %>% 
  rename(latitude=lattitude)

tafea_coord <- coordinates %>% filter(province_name=='Tafea') %>% 
  st_as_sf(coords = c('latitude', 'longitude')) %>%
  st_set_crs(st_crs(tafea_shp)) %>% 
  clean_data()

survey_villages <- readxl::read_xlsx(here('/Users/DK_kirby/Library/CloudStorage/OneDrive-UNSW/analysis/van_tafea_survey1/data/input/Vanuatu_village_sampling_v2.1.xlsx'), 
                                     sheet='Tafea - SELECTED') %>% 
  clean_data()

original_survey_villages_coord <- survey_villages %>% left_join(tafea_coord, 
                                                                by=c('acname' = 'ac_name',
                                                                     'vname' = 'village_area_name')) %>% 
  st_as_sf()

visited_survey_villages_coord <- readRDS( here('outputs', 'skin_exam_data.Rds')) %>% 
  count(skin_exam_area_council_fct, skin_exam_village_fct) %>% 
  left_join(tafea_coord, 
            by=c('skin_exam_area_council_fct' = 'ac_name',
                 'skin_exam_village_fct' = 'village_area_name')) 

ggplot() +
  geom_sf(data=tafea_shp) +
  geom_sf(data=survey_villages_coord) +
  geom_sf_label_repel(data=survey_villages_coord, 
                      aes(label = vname))
