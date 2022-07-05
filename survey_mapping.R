
library(here)
library(sf)
library(tidyverse)
library(ggsflabel)
map_data <-
  skin_exam_working_data %>% select(
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
  
van_shp <- list.files(here('data', 'input', 'shp'),
                      pattern=".shp$", full.names = T, recursive = T)

van_shp <- st_read(van_shp[[1]])

tafea_shp <- van_shp %>% filter(ADM1_EN=="Tafea") %>% clean_data()

plot(tafea_shp$geometry)
plot(map_data$geometry, add=TRUE)

map_shp_join <- st_intersection(map_data,tafea_shp) %>% 
  mutate(area_council_match=case_when(skin_exam_area_council_fct==adm2_en ~ 'YES', 
                                      TRUE ~ 'NO'))

map_shp_join %>% as_tibble() %>% 
  tabyl(skin_exam_area_council_fct, adm2_en)

ggplot() +
 geom_sf(data=tafea_shp) +
  geom_sf(data=map_shp_join, aes(color=area_council_match), size=3) +
  theme_void()


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
  mutate(x_nudge=case_when(skin_exam_village_fct=='ishia' ~ +0.2, 
                           skin_exam_village_fct=='anowunamlao' ~ -0.1, 
                           skin_exam_village_fct=='lounelapen' ~ -0.15, 
                           skin_exam_village_fct=='lounapektuan' ~ -0.15, 
                           skin_exam_village_fct=='koraioken' ~ -0.15, 
                           TRUE ~ 0.35),
         y_nudge=case_when(skin_exam_village_fct=='lounapektuan' ~ 0.05,
                           skin_exam_village_fct=='futuna' ~ 0.07,
                           skin_exam_village_fct=='ishia' ~ 0,
                           
                           skin_exam_village_fct=='north_tanna' ~ 0.1,
                           TRUE ~ 0))

(tafea_map <- ggplot(data=tafea_shp) +
 geom_sf() +
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
  labs(fill='Population density (people per km2)') +
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
