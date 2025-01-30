
# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("here","tidyverse","openxlsx","sf"))

rute_1<- read.xlsx(xlsxFile = here("results","rute scenarier med pris sensitivitet","rute_scenario_summering.xlsx"),sheet=1) %>% 
  mutate(across(HAU_Flesland_yrke:HAU_Sola_ferie,~.x/(10^6))) %>% 
  group_by(rute) %>% 
  mutate(Bergen = HAU_Flesland_yrke+HAU_Flesland_ferie) %>% 
  mutate(Stavanger = HAU_Sola_yrke+HAU_Sola_ferie) %>%
  select(Kommune,rute, Bergen,Stavanger) %>% 
  pivot_longer(Bergen:Stavanger, names_to = "Lufthavn",values_to = "nytte") %>% 
  mutate(Kommune = case_when(Kommune == "Tysvaer"~"Tysvær",.default = Kommune))

regional_nytte<- rute_1 %>% filter(Kommune == "Region")
  
geojson_file <- list.files(here("data","auxilary data"), pattern = "\\.geojson$", full.names = TRUE)

# Read the GeoJSON file into an sf object
municipalities_gdf <- st_read(geojson_file,layer = "Kommune")[,c("kommunenavn","geometry")] %>% 
  filter(kommunenavn %in% unique(rute_1$Kommune)) %>% 
  rename(Kommune = kommunenavn)

map_data<- left_join(municipalities_gdf,rute_1, by ="Kommune")

# map visualization -------------------------------------------------------

## gdansk

gdansk<-map_data %>% 
  filter(rute == "Gdansk") %>% 
  ggplot()+
  geom_sf(aes(fill = nytte),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno",direction = -1,
                       guide = guide_colorbar(title = "Nytte (.mill nok)",
                                               direction = "horizontal",
                                               position = "bottom"))+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Samfunnsnytte beløp:",subtitle = paste0("Total: 227,8 mill. nok","\n",
                                                        "Mot Flesland: 119,9 mill. nok","\n",
                                                        "Mot Sola: 107,9 mill. nok"),caption = "Kartene viser akkumulert overskudd per år etter kommune for alternativene")+
  
  facet_wrap(~Lufthavn)

ggsave(filename = here("graphs","rute_scenarier","gdansk.jpeg"),plot = gdansk,width = 9,height = 7,units = "in")

## aberdeen direkt mot indirekt


ABR_direkt<-map_data %>% 
  filter(grepl(rute,pattern = "Aberdeen-direkt",perl = T)) %>% 
  ggplot()+
  geom_sf(aes(fill = nytte),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno",direction = -1,
                       guide = guide_colorbar(title = "Nytte (.mill nok)",
                                              direction = "horizontal",
                                              position = "bottom"))+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Samfunnsnytte beløp:",subtitle = paste0("Total: 83,5 mill. nok","\n",
                                                        "Mot Flesland: 50,4 mill. nok","\n",
                                                        "Mot Sola: 33,1 mill. nok"),caption = "Kartene viser akkumulert overskudd per år etter kommune for alternativene")+
  facet_wrap(~Lufthavn)

ggsave(filename = here("graphs","rute_scenarier","ABR_direkte.jpeg"),plot = ABR_direkt,width = 9,height = 7,units = "in")


ABR_indirekt<-map_data %>% 
  filter(grepl(rute,pattern = "Aberdeen-indirekt",perl = T)) %>% 
  ggplot()+
  geom_sf(aes(fill = nytte),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno",direction = 1,
                       guide = guide_colorbar(title = "Nytte (.mill nok)",
                                              direction = "horizontal",
                                              position = "bottom"))+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Samfunnsnytte beløp:",subtitle = paste0("Total: -84,5 mill. nok","\n",
                                                        "Mot Flesland: -33,7 mill. nok","\n",
                                                        "Mot Sola: -51,1 mill. nok"),caption = "Kartene viser akkumulert ekstra kostnad per år etter kommune for alternativene")+
  facet_wrap(~Lufthavn)

ggsave(filename = here("graphs","rute_scenarier","ABR_indirekte.jpeg"),plot = ABR_indirekt,width = 9,height = 7,units = "in")


## Bergen
rute_2<- read.xlsx(xlsxFile = here("results","rute scenarier med pris sensitivitet","rute_scenario_summering.xlsx"),sheet=1) %>% 
  mutate(across(HAU_Flesland_yrke:HAU_Sola_ferie,~.x/(10^6))) %>% 
  filter(rute == "Berge-bil_mot_fly")%>% 
  mutate(Kommune = case_when(Kommune == "Tysvaer"~"Tysvær",.default = Kommune))


map_data_2<- left_join(municipalities_gdf,rute_2, by ="Kommune") %>% 
  select(Kommune,HAU_Flesland_yrke,HAU_Flesland_ferie) %>% 
  pivot_longer(HAU_Flesland_yrke:HAU_Flesland_ferie, names_to = "reisemal",values_to = "nytte") %>% 
  mutate(reisemal = str_split_i(reisemal,"_",i=3))


bergen<- map_data_2 %>% 
  ggplot()+
  geom_sf(aes(fill = nytte),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno",direction = -1,
                       guide = guide_colorbar(title = "Nytte (.mill nok)",
                                              direction = "horizontal",
                                              position = "bottom"))+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Samfunnsnytte beløp:",subtitle = paste0("Total: 52,09 mill. nok","\n",
                                                        "For yrke: 94,4 mill. nok","\n",
                                                        "For ferie: -42,3 mill. nok"),
       caption = "Kartene viser akkumulert overskudd/kostnad per år etter kommune for alternativene")+
  facet_wrap(~reisemal)

  
ggsave(filename = here("graphs","rute_scenarier","bergen.jpeg"),plot = bergen,width = 9,height = 7,units = "in")


## Oslo

oslo_rute<-read.xlsx(xlsxFile = here("results","rute scenarier med pris sensitivitet","rute_scenario_summering.xlsx"),sheet=2) %>% 
  mutate(across(contains("HAU"),~.x/(10^6))) %>% 
  pivot_longer(cols = contains("HAU"),names_to = "Lufthavn",values_to = "nytte") %>% 
  mutate(Lufthavn = str_split_i(Lufthavn, "-",i = 2)) %>% 
  mutate(Kommune = case_when(Kommune == "Tysvaer"~"Tysvær",.default = Kommune))

oslo_map<-left_join(municipalities_gdf,oslo_rute, by ="Kommune")

for (i in unique(oslo_map$Lufthavn)) {
  
  graph<-oslo_map %>% 
    filter(Lufthavn == i) %>% 
    ggplot()+
    geom_sf(aes(fill = nytte),color = "black")+
    geom_sf_label(aes(label = Kommune),color = "black")+
    scale_fill_viridis_c(option = "inferno",direction = -1,
                         guide = guide_colorbar(title = "Nytte (.mill nok)",
                                                direction = "horizontal",
                                                position = "bottom"))+
    theme_minimal()+
    theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())
  
  ggsave(filename = here("graphs","rute_scenarier",paste0("oslo_",i,".jpeg")),plot = graph, width = 6,height = 4,units = "in")
}
