#Heatmap of the results



# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","sf","ggthemes","geodata","here","rnaturalearth","rnaturalearthdata"))


# grunnkartet og overskudd data------------------------------------------------------

overskudd<- readRDS(file = here("results","all_overskudd.RDS")) %>% 
  mutate(Kommune = case_when(Kommune == "Tysvaer"~"Tysvær",.default = Kommune)) %>% 
  mutate(rute = gsub("hlh","HAU",rute)) %>% 
  mutate(bil.ferie = round((bil.ferie_driver_overskudd+bil.ferie_passajerer_overskudd)/2),
         bil.yrke = round((bil.yrke_driver_overskudd+bil.yrke_passajerer_overskudd)/2)) %>% 
  select(-contains("driver_overskudd"),-contains("passajerer_overskudd"))
  

# url <- "https://nedlasting.geonorge.no/geonorge/Basisdata/Kommuner/GeoJSON/Basisdata_0000_Norge_25833_Kommuner_GeoJSON.zip"
# temp_file <- tempfile()
# download.file(url, temp_file)
# unzip_dir <- "C:/Users/sioz/Desktop/arbeid/prosjektene/Kostnytte_analyse_haugesund_lufthavng/data/auxilary data"
# unzip(temp_file, exdir = unzip_dir)
geojson_file <- list.files(here("data","auxilary data"), pattern = "\\.geojson$", full.names = TRUE)

# Read the GeoJSON file into an sf object
municipalities_gdf <- st_read(geojson_file,layer = "Kommune")[,c("kommunenavn","geometry")] %>% 
  filter(kommunenavn %in% unique(overskudd$Kommune)) %>% 
  rename(Kommune = kommunenavn)


# null alternativ kart -------------------------------------------------------------------

## overskudd data

null_alt<- overskudd %>% 
  filter(scenario == "nullalt") %>% 
  group_by(Kommune) %>% 
  summarise(across(kollektiv.yrke_overskudd:bil.yrke,~round(mean(.x))))

null_alt<- left_join(municipalities_gdf,null_alt,by = "Kommune")

## Yrke reise_med kollektiv

null_alt %>% ggplot()+
  geom_sf(aes(fill = kollektiv.yrke_overskudd),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for yrkesreise med kollektiv")+
  guides(fill = guide_legend(title = ""))


## Yrke reise med bil

null_alt %>% ggplot()+
  geom_sf(aes(fill = bil.yrke),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for yrkesreise med bil")+
  guides(fill = guide_legend(title = ""))


## ferie reise med kollektiv

null_alt %>% ggplot()+
  geom_sf(aes(fill = kollektiv.ferie_overskudd),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for feriereise med kollektiv")+
  guides(fill = guide_legend(title = ""))


## ferie reise med bil


null_alt %>% ggplot()+
  geom_sf(aes(fill = bil.ferie),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for feriereise med bil")+
  guides(fill = guide_legend(title = ""))


# scenarioer --------------------------------------------------------------

###Rogfast

rogfast<- overskudd %>% 
  filter(scenario == "rogfast") %>% 
  group_by(Kommune) %>% 
  summarise(across(kollektiv.yrke_overskudd:bil.yrke,~round(mean(.x))))

rogfast<- left_join(municipalities_gdf,rogfast,by = "Kommune")

#### yrke - bil

rogfast %>% ggplot()+
  geom_sf(aes(fill = bil.yrke),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Rogfast: yrkesreise med bil")+
  guides(fill = guide_legend(title = ""))


#### yrke - kollektiv

rogfast %>% ggplot()+
  geom_sf(aes(fill = kollektiv.yrke_overskudd),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Rogfast: yrkesreise med kollektiv")+
  guides(fill = guide_legend(title = ""))


#### ferie - bil

rogfast %>% ggplot()+
  geom_sf(aes(fill = bil.ferie),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Rogfast: ferie reise med bil")+
  guides(fill = guide_legend(title = ""))


#### ferie - kollektiv

rogfast %>% ggplot()+
  geom_sf(aes(fill = kollektiv.ferie_overskudd),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Rogfast: ferie reise med kollektiv")+
  guides(fill = guide_legend(title = ""))



# ferje fri E39 - nord ----------------------------------------------------


ff_nord<-overskudd %>% 
  filter(scenario == "ferjefri_nord") %>% 
  group_by(Kommune) %>% 
  summarise(across(kollektiv.yrke_overskudd:bil.yrke,~round(mean(.x)))) %>% 
  pivot_longer(kollektiv.yrke_overskudd:bil.yrke,names_to = "reisemidler", values_to = "overskudd") %>% 
  mutate(reisemidler = case_when(reisemidler == "kollektiv.yrke_overskudd" ~"Yrkesreise - kollektiv",
                                 reisemidler == "kollektiv.ferie_overskudd" ~"Ferie - kollektiv",
                                 reisemidler == "bil.ferie"~ "Ferie - bil",
                                 reisemidler == "bil.yrke" ~ "Yrkesreise - bil",.default = reisemidler))

ff_nord<- left_join(municipalities_gdf,ff_nord, by = "Kommune")

ff_nord %>%  ggplot()+
  geom_sf(aes(fill = overskudd),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  facet_wrap(~reisemidler)+
  labs(title = "Ferje fri E39 - Nordre del")+
  guides(fill = guide_legend(title = ""))


# ferje fri e39 alt -------------------------------------------------------




ff_alt<-overskudd %>% 
  filter(scenario == "ferjefri_alt") %>% 
  group_by(Kommune) %>% 
  summarise(across(kollektiv.yrke_overskudd:bil.yrke,~round(mean(.x)))) %>% 
  pivot_longer(kollektiv.yrke_overskudd:bil.yrke,names_to = "reisemidler", values_to = "overskudd") %>% 
  mutate(reisemidler = case_when(reisemidler == "kollektiv.yrke_overskudd" ~"Yrkesreise - kollektiv",
                                 reisemidler == "kollektiv.ferie_overskudd" ~"Ferie - kollektiv",
                                 reisemidler == "bil.ferie"~ "Ferie - bil",
                                 reisemidler == "bil.yrke" ~ "Yrkesreise - bil",.default = reisemidler))

ff_alt<- left_join(municipalities_gdf,ff_alt, by = "Kommune")

ff_alt %>%  ggplot()+
  geom_sf(aes(fill = overskudd),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  facet_wrap(~reisemidler)+
  labs(title = "Ferje fri E39 - Hele veien")+
  guides(fill = guide_legend(title = ""))
