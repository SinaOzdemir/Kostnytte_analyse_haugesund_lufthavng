#Heatmap of the results



# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","sf","ggthemes","geodata","here"))

library(rnaturalearth)
library(rnaturalearthdata)
#devtools::install_github("rspatial/geodata") #server is down for the moment

# fetch the base map ------------------------------------------------------

# 
# url <- "https://nedlasting.geonorge.no/geonorge/Basisdata/Kommuner/GeoJSON/Basisdata_0000_Norge_25833_Kommuner_GeoJSON.zip"
# temp_file <- tempfile()
# download.file(url, temp_file)
# unzip_dir <- "C:/Users/sioz/Desktop/arbeid/prosjektene/Kostnytte_analyse_haugesund_lufthavng/data/auxilary data"
# unzip(temp_file, exdir = unzip_dir)
geojson_file <- list.files(here("data","auxilary data"), pattern = "\\.geojson$", full.names = TRUE)

# Read the GeoJSON file into an sf object
municipalities_gdf <- st_read(geojson_file,layer = "Kommune") %>% 


# test --------------------------------------------------------------------


municipalites_gdf_filtered<- municipalities_gdf %>%
  filter(kommunenavn %in% c(unique(Adata$Kommune),"Tysvær")) %>% 
  rename(Kommune = kommunenavn)

test_info<- savedata %>% 
  mutate(Kommune = case_when(Kommune == "Tysvaer"~"Tysvær",.default = Kommune)) %>% 
  group_by(Kommune) %>% 
  summarise(bil.ferie = mean(bil.ferie)) %>% 
  ungroup()


test<- left_join(municipalites_gdf_filtered,test_info,by = "Kommune")

test %>%  ggplot()+
  geom_sf(aes(fill = bil.ferie),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_gradient2(low="green",mid = "yellow",high = "red")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd med bil for ferie")


# apply -------------------------------------------------------------------

## overskudd data

Odata<-  readRDS(file = here("results","all_overskudd.RDS")) %>% 
  mutate(bil.yrke = round((bil.yrke_driver_overskudd+bil.yrke_passajerer_overskudd)/2,0)) %>% 
  mutate(bil.ferie = round((bil.ferie_driver_overskudd+bil.ferie_passajerer_overskudd)/2,0)) %>% 
  select(-contains("driver"),-contains("passajerer")) %>% 
  filter(scenario == "nullalt") %>% 
  group_by(Kommune) %>% 
  summarise(across(kollektiv.yrke_overskudd:bil.ferie,~mean(.x))) %>% 
  mutate(Kommune = case_when(Kommune == "Tysvaer"~"Tysvær",.default = Kommune))


Pdata<- municipalities_gdf %>% 
  rename(Kommune = kommunenavn) %>% 
  filter(Kommune %in% unique(Odata$Kommune)) %>% 
  left_join(.,Odata,by = "Kommune")


## Yrke reise_med kollektiv

Pdata %>% ggplot()+
  geom_sf(aes(fill = kollektiv.yrke_overskudd),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for yrkesreise med kollektiv")+
  guides(fill = guide_legend(title = ""))


## Yrke reise med bil

Pdata %>% ggplot()+
  geom_sf(aes(fill = bil.yrke),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for yrkesreise med bil")+
  guides(fill = guide_legend(title = ""))


## ferie reise med kollektiv

Pdata %>% ggplot()+
  geom_sf(aes(fill = kollektiv.ferie_overskudd),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for feriereise med kollektiv")+
  guides(fill = guide_legend(title = ""))


## ferie reise med bil


Pdata %>% ggplot()+
  geom_sf(aes(fill = bil.ferie),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_viridis_c(option = "inferno")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for feriereise med bil")+
  guides(fill = guide_legend(title = ""))


# scenarioer --------------------------------------------------------------

### test med null alt
Edata<- Odata %>% 
  filter(scenario == "nullalt") %>% 
  pivot_longer(bil.yrke_driver_overskudd:kollektiv.ferie_overskudd,names_to = "indikator",values_to = "besparelse") %>% 
  group_by(Kommune,indikator) %>% 
  summarise(besparelse = round(mean(besparelse),0)) %>% 
  mutate(Kommune = case_when(Kommune == "Tysvaer" ~ "Tysvær",.default = Kommune)) %>% 
  ungroup()

municipalities<- unique(Edata$Kommune)

gdf_filtered<- municipalities_gdf %>% 
  filter(kommunenavn %in%municipalities) %>% 
  select(kommunenavn,geometry) %>% 
  rename(Kommune = kommunenavn)


Mdata<- gdf_filtered %>% 
  left_join(.,Edata,by = "Kommune")


Mdata %>% ggplot()+
  geom_sf(aes(geometry = geometry,fill = besparelse),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_continuous(type = "viridis")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Konsument overskudd for feriereise med bil")+
  guides(fill = guide_legend(title = ""))+
  facet_wrap(~indikator)


###scenarioer
#TODO: need to fix it, plots look shit
scenario <- unique(Odata$scenario)

for (i in scenario) {
  plot_info<- Odata %>% 
    filter(scenario == i) %>% 
    pivot_longer(bil.yrke_driver_overskudd:kollektiv.ferie_overskudd,names_to = "indikator",values_to = "besparelse") %>% 
    group_by(Kommune,indikator) %>% 
    summarise(besparelse = round(mean(besparelse),0)) %>% 
    mutate(Kommune = case_when(Kommune == "Tysvaer" ~ "Tysvær",.default = Kommune)) %>% 
    ungroup()
  
  plot_data<-gdf_filtered %>% 
    left_join(.,plot_info,by = "Kommune")
  
  plot<- plot_data %>% ggplot()+
    geom_sf(aes(geometry = geometry,fill = besparelse),color = "black")+
    geom_sf_label(aes(label = Kommune),color = "black")+
    scale_fill_continuous(type = "viridis")+
    theme_minimal()+
    theme(legend.position = "bottom")+
    labs(title = paste0("scenario: ", i))+
    guides(fill = guide_legend(title = ""))+
    facet_wrap(~indikator)
  
  if(!dir.exists(here("graphs","scenario_graphs",i))){
    dir.create(here("graphs","scenario_graphs",i))
  }
  
  ggsave(filename = here("graphs","scenario_graphs",i,paste0(i,".png")),plot = plot,width = 1290,height = 707,units = "px")
}