#Heatmap of the results



# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","sf","ggthemes","geodata"))

library(rnaturalearth)
library(rnaturalearthdata)
#devtools::install_github("rspatial/geodata") #server is down for the moment

# fetch the base map ------------------------------------------------------

#base_map<- geodata::gadm(country = "Norway",level = 2,path = tempdir())


# norway_map <- ne_countries(scale = "medium", geounit = "municipality",returnclass = "sf") %>%
#   filter(admin == "Norway")


# test --------------------------------------------------------------------


url <- "https://nedlasting.geonorge.no/geonorge/Basisdata/Kommuner/GeoJSON/Basisdata_0000_Norge_25833_Kommuner_GeoJSON.zip"
temp_file <- tempfile()
download.file(url, temp_file)
unzip_dir <- tempdir()
unzip(temp_file, exdir = unzip_dir)
geojson_file <- list.files(unzip_dir, pattern = "\\.geojson$", full.names = TRUE)

# Read the GeoJSON file into an sf object
municipalities_gdf <- st_read(geojson_file,layer = "Kommune")

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

Odata<- Adata %>% 
  filter(scenario == "nullalt") %>% 
  group_by(Kommune) %>% 
  summarise(across(kollektiv.yrke_overskudd:bil.ferie,~mean(.x))) %>% 
  mutate(Kommune = case_when(Kommune == "Tysvaer"~"Tysvær",.default = Kommune))


Pdata<- left_join(municipalites_gdf_filtered,Odata,by = "Kommune")


## Yrke reise_med kollektiv

Pdata %>% ggplot()+
  geom_sf(aes(fill = kollektiv.yrke_overskudd),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_gradient2(low="green",mid = "yellow",high = "red")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for yrkesreise med kollektiv")+
  guides(fill = guide_legend(title = ""))


## Yrke reise med bil

Pdata %>% ggplot()+
  geom_sf(aes(fill = bil.yrke),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_gradient2(low="green",mid = "yellow",high = "red")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for yrkesreise med bil")+
  guides(fill = guide_legend(title = ""))


## ferie reise med kollektiv

Pdata %>% ggplot()+
  geom_sf(aes(fill = kollektiv.ferie_overskudd),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_gradient2(low="green",mid = "yellow",high = "red")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for feriereise med kollektiv")+
  guides(fill = guide_legend(title = ""))


## ferie reise med bil


Pdata %>% ggplot()+
  geom_sf(aes(fill = bil.ferie),color = "black")+
  geom_sf_label(aes(label = Kommune),color = "black")+
  scale_fill_gradient2(low="green",mid = "yellow",high = "red")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())+
  labs(title = "Konsument overskudd for feriereise med bil")+
  guides(fill = guide_legend(title = ""))
