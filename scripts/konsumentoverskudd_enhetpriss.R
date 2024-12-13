#overskudd beregning


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","openxlsx","here"))


# data --------------------------------------------------------------------

kollektiv_data<- read.xlsx(xlsxFile = here("data","lufthavn_reisetid.xlsx"),sheet = 1) %>% 
  group_by(Kommune,Lufthavn) %>% 
  summarise(across(tidsbruk:ferjefrie39_alt_tidsbruk,~round(mean(.x)))) %>% 
  pivot_wider(id_cols = Kommune,names_from = Lufthavn,values_from = tidsbruk:ferjefrie39_alt_tidsbruk,names_sep = "_")


kjøring_data<- read.xlsx(xlsxFile = here("data","lufthavn_reisetid.xlsx"),sheet = 2) %>% 
  group_by(Kommune, Lufthavn) %>% 
  summarise(across(kjoretid:ferjefrie39_alt_bompenger,~round(mean(.x)))) %>% 
  pivot_wider(id_cols = Kommune, names_from = Lufthavn, values_from = kjoretid:ferjefrie39_alt_bompenger)



# tidsberegning -----------------------------------------------------------

##kollektivtidsbesparelse_nullalternativ
kollektiv_tidsbesparelse<- kollektiv_data %>% 
  select(Kommune,contains("tid")) %>% 
  mutate(tid_hlh_flesland = tidsbruk_HLH-tidsbruk_Flesland, #beregner tidsbesparelse for null alternativ
         tid_hlh_sola = tidsbruk_HLH-tidsbruk_Sola,
         tid_hlh_stord = tidsbruk_HLH-tidsbruk_Stord) %>% 
  mutate(rogfast.tid_hlh_flesland = rogfast_tidsbruk_HLH-rogfast_tidsbruk_Flesland, #beregner tidsbesparelse for rogfast alternativ
         rogfast.tid_hlh_sola = rogfast_tidsbruk_HLH-rogfast_tidsbruk_Sola,
         rogfast.tid_hlh_stord = rogfast_tidsbruk_HLH-rogfast_tidsbruk_Stord) %>% 
  mutate(ferjefri_nord.tid_hlh_flesland = ferjefrie39_nord_tidsbruk_HLH-ferjefrie39_nord_tidsbruk_Flesland, #beregner tidsparelse for ferjefri nord (Stord-Os) alternativ
         ferjefri_nord.tid_hlh_sola = ferjefrie39_nord_tidsbruk_HLH-ferjefrie39_nord_tidsbruk_Sola,
         ferjefri_nord.tid_hlh_stord = ferjefrie39_nord_tidsbruk_HLH-ferjefrie39_nord_tidsbruk_Stord) %>% 
  mutate(ferjefri_alt.tid_hlh_flesland = ferjefrie39_alt_tidsbruk_HLH-ferjefrie39_alt_tidsbruk_Flesland, #beregner tidsbesparelse for ferjefri e39 alternativ
         ferjefri_alt.tid_hlh_sola = ferjefrie39_alt_tidsbruk_HLH-ferjefrie39_alt_tidsbruk_Sola,
         ferjefri_alt.tid_hlh_stord = ferjefrie39_alt_tidsbruk_HLH-ferjefrie39_alt_tidsbruk_Stord) %>% 
  mutate(reisemidler = "kollektiv") %>% 
  select(Kommune,reisemidler,matches("tid_"))


##kjøringstidbesparelse

kjøring_tidsbesparelser<- kjøring_data %>% 
  select(Kommune,contains("kjoretid")) %>% 
  mutate(tid_hlh_flesland = kjoretid_HLH-kjoretid_Flesland, #beregner kjøretidsbesparelse for null alternativ
         tid_hlh_sola = kjoretid_HLH-kjoretid_Sola,
         tid_hlh_stord = kjoretid_HLH-kjoretid_Stord) %>% 
  mutate(rogfast.tid_hlh_flesland = Rogfast_kjoretid_HLH-Rogfast_kjoretid_Flesland, #beregner kjøretidsbesparelse for rogfast alternativ
         rogfast.tid_hlh_sola = Rogfast_kjoretid_HLH-Rogfast_kjoretid_Sola,
         rogfast.tid_hlh_stord = Rogfast_kjoretid_HLH-Rogfast_kjoretid_Stord) %>% 
  mutate(ferjefri_nord.tid_hlh_flesland = ferjefrie39_nord_kjoretid_HLH- ferjefrie39_nord_kjoretid_Flesland, #beregner kjøretidsbesparelse for ferjefri nord (Stord-Os) alternativ
         ferjefri_nord.tid_hlh_sola = ferjefrie39_nord_kjoretid_HLH- ferjefrie39_nord_kjoretid_Sola,
         ferjefri_nord.tid_hlh_stord = ferjefrie39_nord_kjoretid_HLH- ferjefrie39_nord_kjoretid_Stord) %>% 
  mutate(ferjefri_alt.tid_hlh_flesland = ferjefrie39_alt_kjoretid_HLH-ferjefrie39_alt_kjoretid_Flesland,
         ferjefri_alt.tid_hlh_sola = ferjefrie39_alt_kjoretid_HLH-ferjefrie39_alt_kjoretid_Sola,
         ferjefri_alt.tid_hlh_stord = ferjefrie39_alt_kjoretid_HLH-ferjefrie39_alt_kjoretid_Stord) %>% 
  mutate(reisemidler = "kjøring") %>% 
  select(Kommune,reisemidler,matches("tid_"),-contains("kjoretid"))
  

totalt_tids_besparelse <- rbind(kollektiv_tidsbesparelse,kjøring_tidsbesparelser) %>% 
  pivot_longer(cols = tid_hlh_flesland:ferjefri_alt.tid_hlh_stord,names_to = "rute",values_to = "tidsbesparelse") %>% 
  mutate(scenario = str_split_i(pattern = "\\.tid_",string = rute, i = 1)) %>% 
  mutate(rute = str_split_i(pattern = "\\.tid_", string = rute, i =2)) %>% 
  mutate(rute = case_when(is.na(rute)~scenario,.default= rute)) %>% 
  mutate(rute = str_remove_all(rute,pattern = "tid_")) %>% 
  pivot_wider(id_cols =c(Kommune,rute,scenario),names_from = reisemidler,values_from = tidsbesparelse)

if(!file.exists(here("data","tidsbesparelse_lufthavn_scenario.xlsx"))){
  write.xlsx(x =totalt_tids_besparelse,file = here("data","tidsbesparelse_lufthavn_scenario.xlsx"))
}


## pris på tidsbesparelse

tøi_pris_data<- read.xlsx(xlsxFile = here("data","tidsverdi_2024.xlsx"),sheet = 2,na.strings = "") %>% 
  mutate(across(under_70km:over_200km_2024,~round(.x))) %>% 
  pivot_longer(cols = under_70km_2024:over_200km_2024,values_to = "verdi",names_to = "avstand") %>% 
  group_by(mode,role,trip_purpose) %>% 
  summarise(verdi = round(mean(verdi,na.rm = T))) %>% 
  ungroup() %>% 
  pivot_wider(id_cols= c(mode,role),names_from = trip_purpose,values_from = verdi)

kollektiv_koefs<- c("yrke" = 614,"ferie"=112)
car_koefs <- c("yrke_driver" =762,"ferie_driver" = 180,"yrke_passajerer" = 611,"ferie_passajerer" = 155)



tidsbesparelse_verdi <- totalt_tids_besparelse %>% 
  mutate(kollektiv = kollektiv/60,
         kjøring = kjøring/60) %>% 
  mutate(yrke_reise_car_driver = kjøring*car_koefs[["yrke_driver"]],
         yrke_reise_car_passajerer = kjøring*car_koefs[["yrke_passajerer"]],
         ferie_reise_car_driver = kjøring*car_koefs[["ferie_driver"]],
         ferie_reise_car_passajerer = kjøring*car_koefs[["ferie_passajerer"]],
         yrke_kollektiv = kollektiv*kollektiv_koefs[["yrke"]],
         ferie_kollektiv = kollektiv*kollektiv_koefs[["ferie"]]) %>% 
  mutate(across(yrke_reise_car_driver:ferie_kollektiv,~round(.x)))


if(!file.exists(here("data","tidsbesparelseverdi_lufthavn_scenario.xlsx"))){
  write.xlsx(x =totalt_tids_besparelse,file = here("data","tidsbesparelseverdi_lufthavn_scenario.xlsx"))
}
