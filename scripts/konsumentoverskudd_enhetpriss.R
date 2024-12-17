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
  mutate(across(yrke_reise_car_driver:ferie_kollektiv,~round(.x))) %>% 
  select(-kollektiv,-kjøring)


if(!file.exists(here("data","tidsbesparelseverdi_lufthavn_scenario.xlsx"))){
  write.xlsx(x =tidsbesparelse_verdi,file = here("data","tidsbesparelseverdi_lufthavn_scenario.xlsx"))
}



# korner pris -------------------------------------------------------------

## kollektiv transport

kollektiv_pris<- kollektiv_data %>% 
  select(Kommune,contains("pris")) %>% 
  mutate(rutepris.hlh_flesland = pris_HLH-pris_Flesland,
         rutepris.hlh_sola = pris_HLH-pris_Sola,
         rutepris.hlh_stord = pris_HLH-pris_Stord) %>% 
  select(Kommune,contains("rutepris")) %>% 
  pivot_longer(cols = contains("rutepris"),names_to = "rute",values_to = "pris") %>% 
  mutate(rute = gsub("rutepris.","",rute)) %>% 
  mutate(reisemidler = "kollektiv") %>% 
  mutate(scenario = "nullalt")

##kjøring km pris


kjøring_km_pris<- kjøring_data %>% 
  select(Kommune,contains("km")) %>%
  pivot_longer(cols = contains("km_"),names_to = "Lufthavn",values_to = "km") %>% 
  mutate(Lufthavn = gsub("km_","",Lufthavn)) %>% 
  mutate(mellom_bensin_pris = round(((km/100)*6.93)*22.12)) %>% 
  mutate(mellom_diesel_pris = round(((km/100)*5.6)*20.53)) %>% 
  mutate(mellom_elbil_pris = round(((km/100)*17.10)*1.23))

bompenger<- read.xlsx(xlsxFile = here("data","lufthavn_reisetid.xlsx"),sheet = 2) %>% 
  group_by(Kommune, Lufthavn) %>% 
  summarise(across(kjoretid:ferjefrie39_alt_bompenger,~round(mean(.x)))) %>% 
  select(Kommune,Lufthavn,contains("pris"),contains("bompenger"))

kjøring_penger_kostnad<- left_join(kjøring_km_pris,bompenger,by = c("Kommune","Lufthavn"))

##penger kostnad av kjøring

scenario_penger_kostnad<- kjøring_penger_kostnad %>% 
  ungroup() %>% 
  mutate(nullalt.mellom_besin = mellom_bensin_pris+freje_pris+bompenger) %>% 
  mutate(rogfast.mellom_bensin = mellom_bensin_pris+Rogfast_ferge_pris+Rogfast_bompenger) %>% 
  mutate(ferjefrie39_nord.mellom_bensin = mellom_bensin_pris+ferjefrie39_nord_ferge_pris+FerjefriE39_nord_bompenger) %>% 
  mutate(ferjefrie39_alt.mellom_bensin = mellom_bensin_pris + 0 + ferjefrie39_alt_bompenger) %>% 
  select(Kommune,Lufthavn,contains(".")) %>% 
  pivot_wider(id_cols = Kommune, names_from = Lufthavn, values_from = nullalt.mellom_besin:ferjefrie39_alt.mellom_bensin,names_sep = ".") %>% 
  mutate(nullalt.mellom_bensin.hlh_flesland =nullalt.mellom_besin.HLH-nullalt.mellom_besin.Flesland, #beregner kroner besparelseverdi av drivstoff,bompenger og ferje på null alternativ
         nullalt.mellom_bensin.hlh_sola = nullalt.mellom_besin.HLH-nullalt.mellom_besin.Sola,
         nullalt.mellom_bensin.hlh_stord = nullalt.mellom_besin.HLH-nullalt.mellom_besin.Stord) %>% 
  mutate(rogfast.mellom_bensin.hlh_flesland = rogfast.mellom_bensin.HLH-rogfast.mellom_bensin.Flesland, #beregner kroner besparelseverdi av drivstoff, bompenger, og ferje på rogfast alternativ
         rogfast.mellom_bensin.hlh_sola = rogfast.mellom_bensin.HLH-rogfast.mellom_bensin.Sola,
         rogfast.mellom_bensin.hlh_stord = rogfast.mellom_bensin.HLH-rogfast.mellom_bensin.Stord) %>% 
  mutate(ferjefrie39_nord.mellom_bensin.hlh_flesland = ferjefrie39_nord.mellom_bensin.HLH-ferjefrie39_nord.mellom_bensin.Flesland, #beregner kroner besparelseverdi av drivstoff, bompenger, og ferje på ferje fri nord e39
         ferjefrie39_nord.mellom_bensin.hlh_sola = ferjefrie39_nord.mellom_bensin.HLH-ferjefrie39_nord.mellom_bensin.Sola,
         ferjefrie39_nord.mellom_bensin.hlh_stord = ferjefrie39_nord.mellom_bensin.HLH-ferjefrie39_nord.mellom_bensin.Stord) %>% 
  mutate(ferjefrie39_alt.mellom_bensin.hlh_flesland = ferjefrie39_alt.mellom_bensin.HLH-ferjefrie39_alt.mellom_bensin.Flesland,
         ferjefrie39_alt.mellom_bensin.hlh_sola = ferjefrie39_alt.mellom_bensin.HLH-ferjefrie39_alt.mellom_bensin.Sola,
         ferjefrie39_alt.mellom_bensin.hlh_stord = ferjefrie39_alt.mellom_bensin.HLH-ferjefrie39_alt.mellom_bensin.Stord) %>% 
  select(Kommune,contains("hlh_flesland"),contains("hlh_sola"),contains("hlh_stord")) %>% 
  pivot_longer(cols = nullalt.mellom_bensin.hlh_flesland:ferjefrie39_alt.mellom_bensin.hlh_stord,
               names_to = "alternatives",values_to = "pris") %>% 
  mutate(rute = str_split_i(alternatives,pattern = "\\.",i = 3)) %>% 
  mutate(scenario = str_split_i(alternatives,pattern = "\\.",i=1)) %>% 
  select(-alternatives) %>% 
  mutate(reisemidler = "car")
  
penger_besparelse<- rbind(kollektiv_pris,scenario_penger_kostnad)  

if(!file.exists(here("data","pengerkostnadsbesparelse.xlsx"))){
  write.xlsx(penger_besparelse,here("data","pengerkostnadsbesparelse.xlsx"))
}



# total konsumentoverskudd per konsument ----------------------------------


penger_besparelse<- read.xlsx(here("data","pengerkostnadsbesparelse.xlsx"),sheet = 1) 

tidsbesparelse_verdi<- read.xlsx(here("data","tidsbesparelseverdi_lufthavn_scenario.xlsx"),sheet= 1) %>% 
  mutate(scenario = case_when(scenario %in% c("tid_hlh_flesland","tid_hlh_sola","tid_hlh_stord")~"nullalt",.default = scenario))

###kollektiv overskudd

###anta at kollektiv prisene ikke skal forandre seg med alternativer

rogfast_kollektiv_pris<- penger_besparelse %>% 
  filter(reisemidler == "kollektiv") %>% 
  mutate(scenario = "rogfast")


ferjefri_nord_kollektiv_pris<- penger_besparelse %>% 
  filter(reisemidler == "kollektiv") %>% 
  mutate(scenario = "ferjefri_nord")


ferjefri_alt_kollektiv_pris<- penger_besparelse %>% 
  filter(reisemidler == "kollektiv") %>% 
  mutate(scenario = "ferjefri_alt")

nullalt_kollektiv_pris<- penger_besparelse %>% 
  filter(reisemidler == "kollektiv")

kollektiv_penger<- rbind(nullalt_kollektiv_pris,rogfast_kollektiv_pris,ferjefri_nord_kollektiv_pris,ferjefri_alt_kollektiv_pris) %>% 
  rename(kollektiv_pris = pris) %>% 
  mutate(rute = tolower(rute))

kollektiv_tids_penger<- tidsbesparelse_verdi %>% select(Kommune,rute,scenario,contains("kollektiv")) %>% 
  left_join(.,kollektiv_penger,by = c("Kommune","rute","scenario")) %>% 
  mutate(yrke_overskudd = yrke_kollektiv+kollektiv_pris,
         ferie_overskudd = ferie_kollektiv+kollektiv_pris) %>% 
  select(-contains("kollektiv"),-reisemidler) %>% 
  rename(kollektiv.yrke_overskudd = yrke_overskudd,
         kollektiv.ferie_overskudd = ferie_overskudd)

if(!file.exists(here("results","kollektiv_overskudd_per_reisende.xlsx"))){
  write.xlsx(kollektiv_tids_penger,here("results","kollektiv_overskudd_per_reisende.xslx"))
}

### bil overskudd

#anta at pengekostnad skall ikke endre med alternativer

car_penger_besparelse = penger_besparelse %>% 
  filter(reisemidler == "car") %>% 
  mutate(scenario = case_when(scenario=="ferjefrie39_nord" ~"ferjefri_nord",
                              scenario == "ferjefrie39_alt"~"ferjefri_alt",.default=scenario))

car_besparelse = tidsbesparelse_verdi %>% 
  select(Kommune,rute,scenario,contains("car")) %>% 
  left_join(.,car_penger_besparelse,by = c("Kommune","rute","scenario")) %>% 
  mutate(bil.yrke_driver_overskudd = yrke_reise_car_driver+pris,
         bil.yrke_passajerer_overskudd = round((2*yrke_reise_car_passajerer)+(pris/2)),
         bil.ferie_driver_overskudd = ferie_reise_car_driver + pris,
         bil.ferie_passajerer_overskudd = round((2*ferie_reise_car_passajerer)+(pris/2))
  ) %>% 
  select(Kommune,rute,scenario,contains("overskudd"))

if(!file.exists(here("results","bil_overskudd_per_reisende.xlsx"))){
  write.xlsx(car_besparelse,file = here("results","bil_overskudd_per_reisende.xlsx"))
}


# besparelse data ---------------------------------------------------------


besparelse_alt<- left_join(car_besparelse,kollektiv_tids_penger,by = c("Kommune","rute","scenario"))

if(!file.exists(here("results","all_overskudd.xlsx"))){
  write.xlsx(besparelse_alt,file = here("results","all_overskudd.xlsx"))
}