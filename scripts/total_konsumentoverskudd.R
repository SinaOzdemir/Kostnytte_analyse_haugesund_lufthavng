##################################
#Title: totalt konsumentoverskudd#
#Author: Sina Ozdemir, PhD       #
#        Samfunnsanalytiker      #
#        Karmøy Kommune          #
#Kontakt: sioz@karmoy.kommune.no #
#Dato: 20/12/2024                #
##################################


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","openxlsx","here"))


# data --------------------------------------------------------------------

penger_besparelse<- read.xlsx(here("data","pengerkostnadsbesparelse.xlsx"),sheet = 1) 


tidsbesparelse_verdi<- read.xlsx(here("data","tidsbesparelseverdi_lufthavn_scenario.xlsx"),sheet= 1) 


eksternK_besparelse<- read.xlsx(here("data","overskudd data","ekstern_kostnad_besparelse.xlsx"),sheet = 1)

# total konsumentoverskudd per konsument ----------------------------------
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

kollektiv_tids_penger<- tidsbesparelse_verdi %>% select(Kommune,rute,scenario,contains("kollektiv"),-contains("tidsbesparelse")) %>%
  left_join(.,kollektiv_penger,by = c("Kommune","rute","scenario")) %>%
  mutate(yrke_overskudd = yrke.kollektiv+kollektiv_pris,
         ferie_overskudd = ferie.kollektiv+kollektiv_pris) %>%
  select(-contains("kollektiv"),-reisemidler) %>%
  rename(kollektiv.yrke_overskudd = yrke_overskudd,
         kollektiv.ferie_overskudd = ferie_overskudd)

if(!file.exists(here("results","kollektiv_overskudd_per_reisende.xlsx"))){
  write.xlsx(kollektiv_tids_penger,here("results","kollektiv_overskudd_per_reisende.xslx"))
}

### bil overskudd

#anta at pengekostnad skall ikke endre med alternativer

car_penger_besparelse = penger_besparelse %>% 
  filter(reisemidler == "bil") %>% 
  mutate(scenario = case_when(scenario=="ferjefrie39_nord" ~"ferjefri_nord",
                              scenario == "ferjefrie39_alt"~"ferjefri_alt",.default=scenario))

car_besparelse = tidsbesparelse_verdi %>% 
  select(Kommune,rute,scenario,contains("bil")) %>% 
  left_join(.,car_penger_besparelse,by = c("Kommune","rute","scenario")) %>% 
  mutate(bil.yrke_driver_overskudd = yrke.bil_sjofor+pris,
         bil.yrke_passajerer_overskudd = yrke.bil_passajerer+pris,
         bil.ferie_driver_overskudd = ferie.bil_sjofor + pris,
         bil.ferie_passajerer_overskudd = ferie.bil_passajerer+pris) %>% 
  select(Kommune,rute,scenario,contains("overskudd"))

if(!file.exists(here("results","bil_overskudd_per_reisende.xlsx"))){
  write.xlsx(car_besparelse,file = here("results","bil_overskudd_per_reisende.xlsx"))
}


# besparelse data ---------------------------------------------------------


besparelser<- left_join(car_besparelse,kollektiv_tids_penger,by = c("Kommune","rute","scenario")) %>% 
  left_join(.,eksternK_besparelse,by = c("Kommune","rute"))

besparelse_alt<- besparelser %>% 
  mutate(across(contains("bil."),~.x+eksternK.bil)) %>% 
  mutate(across(contains("kollektiv."),~.x+eksternK.kollektiv))


if(!file.exists(here("results","all_overskudd.xlsx"))){
  write.xlsx(besparelse_alt,file = here("results","all_overskudd.xlsx"))
}


if(!file.exists(here("results","all_overskudd.RDS"))){
  saveRDS(besparelse_alt,file = here("results","all_overskudd.RDS"))
}
