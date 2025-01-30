##################################
#Title: Penger besparelse        #
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

kollektiv_data<- read.xlsx(xlsxFile = here("data","overskudd data","lufthavn_reisetid.xlsx"),sheet = 1) %>% 
  group_by(Kommune,Lufthavn) %>% 
  summarise(across(tidsbruk:ferjefrie39_alt_tidsbruk,~round(mean(.x)))) %>% 
  pivot_wider(id_cols = Kommune,names_from = Lufthavn,values_from = tidsbruk:ferjefrie39_alt_tidsbruk,names_sep = "_")


kjoring_data<- read.xlsx(xlsxFile = here("data","overskudd data","lufthavn_reisetid.xlsx"),sheet = 2) %>% 
  group_by(Kommune, Lufthavn) %>% 
  summarise(across(kjoretid:ferjefrie39_alt_bompenger,~round(mean(.x)))) %>% 
  pivot_wider(id_cols = Kommune, names_from = Lufthavn, values_from = kjoretid:ferjefrie39_alt_bompenger)

# penger kostnad -------------------------------------------------------------

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


kjoring_km_pris<- kjoring_data %>% 
  select(Kommune,contains("km")) %>%
  pivot_longer(cols = contains("km_"),names_to = "Lufthavn",values_to = "km") %>% 
  mutate(Lufthavn = gsub("km_","",Lufthavn)) %>% 
  mutate(mellom_bensin_pris = round(((km/100)*6.93)*22.12)) %>% 
  mutate(mellom_diesel_pris = round(((km/100)*5.6)*20.53)) %>% 
  mutate(mellom_elbil_pris = round(((km/100)*17.10)*1.23))



bompenger<- read.xlsx(xlsxFile = here("data","overskudd data","lufthavn_reisetid.xlsx"),sheet = 2) %>% 
  group_by(Kommune, Lufthavn) %>% 
  summarise(across(kjoretid:ferjefrie39_alt_bompenger,~round(mean(.x)))) %>% 
  select(Kommune,Lufthavn,contains("pris"),contains("bompenger"))

kjoring_penger_kostnad<- left_join(kjoring_km_pris,bompenger,by = c("Kommune","Lufthavn"))

##penger kostnad av kjøring

scenario_penger_kostnad<- kjoring_penger_kostnad %>% 
  ungroup() %>% 
  mutate(nullalt.mellom_diesel = mellom_diesel_pris+freje_pris+bompenger) %>% 
  mutate(rogfast.mellom_diesel = mellom_diesel_pris+Rogfast_ferge_pris+Rogfast_bompenger) %>% 
  mutate(ferjefrie39_nord.mellom_diesel = mellom_diesel_pris+ferjefrie39_nord_ferge_pris+FerjefriE39_nord_bompenger) %>% 
  mutate(ferjefrie39_alt.mellom_diesel = mellom_diesel_pris + 0 + ferjefrie39_alt_bompenger) %>% 
  select(Kommune,Lufthavn,contains(".")) %>% 
  pivot_wider(id_cols = Kommune, names_from = Lufthavn, values_from = nullalt.mellom_diesel:ferjefrie39_alt.mellom_diesel,names_sep = ".")


scenario_penger_besparelse<- scenario_penger_kostnad%>% 
  mutate(nullalt.mellom_diesel.hlh_flesland =nullalt.mellom_diesel.HLH-nullalt.mellom_diesel.Flesland, #beregner kroner besparelseverdi av drivstoff,bompenger og ferje på null alternativ
         nullalt.mellom_diesel.hlh_sola = nullalt.mellom_diesel.HLH-nullalt.mellom_diesel.Sola,
         nullalt.mellom_diesel.hlh_stord = nullalt.mellom_diesel.HLH-nullalt.mellom_diesel.Stord) %>% 
  mutate(rogfast.mellom_diesel.hlh_flesland = rogfast.mellom_diesel.HLH-rogfast.mellom_diesel.Flesland, #beregner kroner besparelseverdi av drivstoff, bompenger, og ferje på rogfast alternativ
         rogfast.mellom_diesel.hlh_sola = rogfast.mellom_diesel.HLH-rogfast.mellom_diesel.Sola,
         rogfast.mellom_diesel.hlh_stord = rogfast.mellom_diesel.HLH-rogfast.mellom_diesel.Stord) %>% 
  mutate(ferjefrie39_nord.mellom_diesel.hlh_flesland = ferjefrie39_nord.mellom_diesel.HLH-ferjefrie39_nord.mellom_diesel.Flesland, #beregner kroner besparelseverdi av drivstoff, bompenger, og ferje på ferje fri nord e39
         ferjefrie39_nord.mellom_diesel.hlh_sola = ferjefrie39_nord.mellom_diesel.HLH-ferjefrie39_nord.mellom_diesel.Sola,
         ferjefrie39_nord.mellom_diesel.hlh_stord = ferjefrie39_nord.mellom_diesel.HLH-ferjefrie39_nord.mellom_diesel.Stord) %>% 
  mutate(ferjefrie39_alt.mellom_diesel.hlh_flesland = ferjefrie39_alt.mellom_diesel.HLH-ferjefrie39_alt.mellom_diesel.Flesland,
         ferjefrie39_alt.mellom_diesel.hlh_sola = ferjefrie39_alt.mellom_diesel.HLH-ferjefrie39_alt.mellom_diesel.Sola,
         ferjefrie39_alt.mellom_diesel.hlh_stord = ferjefrie39_alt.mellom_diesel.HLH-ferjefrie39_alt.mellom_diesel.Stord) %>% 
  select(Kommune,contains("hlh_flesland"),contains("hlh_sola"),contains("hlh_stord")) %>% 
  pivot_longer(cols = nullalt.mellom_diesel.hlh_flesland:ferjefrie39_alt.mellom_diesel.hlh_stord,
               names_to = "alternatives",values_to = "pris") %>% 
  mutate(rute = str_split_i(alternatives,pattern = "\\.",i = 3)) %>% 
  mutate(scenario = str_split_i(alternatives,pattern = "\\.",i=1)) %>% 
  select(-alternatives) %>% 
  mutate(reisemidler = "bil")

penger_besparelse<- rbind(kollektiv_pris,scenario_penger_besparelse)  

if(!file.exists(here("data","pengerkostnadsbesparelse.xlsx"))){
  write.xlsx(penger_besparelse,here("data","pengerkostnad_besparelse.xlsx"))
}




