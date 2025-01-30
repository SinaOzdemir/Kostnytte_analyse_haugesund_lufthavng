
#######################################
#Title: flybilett prispoeng simulering#
#Author: Sina Ozdemir, PhD            #
#        Samfunnsanalytiker           #
#        Karmøy Kommune               #
#Kontakt: sioz@karmoy.kommune.no      #
#Dato: 16/01/2024                     #
#######################################


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","openxlsx","here","beepr","patchwork"))


# data --------------------------------------------------------------------


kollektiv_data<- read.xlsx(xlsxFile = here("data","overskudd data","lufthavn_reisetid.xlsx"),sheet = 1) %>% 
  group_by(Kommune,Lufthavn) %>% 
  summarise(across(tidsbruk:ferjefrie39_alt_tidsbruk,~round(mean(.x)))) 

kjoring_data<- read.xlsx(xlsxFile = here("data","overskudd data","lufthavn_reisetid.xlsx"),sheet = 2) %>% 
  group_by(Kommune, Lufthavn) %>% 
  summarise(across(kjoretid:ferjefrie39_alt_bompenger,~round(mean(.x)))) %>% 
  pivot_wider(id_cols = Kommune, names_from = Lufthavn, values_from = kjoretid:ferjefrie39_alt_bompenger)

# gjenberegn kostnader ----------------------------------------------------

##kjøring

#kroner verdi av km 
kjoring_km_pris<- kjoring_data %>% 
  select(Kommune,contains("km")) %>%
  pivot_longer(cols = contains("km_"),names_to = "Lufthavn",values_to = "km") %>% 
  mutate(Lufthavn = gsub("km_","",Lufthavn)) %>% 
  mutate(mellom_bensin_pris = round(((km/100)*6.93)*22.12)) %>% 
  mutate(mellom_diesel_pris = round(((km/100)*5.6)*20.53)) %>% 
  mutate(mellom_elbil_pris = round(((km/100)*17.10)*1.23)) %>% 
  select(Kommune,Lufthavn,km,mellom_diesel_pris)


#bompenger per kjoring
bompenger<- kjoring_data %>% 
  select(Kommune,contains("bompenger"),-contains("Rogfast"),-contains("E39")) %>% 
  pivot_longer(contains("bompenger"),names_to = "Lufthavn",values_to = "bompenger") %>% 
  mutate(Lufthavn = gsub("bompenger_","",Lufthavn))

#fly billet
oslo_billett<- read.xlsx(here("data","auxilary data","oslo_fligt_prices.xlsx")) %>% 
  drop_na(pris) %>% 
  group_by(Lufthavn) %>% 
  summarise(billet_pris = min(pris))

# ferje pris
ferje_pris<- kjoring_data %>% 
  select(Kommune,contains("freje_pris_"),-contains("rogfast"),-contains("E39")) %>% 
  pivot_longer(contains("freje_pris"),names_to = "Lufthavn",values_to = "ferje_pris") %>% 
  mutate(Lufthavn = gsub("freje_pris_","",Lufthavn))

# valuta kostnad
kjoring_penger_kostnad<- left_join(kjoring_km_pris,bompenger,by = c("Kommune","Lufthavn")) %>% 
  left_join(.,ferje_pris,by = c("Kommune","Lufthavn")) %>% 
  mutate(Lufthavn = case_when(Lufthavn == "HLH" ~"HAU",.default = Lufthavn)) %>% 
  left_join(.,oslo_billett,by = "Lufthavn") %>% 
  mutate(mellom_diesel_pris = mellom_diesel_pris*2,
         bompenger = bompenger*2,
         ferje_pris = ferje_pris*2) %>% 
  select(-km) %>% 
  pivot_wider(names_from = Lufthavn,values_from = c(mellom_diesel_pris,bompenger,billet_pris,ferje_pris))


write.xlsx(kjoring_penger_kostnad,file = here("data","pris sensitivitet data","oslo_bil_sensitivitet.xlsx"))
###kollektiv


kollektiv_penger_kostnad<- kollektiv_data %>% 
  select(Kommune,Lufthavn,pris) %>% 
  mutate(Lufthavn = case_when(Lufthavn == "HLH" ~"HAU",.default = Lufthavn)) %>% 
  left_join(.,oslo_billett,by = "Lufthavn") %>% 
  mutate(pris = pris*2) %>% 
  pivot_wider(names_from = Lufthavn,values_from = c(pris,billet_pris))

write.xlsx(kollektiv_penger_kostnad, file = here("data","pris sensitivitet data","oslo_kollektiv_sensitivitet.xlsx"))

# tids besparelse ---------------------------------------------------------

kjoring_tid<- kjoring_data %>% 
  select(Kommune,contains("kjoretid"),-contains("Rogfast"),-contains("E39")) %>% 
  mutate(HAU_Flesland = kjoretid_HLH-kjoretid_Flesland,
         HAU_Sola = kjoretid_HLH-kjoretid_Sola,
         HAU_Stord = kjoretid_HLH-kjoretid_Stord) %>% 
  mutate(across(HAU_Flesland:HAU_Stord,~.x/60)) %>% 
  pivot_longer(HAU_Flesland:HAU_Stord,names_to = "rute",values_to = "tidsbesparelse") %>% 
  mutate(yrke.bil_sjofor = tidsbesparelse*762,
         yrke.bil_passajerer = tidsbesparelse*611,
         ferie.bil_sjofor = tidsbesparelse * 180,
         ferie.bil_passajerer = tidsbesparelse * 155) %>% 
  select(Kommune,rute,contains("yrke"),contains("ferie"))

kollektiv_tid<- kollektiv_data %>% 
  select(Kommune,Lufthavn,contains("tidsbruk"),-contains("rogfast"),-contains("e39")) %>% 
  pivot_wider(names_from = Lufthavn,values_from = tidsbruk) %>% 
  mutate(HAU_Flesland = HLH-Flesland,
         HAU_Sola = HLH-Sola,
         HAU_Stord = HLH-Stord) %>% 
  mutate(across(HAU_Flesland:HAU_Stord, ~.x/60)) %>% 
  pivot_longer(HAU_Flesland:HAU_Stord,names_to = "rute",values_to = "tidsbesparelse") %>% 
  mutate(yrke.kollektiv = tidsbesparelse * 614,
         ferie.kollektiv = tidsbesparelse *112) %>% 
  select(Kommune, rute,yrke.kollektiv,ferie.kollektiv)
  

left_join(kjoring_tid,kollektiv_tid,by = c("Kommune","rute")) %>% 
  write.xlsx(.,file = here("data","pris sensitivitet data","tids_verdi.xlsx"))


# simulering --------------------------------------------------------------

simulering_resultater<-data.frame()

for (i in 0:100) {
  
  ##kjøring penger simulering
  
  x= 100*i
  
  cat("itteration: ",i,", increasing the ticket price by: ",x,"\n")
  
  kjoring_penger<-kjoring_penger_kostnad %>% 
    mutate(billet_pris_HAU = billet_pris_HAU+x) %>% 
    rowwise() %>% 
    mutate(HAU_cost = sum(pick(contains("HAU")))) %>% #total valuta kostnad å bruke HAU
    mutate(Flesland_cost = sum(pick(contains("Flesland")))) %>% #total valuta kostnad å bruke Flesland
    mutate(Sola_cost = sum(pick(contains("Sola")))) %>% #total valuta kostnad å bruke Sola
    mutate(Stord_cost = sum(pick(contains("Stord")))) %>% #total valuta kostnad å bruke Stord
    ungroup() %>% 
    select(Kommune, contains("cost")) %>% #behold kun nødvendige variabler
    mutate(HAU_Flesland = HAU_cost-Flesland_cost, # overskudd: HAU mot Flesland
           HAU_Sola = HAU_cost-Sola_cost, # overskudd: HAU mot Sola
           HAU_Stord = HAU_cost-Stord_cost) %>% #overskudd: HAU mot Stord
    pivot_longer(HAU_Flesland:HAU_Stord,names_to = "rute",values_to = "penger_sparelse") %>% # long data av overskudd
    select(Kommune,rute,penger_sparelse) %>% 
    mutate(itter = i)
  
  kjoring_cost<- left_join(kjoring_penger,kjoring_tid,by = c("Kommune","rute")) %>% 
    mutate(yrke.bil_passajerer = penger_sparelse + yrke.bil_passajerer, #beregn valuta + tid overskudd
           yrke.bil_sjofor = penger_sparelse + yrke.bil_sjofor,
           ferie.bil_passajerer = penger_sparelse + ferie.bil_passajerer,
           ferie.bil_sjofor = penger_sparelse + ferie.bil_sjofor)
  
  kollektiv_penger<- kollektiv_penger_kostnad %>% 
    mutate(billet_pris_HAU = billet_pris_HAU+x) %>% 
    rowwise() %>% 
    mutate(HAU_cost = sum(pick(contains("HAU"))), #total valuta kostnad å bruke HAU med kollektiv
           Flesland_cost = sum(pick(contains("Flesland"))), #total valuta kostnad å bruke Flesland med kollektiv
           Stord_cost = sum(pick(contains("Stord"))), #total valuta kostnad å bruke Stord med kollektiv
           Sola_cost = sum(pick(contains("Sola")))) %>%  #total valuta kostnad å bruke Sola med kollektiv
    ungroup() %>% 
    mutate(HAU_Flesland = HAU_cost-Flesland_cost, #overskudd: HAU mot Flesland med kollektiv
           HAU_Sola = HAU_cost-Sola_cost, #overskudd: HAU mot Sola med kollektiv
           HAU_Stord = HAU_cost - Stord_cost) %>% # overskudd: HAU mot Stord med kollektiv
    select(Kommune,HAU_Flesland:HAU_Stord) %>%
    pivot_longer(HAU_Flesland:HAU_Stord,names_to = "rute",values_to = "penger_sparelse") %>%  #long data av overskud med kollektiv
    mutate(itter = i)
  
  kollektiv_cost<- left_join(kollektiv_penger,kollektiv_tid,by = c("Kommune","rute")) %>% 
    mutate(yrke.kollektiv = penger_sparelse+yrke.kollektiv, # total overskudd (valuta + tid) fra alternativer
           ferie.kollektiv = penger_sparelse + ferie.kollektiv)
 
  #slå sammen kollektiv og privat kjøring overskudd informasjon   
 sim_res<- left_join(kjoring_cost,kollektiv_cost,by = c("Kommune","rute","itter")) %>% select(-contains("penger_sparelse"))
 
 
 simulering_resultater<-rbind(simulering_resultater,sim_res)
 # lagre data for videre analyse
 write.xlsx(simulering_resultater,file = here("results","Oslo_rute_overskudd_simulering.xlsx"))
 
}

#beepr::beep("fanfare")


# visualisation -----------------------------------------------------------


graph_data<-read.xlsx(here("results","Oslo_rute_overskudd_simulering.xlsx")) %>% 
  filter(itter<=30)

a<-graph_data %>% 
  filter(rute == "HAU_Stord") %>% 
  group_by(Kommune) %>% 
  filter(abs(yrke.bil_sjofor) == min(abs(yrke.bil_sjofor))) %>% 
  ggplot(aes(x = reorder(Kommune,-itter), y = itter,group = Kommune))+
  geom_col(aes(fill = Kommune),color = "black",position = "dodge",show.legend = F)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  labs(y = "hundre nok.",x = "Kommune")+
  labs(title = "HAU vs Stord")

b<-graph_data %>% 
  filter(rute == "HAU_Flesland") %>% 
  group_by(Kommune) %>% 
  filter(abs(yrke.bil_sjofor) == min(abs(yrke.bil_sjofor))) %>% 
  ggplot(aes(x = reorder(Kommune,-itter), y = itter,group = Kommune))+
  geom_col(aes(fill = Kommune),color = "black",position = "dodge",show.legend = F)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  labs(y = "hundre nok.",x = "Kommune")+
  labs(title = "HAU vs Bergen")



c<-graph_data %>% 
  filter(rute == "HAU_Sola") %>% 
  group_by(Kommune) %>% 
  filter(abs(yrke.bil_sjofor) == min(abs(yrke.bil_sjofor))) %>% 
  ggplot(aes(x = reorder(Kommune,-itter), y = itter,group = Kommune))+
  geom_col(aes(fill = Kommune),color = "black",position = "dodge",show.legend = F)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  labs(y = "hundre nok.",x = "Kommune")+
  labs(title = "HAU vs Stavanger")

d<-(a+b+c)

graph_data %>% 
  filter(rute == "HAU_Stord") %>% 
  mutate(line_label = ifelse(.$itter == 30,.$Kommune,NA)) %>% 
  ggplot(aes(x = itter, y = ferie.kollektiv,group = Kommune))+
  geom_line(aes(color = Kommune),lwd = 1,show.legend = F)+
  geom_hline(yintercept = 0,color = "red")+
  geom_label(aes(label = line_label))+
  theme_bw()+
  labs(title = "HAU vs Stord")



graph_data %>% 
  filter(rute == "HAU_Sola") %>% 
  mutate(line_label = ifelse(.$itter == 30,.$Kommune,NA)) %>% 
  ggplot(aes(x = itter, y = ferie.kollektiv,group = Kommune))+
  geom_line(aes(color = Kommune),lwd = 1,show.legend = F)+
  geom_hline(yintercept = 0,color = "red")+
  geom_label(aes(label = line_label))+
  theme_bw()+
  labs(title = "HAU vs Sola")


graph_data %>% 
  filter(rute == "HAU_Flesland") %>% 
  mutate(line_label = ifelse(.$itter == 30,.$Kommune,NA)) %>% 
  ggplot(aes(x = itter, y = ferie.kollektiv,group = Kommune))+
  geom_line(aes(color = Kommune),lwd = 1,show.legend = F)+
  geom_hline(yintercept = 0,color = "red")+
  geom_label(aes(label = line_label))+
  theme_bw()+
  labs(title = "HAU vs Sola")

