# totalt konsument overskudd


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","openxlsx"))


# data --------------------------------------------------------------------

passajerer_2023 <- read.xlsx(xlsxFile = here("data","haugesund_lufthavn_passajerer_data_2023.xlsx")) %>% 
  select(Radetiketter,Innland,Utland,Totalsum) 


lufthavn_passajerer_koef<- c("yrke" = .7,"ferie" = .3)

reisevane_passajerer_koef<- c("yrke_innland" = .41,"ferie_innland" = .59,"yrke_utland"=.24,"ferie_utland"=.76)

andel_monad<- passajerer_2023 %>% 
  filter(Radetiketter != "Totalsum") %>% 
  mutate(innland_andel_per_monad = round(Innland/sum(Innland),2),
         utland_andel_per_monad = round(Utland/sum(Utland),2),
         total_andel_per_monad = round(Totalsum/sum(Totalsum),2))


## andel monad is wrong

gjennomsnitt_besparelse<- read.xlsx(here("results","all_overskudd.xlsx")) %>% 
  group_by(rute,scenario) %>% 
  summarise(across(bil.yrke_driver_overskudd:kollektiv.ferie_overskudd,~mean(.x))) %>% 
  ungroup() %>% 
  pivot_longer(cols = bil.yrke_driver_overskudd:kollektiv.ferie_overskudd,names_to = "mode",values_to = "overskudd") %>% 
  mutate(hensikt = case_when(grepl(x=mode,pattern = "yrke")~"yrke",
                             grepl(x=mode,pattern="ferie")~"ferie",.default = "error")) %>% 
  group_by(rute,scenario,hensikt) %>% 
  summarise(overskudd = mean(overskudd)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(hensikt,scenario),names_from = rute,values_from = overskudd)
  

# total overskudd ---------------------------------------------------------


lufthavn_passajerer_segment<- data.frame(antall = round(621302*lufthavn_passajerer_koef)) %>% 
  rownames_to_column(var = "hensikt") 

gjennomsnittlig_overskudd <- gjennomsnitt_besparelse %>% 
  left_join(.,lufthavn_passajerer_segment,by = "hensikt") %>% 
  mutate(hlh_flesland_overskudd = hlh_flesland*antall,
         hlh_sola_overskudd = hlh_sola*antall,
         hlh_stord_overskudd = hlh_stord*antall) %>% 
  mutate(across(hlh_flesland_overskudd:hlh_stord_overskudd,~round(.x))) %>% 
  mutate(source = "lufthavndriftAS")

reisevane_segmenter<- passajerer_2023 %>% 
  filter(Radetiketter == "Totalsum") %>% 
  mutate(innland_yrke = Innland*reisevane_passajerer_koef[["yrke_innland"]],
         innland_ferie = Innland*reisevane_passajerer_koef[["ferie_innland"]],
         utland_yrke = Utland*reisevane_passajerer_koef[["yrke_utland"]],
         utland_ferie = Utland*reisevane_passajerer_koef[["ferie_utland"]]) %>% 
  pivot_longer(cols = innland_yrke:utland_ferie,names_to = "type",values_to = "antall") %>% 
  mutate(hensikt = str_split_i(type,"_",i=2)) %>% 
  group_by(hensikt) %>% 
  summarise(antall = sum(antall)) %>% 
  ungroup() 


gjennomsnittlig_overskudd2<-gjennomsnitt_besparelse %>% 
  left_join(.,reisevane_segmenter,by = "hensikt") %>% 
  mutate(hlh_flesland_overskudd = hlh_flesland*antall,
         hlh_sola_overskudd = hlh_sola*antall,
         hlh_stord_overskudd = hlh_stord*antall) %>% 
  mutate(across(hlh_flesland_overskudd:hlh_stord_overskudd,~round(.x))) %>% 
  mutate(source = "ReisevaneSU")

total_overskudd<- rbind(gjennomsnittlig_overskudd,gjennomsnittlig_overskudd2) %>% 
  mutate(total_overskudd = (hlh_flesland_overskudd+hlh_sola_overskudd+hlh_stord_overskudd)) %>% 
  mutate(total_overskud_mill_kr = total_overskudd/10^6)

if(!file.exists(here("results","total_overskudd_detaljert.xlsx"))){
  write.xlsx(total_overskudd,here("results","total_overskudd_detaljert.xlsx"))
}