#Klima utslipp overskudd


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","openxlsx"))

##kjøring km pris


# data --------------------------------------------------------------------

kjoring_data<- read.xlsx(xlsxFile = here("data","overskudd data","lufthavn_reisetid.xlsx"),sheet = 2) %>% 
  group_by(Kommune, Lufthavn) %>% 
  summarise(across(kjoretid:ferjefrie39_alt_bompenger,~round(mean(.x)))) %>% 
  pivot_wider(id_cols = Kommune, names_from = Lufthavn, values_from = kjoretid:ferjefrie39_alt_bompenger)


km_besparelse<- kjoring_data %>% 
  select(Kommune,contains("km_")) %>% 
  mutate(km_besparelse.hlh_flesland = km_HLH-km_Flesland,
         km_besparelse.hlh_sola = km_HLH-km_Sola,
         km_besparelse.hlh_stord = km_HLH-km_Stord) %>% 
  select(Kommune,contains("besparelse"))

#write.xlsx(km_besparelse,file = here("data","overskudd data","km_besparelse.xlsx"))


marginal_ekstern_koef<- c("bil" = 1.12+0.08,
                       "kollektiv"= 0.34+0.08) 

margina_kostnad_besparelse<- km_besparelse %>% 
  mutate(eksternK.bil.hlh_flesland = km_besparelse.hlh_flesland*marginal_ekstern_koef["bil"],
         eksternK.bil.hlh_sola = km_besparelse.hlh_sola*marginal_ekstern_koef["bil"],
         eksternK.bil.hlh_stord = km_besparelse.hlh_stord*marginal_ekstern_koef["bil"]) %>% 
  mutate(eksternK.kollektiv.hlh_flesland = km_besparelse.hlh_flesland*marginal_ekstern_koef["kollektiv"],
         eksternK.kollektiv.hlh_sola = km_besparelse.hlh_sola*marginal_ekstern_koef["kollektiv"],
         eksternK.kollektiv.hlh_stord = km_besparelse.hlh_stord*marginal_ekstern_koef["kollektiv"]) %>% 
  select(Kommune,contains("eksternK")) %>% 
  pivot_longer(cols = contains("eksternK"),names_to = "vars",values_to = "vals") %>% 
  mutate(rute = str_split_i(vars,pattern = "\\.",i=3)) %>% 
  mutate(vars = gsub(".hlh_flesland|.hlh_sola|.hlh_stord","",vars)) %>% 
  pivot_wider(names_from = vars,values_from = vals)

write.xlsx(margina_kostnad_besparelse,here("data","overskudd data","ekstern_kostnad_besparelse"))


