# totalt konsument overskudd


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","openxlsx"))


# data --------------------------------------------------------------------

passajerer_2023 <- read.xlsx(xlsxFile = here("data","haugesund_lufthavn_passajerer_data_2023.xlsx")) %>% 
  select(Radetiketter,Innland,Utland,Totalsum) 

besparelse<- read.xlsx(here("results","all_overskudd.xlsx"))

passajerer_segmenter<- data.frame(scenario = c("mean","lufthavn","innovasjonNorge","trajectory"),
                                  ferie = c(375931,	184884,	523839,	449885),
                                  yrke = c(240350,431397,92442,166396))


yrkes_vekt<- read.xlsx(xlsxFile = here("data","auxilary data","arbeidsmarked.xlsx"),sheet = 2) %>% 
  mutate(Kommune = str_split_i(Kommune,pattern = " ",i=2)) %>% 
  select(Kommune,Percent)


befolkning_vekt<- read.xlsx(xlsxFile = here("data","auxilary data","befolkning.xlsx"),sheet = 2) %>% 
  mutate(Kommune = str_split_i(kommune, pattern = " ", i=2)) %>% 
  select(Kommune,percent)
# total_overskudd -----------------------------------------------

###weighted averages by population

besparelse_aggregert<- besparelse %>% 
  filter(scenario=="nullalt") %>% 
  rowwise() %>% 
  mutate(yrkes_besparelse = mean(c(bil.yrke_passajerer_overskudd,bil.yrke_driver_overskudd,kollektiv.yrke_overskudd))*-1,
         ferie_besparelse = mean(c(bil.ferie_driver_overskudd,bil.ferie_passajerer_overskudd,kollektiv.ferie_overskudd))*-1) %>% 
  select(Kommune,rute,yrkes_besparelse,ferie_besparelse) %>% 
  mutate(Kommune = case_when(Kommune == "Tysvaer" ~"Tysvær",.default = Kommune))

utvidet_ferie_segment<- befolkning_vekt %>% 
  mutate(ferie_segment.mean = percent*passajerer_segmenter[[1,2]],
         ferie_segment.lufthavn = percent*passajerer_segmenter[[2,2]],
         ferie_segment.innovasjonNorge = percent*passajerer_segmenter[[3,2]],
         ferie_segment.trajectory = percent*passajerer_segmenter[[4,2]]) %>% 
  mutate(across(ferie_segment.mean:ferie_segment.trajectory,~round(.x))) %>% 
  select(Kommune,contains("ferie"))


utvidet_yrkes_segment<- yrkes_vekt %>% 
  mutate(yrke_segment.mean = Percent*passajerer_segmenter[[1,3]],
         yrke_segment.lufthavn = Percent*passajerer_segmenter[[2,3]],
         yrke_segment.innovasjonNorge = Percent*passajerer_segmenter[[3,3]],
         yrke_segment.trajectory = Percent*passajerer_segmenter[[4,3]]) %>% 
  mutate(across(yrke_segment.mean:yrke_segment.trajectory,~round(.x))) %>% 
  select(Kommune,contains("yrke"))


utvidet_segmenter<- left_join(utvidet_ferie_segment,utvidet_yrkes_segment, by = "Kommune") %>% 
  pivot_longer(cols = ferie_segment.mean:yrke_segment.trajectory,names_to = "var",values_to = "vals") %>% 
  mutate(alternativ = str_split_i(var, "\\.",i=2)) %>% 
  mutate(segment = str_split_i(var,"\\.",i =1)) %>% 
  select(-var) %>% 
  pivot_wider(names_from = segment,values_from = vals)

### aggregation

nytte_matrix<- left_join(utvidet_segmenter,besparelse_aggregert,by = c("Kommune"),relationship = "many-to-many")

total_nytte<- nytte_matrix %>% 
  mutate(ferie_nytte = round(ferie_segment*ferie_besparelse),
         yrkes_nytte = round(yrke_segment*yrkes_besparelse)) %>% 
  mutate(total_nytte = ferie_nytte+yrkes_nytte)


kommune_nytte<- total_nytte %>% 
  select(-contains("besparelse"),-contains("segment")) %>% 
  pivot_wider(names_from = rute,values_from = ferie_nytte:total_nytte,names_sep = ".")

write.xlsx(kommune_nytte,file = here("results","kommune_nytte_scenarier.xlsx"))


# monte carlo simulation --------------------------------------------------


