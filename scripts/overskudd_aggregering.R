# konsument overskudd aggregering og datafil deling


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","openxlsx","here"))

data<- readRDS(file = here("results","all_overskudd.RDS"))

Adata<- data %>% 
  mutate(bil.yrke = round((bil.yrke_driver_overskudd+bil.yrke_passajerer_overskudd)/2,0)) %>% 
  mutate(bil.ferie = round((bil.ferie_driver_overskudd+bil.ferie_passajerer_overskudd)/2,0)) %>% 
  select(-contains("driver"),-contains("passajerer"))

# split -------------------------------------------------------------------

vars<- colnames(Adata)[4:length(colnames(Adata))]

scenarier<- unique(Adata$scenario)


for (i in scenarier) {
  subdata<- Adata %>% 
    filter(scenario == "nullalt")
  
  if(!dir.exists(here("results","scenario_results",i))){
    dir.create(here("results","scenario_results",i))
  }
  
  for (j in vars) {
    savedata<- subdata %>% 
      select(Kommune,rute,all_of(j))
    
    write.xlsx(savedata,file = here("results","scenario_results",i,paste0("nullalt_",j,".xlsx")))
  }
}
