devtools::install_github("ModelOriented/EIX")
library("EIX")
data(titanic_data)
library(tidyverse)
library(magrittr)
titanic_data %>% dplyr::select(gender,age, class,survived)->smallTitanic

smallTitanic %>% count(class)
smallTitanic %<>% mutate(category=case_when(class=="1st"~1,
                                           class=="2nd"~2,
                                           class=="3rd"~3,
                                           TRUE~NA)) %>% 
  rename(sex=gender, survival=survived) %>% 
  dplyr::select(survival, category,sex, age) %>% 
  mutate(survival=ifelse(survival=="no",0, 1))

smallTitanic %<>% mutate(age=round(age,2))

write_csv(smallTitanic,"mincemeat_updatedByMC_2025.csv") 