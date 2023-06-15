


#load libraries
library(janitor)
library(tidyverse)
library(lubridate)
library(here)
library(DBI)
library(RPostgres)
library(vegan)

#load functions

## Normalise data
normalise <- function(dat, values, id, join) {
  
  test <- dat %>%
    group_by({{id}}) %>%
    summarise(total = sum({{values}}))
  
  test1 <-  dat %>%
    left_join(test, by = {{join}})
  
  test2 <- test1 %>%
    mutate(norm = {{values}} /total)%>%
    select(-total)
}
## pairwise tests for permanova
pairwise.adonis <- function(x,factors, sim.function = 'vegdist', sim.method = 'bray', p.adjust.m ='bonferroni')
{
  library(vegan)
  
  co = combn(unique(as.character(factors)),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  
  
  for(elem in 1:ncol(co)){
    if(sim.function == 'daisy'){
      library(cluster); x1 = daisy(x[factors %in% c(co[1,elem],co[2,elem]),],metric=sim.method)
    } else{x1 = vegdist(x[factors %in% c(co[1,elem],co[2,elem]),],method=sim.method)}
    
    ad = adonis(x1 ~ factors[factors %in% c(co[1,elem],co[2,elem])] );
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  sig = c(rep('',length(p.adjusted)))
  sig[p.adjusted <= 0.05] <-'.'
  sig[p.adjusted <= 0.01] <-'*'
  sig[p.adjusted <= 0.001] <-'**'
  sig[p.adjusted <= 0.0001] <-'***'
  
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted,sig)
  print("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  return(pairw.res)
  
} 


#get data for all trips made by vessels operating in 2019, in the inshore trawl grounds

#connect to the sql database
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "occur_clean",
                 port = 5432,
                 user = "postgres",
                 password = "A.inodorus")


drags <- dbGetQuery(con, "SELECT * FROM dit_drags_clean WHERE vessel_number IN
           (SELECT vessel_number FROM dit_drags_clean WHERE docking_date_yy = 2019) AND 
           grid_code IN (SELECT grid_code FROM dit_drags_clean WHERE grid_code IN (512,513,514,515,516,517,518,519,520,521,522,523,524, 525,526,527,530,531,532,533,535,536,537,538,539,540,541,550,551,622,625,628,629,632,633,636,637,640))")


landings <-  dbGetQuery(con, "SELECT * FROM dit_landings_clean WHERE land_id IN 
            (SELECT land_id FROM dit_drags_clean WHERE vessel_number IN
           (SELECT vessel_number FROM dit_drags_clean WHERE docking_date_yy = 2019) AND 
           grid_code IN (SELECT grid_code FROM dit_drags_clean WHERE grid_code IN (512,513,514,515,516,517,518,519,520,521,522,523,524, 525,526,527,530,531,532,533,535,536,537,538,539,540,541,550,551,622,625,628,629,632,633,636,637,640)))")

drag_catches <-  dbGetQuery(con, "SELECT * FROM dit_drag_catches_clean WHERE drag_id IN 
            (SELECT drag_id FROM dit_drags_clean WHERE vessel_number IN
           (SELECT vessel_number FROM dit_drags_clean WHERE docking_date_yy = 2019) AND 
           grid_code IN (SELECT grid_code FROM dit_drags_clean WHERE grid_code IN (512,513,514,515,516,517,518,519,520,521,522,523,524, 525,526,527,530,531,532,533,535,536,537,538,539,540,541,550,551,622,625,628,629,632,633,636,637,640)))")

landings_catches <-  dbGetQuery(con, "SELECT * FROM dit_landings_catches_clean WHERE land_id IN 
            (SELECT land_id FROM dit_dragsv WHERE vessel_number IN
           (SELECT vessel_number FROM dit_drags_clean WHERE docking_date_yy = 2019) AND 
           grid_code IN (SELECT grid_code FROM dit_drags_clean WHERE grid_code IN (512,513,514,515,516,517,518,519,520,521,522,523,524, 525,526,527,530,531,532,533,535,536,537,538,539,540,541,550,551,622,625,628,629,632,633,636,637,640)))")

#close connection to the database
dbDisconnect(con)


#fix species records

drag_spp <- drag_catches %>% 
  distinct(species_code)
land_spp <- landings_catches %>% 
  distinct(species_code)
spp <- drag_spp %>% 
  full_join(land_spp)


drag_catches <- drag_catches%>%
  filter(!species_code %in% c("DEMF", "DEMLIN"))%>%
  group_by(drag_id,
           vessel_number,
           company_number,
           docking_date_yy, 
           species_code)%>%
  summarise(nominal_mass= sum(nominal_mass, na.rm = T))%>%
  ungroup()%>%
  pivot_wider(names_from = species_code, values_from = nominal_mass)%>%
  mutate_all(~replace_na(.,0))%>%
  mutate(HNSH = (HNSH+SHRK)/2, HNSH)%>%
  mutate(SFSH = (SFSH+SHRK)/2, SFSH)%>%
  select(-c(SHRK)) %>% 
  pivot_longer(cols = 5:47, 
               names_to = "species_code", 
               values_to = "nominal_mass", 
               values_drop_na = T) %>% 
  mutate(nominal_mass = na_if(nominal_mass, 0)) %>% 
  na.omit()


shark <- landings_catches%>%
  filter(species_code %in% c("SFSH", "HNSH","SHRK"))%>%
  count(vessel_number, species_code)%>%
  count(vessel_number)%>%
  mutate(shark = if_else(n == 2, "a", "b"))%>%
  select(-n)

landings_catches <- landings_catches%>%
  filter(!(land_id == "RSA, 50, 262, 1/7/1992 0:0" & 
             species_code == "SHRK"))%>%
  filter(!species_code %in% "DEMF")%>%
  left_join(shark)%>%
  group_by(land_id,vessel_number,company_number, docking_date_yy, species_code, shark)%>%
  summarise(nominal_mass= sum(nominal_mass, na.rm = T))%>%
  ungroup()%>%
  pivot_wider(names_from = species_code, values_from = nominal_mass)%>%
  mutate_all(~replace_na(.,0))%>%
  mutate(SFSH = if_else(shark == "a", SHRK+SFSH, SFSH))%>%
  mutate(HNSH = if_else(shark != "a", HNSH+SHRK/2, HNSH))%>%
  mutate(SFSH = if_else(shark != "a", SFSH+SHRK/2, SFSH))%>%
  select(-c(SHRK, shark))%>%
  pivot_longer(cols = 6:50, 
               names_to = "species_code", 
               values_to = "nominal_mass", 
               values_drop_na = T) %>% 
  mutate(na_if(nominal_mass, 0))




