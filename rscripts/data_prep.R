


#load libraries
library(janitor)
library(tidyverse)
library(here)
library(DBI)
library(RPostgres)

##get data

#connect to the sql database
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "occur_clean",
                 port = 5432,
                 user = "postgres",
                 password = "A.inodorus")


drags <- dbGetQuery(con, "SELECT * FROM dit_drags_clean")

landings <-  dbGetQuery(con, "SELECT * FROM dit_landings_clean")

drag_catches <-  dbGetQuery(con, "SELECT * FROM dit_drag_catches_clean")

landings_catches <-  dbGetQuery(con, "SELECT * FROM dit_landings_catches_clean")

#close connection to the database
dbDisconnect(con)

grid <- read.csv("data/ComGrids.csv")

### filter records
###--------------------------------------------------------------------------

##remove species codes that are not species
##only take years fater 2000
##only take vessel that fished in more than 50% years
##only take gridcells in the inshore trawl grounds

vessels <- landings %>% 
  filter(docking_date_yy>2000) %>% 
  distinct(docking_date_yy, vessel_number) %>% 
  count(vessel_number) %>% 
  filter(n>=10) %>% 
  select(-n)

gridcells <- grid %>% 
  filter(inshore_area==1) %>% 
  select(code) %>% 
  rename(grid_code = code)

## filter datasets

drags.1 <- drags%>%
  filter(docking_date_yy>2000) %>%  
  right_join(vessels) %>% 
  inner_join(gridcells)

filter_drag <- drags.1 %>% 
  distinct(drag_id)

filter_land <- drags.1 %>% 
  distinct(land_id)
  
drag_catches.1 <- drag_catches%>%
  filter(!species_code %in% c("DEMF", "DEMLIN"))%>%
  inner_join(filter_drag)

landings.1 <- landings %>% 
  inner_join(filter_land)

landings_catches.1 <- landings_catches%>%
  filter(!species_code %in% c("DEMF", "DEMLIN"))%>%
  inner_join(filter_land)

##split all remaining shark records evenly between HNSH and SFSH

drag_catches.2 <- drag_catches%>% 
  group_by(drag_id,
           vessel_number,
           docking_date_yy, 
           species_code)%>%
  summarise(nominal_mass= sum(nominal_mass, na.rm = T))%>%
  ungroup()%>%
  pivot_wider(names_from = species_code, values_from = nominal_mass)%>%
  mutate_all(~replace_na(.,0))%>%
  mutate(HNSH = HNSH+(SHRK/2))%>%
  mutate(SFSH = SFSH+(SHRK/2))%>%
  select(-c(SHRK)) %>% 
  pivot_longer(cols = 4:50, 
               names_to = "species_code", 
               values_to = "nominal_mass", 
               values_drop_na = T) %>% 
  mutate(nominal_mass = na_if(nominal_mass, 0)) %>% 
  na.omit()

test <- drag_catches%>%
  distinct(species_code, scientific_name)

drag_catches.3 <- drag_catches.2%>%
  left_join(test)


landings_catches.2 <- landings_catches.1%>%
  group_by(land_id,
           vessel_number, 
           docking_date_yy, 
           species_code)%>%
  summarise(nominal_mass= sum(nominal_mass, na.rm = T))%>%
  ungroup()%>%
  pivot_wider(names_from = species_code, values_from = nominal_mass)%>%
  mutate_all(~replace_na(.,0))%>%
  mutate(HNSH = HNSH+(SHRK/2))%>%
  mutate(SFSH = SFSH+(SHRK/2))%>%
  select(-c(SHRK))%>%
  pivot_longer(cols = 4:49, 
               names_to = "species_code", 
               values_to = "nominal_mass", 
               values_drop_na = T) %>% 
  mutate(nominal_mass = na_if(nominal_mass, 0)) %>% 
  na.omit() 

test <- landings_catches%>%
  distinct(species_code, scientific_name)

landings_catches.3 <- landings_catches.2%>%
  left_join(test)
  


#connect to the sql database
con <- dbConnect(RPostgres::Postgres(),
                 port = 5432,
                 user = "postgres",
                 password = "A.inodorus")

dbSendQuery(con, "create database masters_paper")

#close connection to the database
dbDisconnect(con)

#connect to the sql database
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "masters_paper",
                 port = 5432,
                 user = "postgres",
                 password = "A.inodorus")

dbWriteTable(con, "drags",drags.1,overwrite = T )
dbWriteTable(con, "landings",landings.1,overwrite = T )
dbWriteTable(con, "drag_catches",drag_catches.3,overwrite = T )
dbWriteTable(con, "landings_catches",landings_catches.3,overwrite = T )


#close connection to the database
dbDisconnect(con)
