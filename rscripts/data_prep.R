


#load libraries
library(janitor)
library(tidyverse)
library(here)
library(DBI)
library(RPostgres)

#load functions

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
            (SELECT land_id FROM dit_drags_clean WHERE vessel_number IN
           (SELECT vessel_number FROM dit_drags_clean WHERE docking_date_yy = 2019) AND 
           grid_code IN (SELECT grid_code FROM dit_drags_clean WHERE grid_code IN (512,513,514,515,516,517,518,519,520,521,522,523,524, 525,526,527,530,531,532,533,535,536,537,538,539,540,541,550,551,622,625,628,629,632,633,636,637,640)))")

#close connection to the database
dbDisconnect(con)


#fix species records
#check the species codes
drag_spp <- drag_catches %>% 
  distinct(species_code)
land_spp <- landings_catches %>% 
  distinct(species_code)
spp <- drag_spp %>% 
  full_join(land_spp)

#split all remaining shark records evenly between HNSH and SFSH

drag_catches.1 <- drag_catches%>%
#  filter(!species_code %in% c("DEMF", "DEMLIN"))%>%
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
  pivot_longer(cols = 4:48, 
               names_to = "species_code", 
               values_to = "nominal_mass", 
               values_drop_na = T) %>% 
  mutate(nominal_mass = na_if(nominal_mass, 0)) %>% 
  na.omit()

test <- drag_catches%>%
  distinct(species_code, scientific_name)

drag_catches.2 <- drag_catches.1%>%
  left_join(test)


count(distinct(landings_catches, land_id))
#8542

landings_catches.1 <- landings_catches%>%
  #  filter(!species_code %in% c("DEMF", "DEMLIN"))%>%
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
  pivot_longer(cols = 4:50, 
               names_to = "species_code", 
               values_to = "nominal_mass", 
               values_drop_na = T) %>% 
  mutate(nominal_mass = na_if(nominal_mass, 0)) %>% 
  na.omit() 

test <- landings_catches%>%
  distinct(species_code, scientific_name)

landings_catches.2 <- landings_catches.1%>%
  left_join(test)
  


#connect to the sql database
#con <- dbConnect(RPostgres::Postgres(),
 #                port = 5432,
  #               user = "postgres",
   #              password = "A.inodorus")

#dbSendQuery(con, "create database masters_paper")

#close connection to the database
#dbDisconnect(con)

#connect to the sql database
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "masters_paper",
                 port = 5432,
                 user = "postgres",
                 password = "A.inodorus")

dbWriteTable(con, "drags",drags,overwrite = T )
dbWriteTable(con, "landings",landings,overwrite = T )
dbWriteTable(con, "drag_catches",drag_catches.2,overwrite = T )
dbWriteTable(con, "landings_catches",landings_catches.2,overwrite = T )


#close connection to the database
dbDisconnect(con)
