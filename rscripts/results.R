


#load libraries
library(janitor)
library(tidyverse)
library(lubridate)
library(here)
library(DBI)
library(RPostgres)


#get data for all trips made by vessels operating in 2019, in the inshore trawl grounds



#connect to the sql database
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "FOClean",
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












