# ---
# title: "Clean the demersal trawl catch database and put it into the clean database"
# author: "Natasha Besseling/Jock Currie"
# date: "2023-04-26"
# ---

##load libraries

library(DBI)
library(tidyverse)
library(createDB)
library(taxize)
library(sf)


## connect to the sql database
sql_tbl_ext("masters_paper", "dit_")

#Fix the target spp code spelling errors in the drags table
#SOLE and HMKC were errors

drags.1 <- dit_drags %>%
  mutate(target_species_code =
           ifelse(target_species_code =="SOLE","ECSOLE", target_species_code))%>%
  mutate(target_species_code =
           ifelse(target_species_code =="HMKC","HMCK", target_species_code))


#Fix the dates in the drags table:
#The main problem was that some end dates were wrong. The trawl would end in the next day (go over midnight) but the end date would be the same as the start. Therefore it would look like the trawl started at 11pm but ended at 2am the same day (i.e in the past) and there had a negative duration.
#To fix this I took all trawls that had negative trawl durations associated with them and moved the end date forward one day.

drags.1 <- drags.1 %>%
  separate(drag_ID, c(NA,NA,NA,NA,"start_date"), sep = ",", remove = F )%>% #get the start date and time of the trawl in one column
  unite(end_date, end_date_dd, end_date_mm, end_date_yy, sep = "/", remove = F) %>% #get the end date of the trawl in one column
  unite(end_time, end_date_hh, end_date_mi, sep = ":", remove = F ) %>%  #get the end time in one column
  unite(end_date, end_date, end_time, sep = " ")%>% #combine the end date and time of the trawl into one column
  mutate(start_date = as.POSIXct(start_date, format = "%d/%m/%Y %H:%M", tz = "GMT"))%>% #make the start date a datetime object
  mutate(end_date = as.POSIXct(end_date, format = "%d/%m/%Y %H:%M", tz = "GMT"))%>% # make the end date a datetime object
  mutate(trawl_length_hh = difftime(end_date, start_date, units = "hours")) %>% #get length of trawl
  mutate(trawl_length_hh = as.numeric(trawl_length_hh))%>% #make trawl length a numeric value
  mutate(end_date_dd_fix = ifelse(trawl_length_hh <0,
                                  ifelse(end_date_mm %in% c(4,6,9,11),
                                         ifelse(end_date_dd== 30, 1 , end_date_dd+1),
                                         ifelse(end_date_mm %in% c(1,3,5,7,8,10,12),
                                                ifelse(end_date_dd ==31, 1, end_date_dd+1),
                                                ifelse(end_date_mm ==2,
                                                       ifelse(end_date_yy %in% c(1992, 1996),
                                                              ifelse(end_date_dd==29, 1 , end_date_dd+1),
                                                              ifelse(end_date_dd==28, 1, end_date_dd+1)),
                                                       end_date_dd+1))),end_date_dd)) %>% #Then if the trawl length is negative then look what month it is. If its April, June, sep, or Nov, then look if the day is the 30th. If it is, make the day 1, otherwise make the day one more than it was. If its one of the other months (except Feb) then look if the day is the 31st. If it is make the day 1, otherwise make the day one more than it was. Then look if the month is february look if the year is 1992 or 1996. If it is then if the day is 29 then make the day 1, and if not make it one more than it was. If its in any other year look to see if its the 28th, and if it is make it 1, and if not make it one more than it was.
  mutate(end_date_mm_fix = ifelse(trawl_length_hh <0,
                                  ifelse(end_date_dd_fix == 1,
                                         ifelse(end_date_mm ==12, 1, end_date_mm +1),
                                         end_date_mm),
                                  end_date_mm)) %>% #if the trawl length is negative then look to see if the month is dec, if it is then make it 1, otherwise make it one more than it was.
  mutate(end_date_yy_fix = ifelse(trawl_length_hh <0,
                                  ifelse(trawl_length_hh < -1000,
                                         end_date_yy +1,
                                         ifelse(end_date_dd_fix ==1 & end_date_mm_fix == 1,
                                                end_date_yy +1,
                                                end_date_yy)),
                                  end_date_yy))%>% #if trawl length is negative, look to see if its less than -1000. If it is increase the year by 1. If not then look to see if the fixed day and month are 1 (Jan 1), and if so increase the year by 1 as well.
  unite(end_date, end_date_dd_fix, end_date_mm_fix, end_date_yy_fix, sep = "/", remove = F)%>% #Combine the fixed end dates into one column
  unite(end_time, end_date_hh, end_date_mi, sep = ":", remove = F ) %>% #combine the end times into one column
  unite(end_date, end_date, end_time, sep = " ")%>% #combine end date and time into one column
  mutate(end_date = as.POSIXct(end_date, format = "%d/%m/%Y %H:%M", tz = "GMT"))%>% #make the fixedend date into a datetime object
  mutate(trawl_length_hh = difftime(end_date, start_date, units = "hours")) %>%           #get length of trawl
  mutate(trawl_length_hh = as.numeric(trawl_length_hh))


#fix the dates in the landings table


landings.1 <- dit_landings%>%
  separate(land_ID, c(NA,NA,NA,"docking_date"), sep = ",", remove = F )%>%
  unite(sailing_date, sailing_date_dd, sailing_date_mm, sailing_date_yy, sep = "/", remove = F) %>%
  unite(sailing_time, sailing_date_hh, sailing_date_mi, sep = ":", remove = F ) %>%
  unite(sailing_date, sailing_date, sailing_time, sep = " ") %>%
  mutate(docking_date = as.POSIXct(docking_date, format = "%d/%m/%Y %H:%M",  tz = "GMT")) %>%
  mutate(sailing_date = as.POSIXct(sailing_date, format = "%d/%m/%Y %H:%M", tz = "GMT"))%>%
  mutate(trip_length_dd = ceiling(difftime(docking_date, sailing_date, units = "days"))) %>%#get length of trip
  mutate(trip_length_dd = as.numeric(trip_length_dd))%>%
  mutate(sailing_date_yy = ifelse(trip_length_dd >300, docking_date_yy,sailing_date_yy))%>%# when the trip was over 300 days I made the start year the same as the end year
  unite(sailing_date, sailing_date_dd, sailing_date_mm, sailing_date_yy, sep = "/", remove = F) %>%
  unite(sailing_time, sailing_date_hh, sailing_date_mi, sep = ":", remove = F ) %>%
  unite(sailing_date, sailing_date, sailing_time, sep = " ") %>%
  mutate(sailing_date = as.POSIXct(sailing_date, format = "%d/%m/%Y %H:%M", tz = "GMT"))

landings.1[landings.1$land_ID=="RSA, 50, 262, 2/1/1991 0:0",
           "docking_date"] <- "1990-12-21 00:00:00"
landings.1[landings.1$land_ID=="RSA, 14, 268, 17/12/1993 0:0",
           "docking_date"] <- "1993-11-17 00:00:00"
landings.1[landings.1$land_ID=="RSA, 43, 239, 24/9/1991 7:0",
           "sailing_date"] <- "1991-09-21 18:30:00"

landings.1[landings.1$land_ID=="RSA, 51, 252, 22/7/1991 0:0",
           "docking_date"] <- "1991-05-21 18:30:00"

landings.1[landings.1$land_ID=="RSA, 68, 264, 26/7/1992 10:0",
           "sailing_date"] <- "1992-07-19 00:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 202, 15/2/1994 0:0",
           "docking_date"] <- "1994-01-15 00:00:00"

landings.1[landings.1$land_ID=="RSA, 9, 244, 21/9/1999 10:0",
           "docking_date"] <- "1999-05-21 07:00:00"

landings.1[landings.1$land_ID=="RSA, 50, 262, 26/10/1997 10:0",
           "docking_date"] <- "1997-09-26 10:00:00"

landings.1[landings.1$land_ID=="RSA, 59, 278, 8/12/1999 10:0",
           "sailing_date"] <- "1999-12-02 11:00:00"

landings.1[landings.1$land_ID=="RSA, 68, 264, 17/5/1999 0:0",
           "sailing_date"] <- "1999-05-15 01:05:00"

landings.1[landings.1$land_ID=="RSA, 9, 93, 8/2/2000 10:0",
           "sailing_date"] <- "2000-01-31 11:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 253, 13/3/1996 17:40",
           "docking_date"] <- "1996-02-13 17:40:00"

landings.1[landings.1$land_ID=="RSA, 51, 253, 25/9/1999 6:30",
           "docking_date"] <- "1999-09-05 06:30:00"

landings.1[landings.1$land_ID=="RSA, 59, 304, 21/10/2002 11:30",
           "sailing_date"] <- "2002-10-16 10:30:00"

landings.1[landings.1$land_ID=="RSA, 51, 294, 3/1/2000 7:0",
           "sailing_date"] <- "1999-12-28 10:20:00"

landings.1[landings.1$land_ID=="RSA, 59, 304, 14/11/2000 10:0",
           "sailing_date"] <- "2000-11-08 13:00:00"

landings.1[landings.1$land_ID=="RSA, 58, 217, 14/9/2000 10:0",
           "sailing_date"] <- "2000-09-12 04:00:00"

landings.1[landings.1$land_ID=="RSA, 43, 264, 3/2/2003 20:0",
           "sailing_date"] <- "2003-02-01 00:50:00"

landings.1[landings.1$land_ID=="RSA, 9, 284, 1/3/2001 7:1",
           "docking_date"] <- "2001-01-29 07:01:00"

landings.1[landings.1$land_ID=="RSA, 9, 282, 24/11/2000 9:20",
           "sailing_date"] <- "2000-11-21 15:10:00"

landings.1[landings.1$land_ID=="RSA, 51, 252, 26/8/2000 14:0",
           "sailing_date"] <- "2000-08-21 18:05:00"

landings.1[landings.1$land_ID=="RSA, 51, 296, 27/3/2003 7:0",
           "sailing_date"] <- "2003-03-21 03:00:00"

landings.1[landings.1$land_ID=="RSA, 59, 278, 3/9/2001 7:0",
           "sailing_date"] <- "2001-09-01 23:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 308, 22/7/2000 10:0",
           "sailing_date"] <- "2000-07-12 10:00:00"

landings.1[landings.1$land_ID=="RSA, 9, 107, 25/7/2001 7:0",
           "sailing_date"] <- "2001-07-19 06:30:00"

landings.1[landings.1$land_ID=="RSA, 9, 230, 6/9/2001 6:0",
           "sailing_date"] <- "2001-09-01 14:30:00"

landings.1[landings.1$land_ID=="RSA, 50, 237, 9/11/2000 6:0",
           "sailing_date"] <- "2000-11-03 13:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 308, 12/2/2003 7:0",
           "sailing_date"] <- "2003-02-05 17:00:00"

landings.1[landings.1$land_ID=="RSA, 124, 147, 18/10/2001 7:0",
           "sailing_date"] <- "2001-10-13 15:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 253, 27/11/2000 0:0",
           "sailing_date"] <- "2000-11-21 17:45:00"

landings.1[landings.1$land_ID=="RSA, 51, 253, 7/6/2001 7:0",
           "sailing_date"] <- "2001-06-01 19:30:00"

landings.1[landings.1$land_ID=="RSA, 145, 243, 30/7/2003 6:0",
           "sailing_date"] <- "2003-07-23 08:00:00"

landings.1[landings.1$land_ID=="RSA, 149, 93, 17/7/2003 7:0",
           "sailing_date"] <- "2003-07-11 18:00:00"

landings.1[landings.1$land_ID=="RSA, 9, 96, 6/9/2002 6:0",
           "sailing_date"] <- "2002-08-31 09:30:00"

landings.1[landings.1$land_ID=="RSA, 32, 310, 10/4/2001 17:0",
           "sailing_date"] <- "2001-04-02 07:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 253, 30/5/2000 14:0",
           "sailing_date"] <- "2000-05-21 17:30:00"

landings.1[landings.1$land_ID=="RSA, 9, 230, 15/3/2000 6:0",
           "sailing_date"] <- "2000-03-08 14:25:00"

landings.1[landings.1$land_ID=="RSA, 9, 230, 23/3/2000 6:0",
           "sailing_date"] <- "2000-03-17 09:30:00"

landings.1[landings.1$land_ID=="RSA, 32, 310, 20/7/2003 7:0",
           "sailing_date"] <- "2003-07-11 13:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 252, 22/4/2000 19:40",
           "sailing_date"] <- "2000-04-18 21:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 252, 7/11/2001 12:32",
           "sailing_date"] <- "2001-11-01 15:28:00"

landings.1[landings.1$land_ID=="RSA, 24, 303, 29/10/2001 7:0",
           "sailing_date"] <- "2001-10-22 07:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 308, 29/4/2003 11:0",
           "sailing_date"] <- "2003-04-22 19:15:00"

landings.1[landings.1$land_ID=="RSA, 59, 278, 2/12/2002 7:0",
           "sailing_date"] <- "2002-11-29 23:00:00"

landings.1[landings.1$land_ID=="RSA, 9, 93, 28/5/2001 7:0",
           "sailing_date"] <- "2001-05-20 10:00:00"

landings.1[landings.1$land_ID=="RSA, 32, 117, 18/3/2003 7:0",
           "sailing_date"] <- "2003-03-10 23:00:00"

landings.1[landings.1$land_ID=="RSA, 14, 205, 28/11/2000 10:0",
           "sailing_date"] <- "2000-11-20 06:00:00"

landings.1[landings.1$land_ID=="RSA, 9, 96, 6/7/2001 6:0",
           "sailing_date"] <- "2001-06-29 10:00:00"

landings.1[landings.1$land_ID=="RSA, 9, 284, 5/2/2001 6:30",
           "sailing_date"] <- "2001-01-30 11:50:00"

landings.1[landings.1$land_ID=="RSA, 24, 303, 9/9/2001 19:0",
           "sailing_date"] <- "2001-09-01 11:10:00"

landings.1[landings.1$land_ID=="RSA, 32, 117, 26/7/2002 13:0",
           "sailing_date"] <- "2002-07-17 13:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 296, 16/4/2000 10:0",
           "sailing_date"] <- "2000-04-07 12:00:00"

landings.1[landings.1$land_ID=="RSA, 145, 243, 9/4/2003 6:0",
           "sailing_date"] <- "2003-03-31 07:00:00"

landings.1[landings.1$land_ID=="RSA, 145, 277, 9/4/2003 6:0",
           "sailing_date"] <- "2003-04-1 07:00:00"

landings.1[landings.1$land_ID=="RSA, 59, 278, 9/3/1998 7:0",
           "docking_date"] <- "1998-03-09 07:00:00"

landings.1[landings.1$land_ID=="RSA, 32, 117, 20/11/2003 7:0",
           "sailing_date"] <- "2003-11-11 23:30:00"

landings.1[landings.1$land_ID=="RSA, 59, 290, 7/2/2000 6:0",
           "sailing_date"] <- "2000-01-31 08:00:00"

landings.1[landings.1$land_ID=="RSA, 125, 225, 13/2/2001 10:0",
           "sailing_date"] <- "2001-02-05 10:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 296, 27/4/2000 10:0",
           "sailing_date"] <- "2000-04-20 10:45:00"

landings.1[landings.1$land_ID=="RSA, 124, 147, 12/10/2001 7:0",
           "sailing_date"] <- "2001-10-05 06:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 253, 24/2/2003 7:0",
           "sailing_date"] <- "2003-02-17 18:05:00"

landings.1[landings.1$land_ID=="RSA, 51, 296, 7/1/2003 7:0",
           "sailing_date"] <- "2003-01-01 18:00:00"

landings.1[landings.1$land_ID=="RSA, 14, 293, 26/9/2001 7:0",
           "sailing_date"] <- "2001-09-17 13:00:00"

landings.1[landings.1$land_ID=="RSA, 124, 147, 30/9/2002 15:0",
           "sailing_date"] <- "2002-09-23 06:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 308, 8/2/2001 7:0",
           "sailing_date"] <- "2001-01-30 19:00:00"

landings.1[landings.1$land_ID=="RSA, 9, 96, 2/11/2001 7:0",
           "sailing_date"] <- "2001-10-27 10:00:00"

landings.1[landings.1$land_ID=="RSA, 124, 147, 1/11/2002 7:0",
           "sailing_date"] <- "2002-10-27 23:00:00"

landings.1[landings.1$land_ID=="RSA, 51, 131, 23/2/1994 0:0",
           "sailing_date"] <- "1994-02-11 10:45:00"

landings.1[landings.1$land_ID=="RSA, 59, 278, 9/3/1998 7:0",
           "sailing_date"] <- "1998-03-02 23:00:00"

landings.1[landings.1$land_ID=="RSA, 14, 243, 21/11/2000 10:0",
           "sailing_date"] <- "2000-11-13 10:00:00"

landings.1[landings.1$land_ID=="RSA, 14, 243, 24/3/2003 7:0",
           "sailing_date"] <- "2003-03-17 07:00:00"

landings.1[landings.1$land_ID=="RSA, 32, 117, 21/9/2000 18:0",
           "docking_date"] <- "2000-09-12 18:00:00"


landings.1 <- landings.1%>%
  mutate(sailing_date = as.POSIXct(sailing_date, format = "%d/%m/%Y %H:%M", tz = "GMT"))%>%
  mutate(docking_date = as.POSIXct(docking_date, format = "%d/%m/%Y %H:%M",  tz = "GMT"))


#fix the number of drags made in the landings table


trawls <- drags.1 %>%
  select(land_ID, drag_ID) %>%
  group_by(land_ID) %>%
  count()%>%
  rename(number_of_trawls = n)

landings.1 <- landings.1 %>%
  left_join(trawls, by ="land_ID")



#To convert cleaned mass to nominal mass for the trips. The problem is the rising factor to use to change a species cleaned mass back into nominal mass has changed over the years. Therefore you need to match the correct one to the correct date.

#1. Get dates for conversions (I manually made convert.1 in excel from the REF_category_codes)

#2. use these to calculate the nominal mass for each spp code:

#3. create an inerval durting which each conversion factor is valid

#4. add the interval and conversion factor columns by spp code and category code to the landings.catches table.
##This will make duplicates as one spp code and category can have multiple conversion factors.

#5. check where the trip date falls within the interval, only keep records where the trip date is within the       interval

#6. multiply the cleaned mass by the rising factor


#convert.1 <- read.csv(here("data/convert.1.csv"))

#date_valid_from <- convert.1$date_valid_from
#date_valid_until <- convert.1$date_valid_until

#dput(as.character(date_valid_from))

date_valid_from <- c("1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "3/1/2003", "1/1/1978", "9/3/2002", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "5/1/2009", "1/1/1978",
                     "5/1/2009", "1/1/1978", "5/1/2009", "1/1/1978", "5/1/2009", "1/1/1978",
                     "5/10/2009", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "3/1/2003", "5/1/2009", "1/1/1978",
                     "3/1/2003", "5/1/2009", "1/1/1978", "3/1/2003", "5/1/2009", "1/1/1978",
                     "3/1/2003", "5/1/2009", "1/1/1978", "3/1/2003", "5/1/2009", "1/1/1978",
                     "1/1/1978", "3/1/2003", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "3/1/2003", "5/1/2009", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "5/1/2009", "1/1/1978", "1/1/1978", "1/1/1978", "3/1/2003", "5/1/2009",
                     "1/1/1978", "3/1/2003", "5/1/2009", "1/1/1978", "3/1/2003", "5/1/2009",
                     "1/1/1978", "3/1/2003", "5/1/2009", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1970", "1/1/1970", "1/1/1970", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "2/1/2008", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978", "1/1/1978",
                     "1/1/1978", "1/1/1978", "1/1/1978")

#dput(as.character(date_valid_until))

date_valid_until <- c("1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "3/1/2003",
                      "1/1/2020", "9/3/2002", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "5/1/2009", "1/1/2020", "5/1/2009",
                      "1/1/2020", "5/1/2009", "1/1/2020", "5/1/2009", "1/1/2020", "5/10/2009",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "3/1/2003", "5/1/2009", "1/1/2020", "3/1/2003", "5/1/2009",
                      "1/1/2020", "3/1/2003", "5/1/2009", "1/1/2020", "3/1/2003", "5/1/2009",
                      "1/1/2020", "3/1/2003", "5/1/2009", "1/1/2020", "1/1/2020", "3/1/2003",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "3/1/2003",
                      "5/1/2009", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "5/1/2009", "1/1/2020",
                      "1/1/2020", "1/1/2020", "3/1/2003", "5/1/2009", "1/1/2020", "3/1/2003",
                      "5/1/2009", "1/1/2020", "3/1/2003", "5/1/2009", "1/1/2020", "3/1/2003",
                      "5/1/2009", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "8/1/2004", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020", "1/1/2020",
                      "1/1/2020", "1/1/2020")


convert.1 <- dit_REF_dbo_product_category_factors %>%
  select(-date_valid_from) %>%
  bind_cols(date_valid_until = date_valid_until) %>%
  bind_cols(date_valid_from = date_valid_from)


convert.1 <- convert.1 %>%
  mutate(date_valid_from =
           as.POSIXct(date_valid_from, format = "%m/%d/%Y", tz = "GMT"))%>%
  mutate(date_valid_until =
           as.POSIXct(date_valid_until, format = "%m/%d/%Y", tz = "GMT"))%>%
  mutate(interval = interval(start= date_valid_from, end = date_valid_until))%>%
  select(-c(date_valid_until, date_valid_from))

##the warning is fine, we match many-to-many, and then only take out the ones that match our criteria.
landings_catches.1 <- dit_landings_catches%>%
  left_join(convert.1, by = c("species_code", "category_code"))%>%
  separate(land_ID, c(NA,NA,NA,"docking_date"), sep = ",", remove = F )%>%
  mutate(docking_date =
           as.POSIXct(docking_date, format = "%d/%m/%Y", tz = "GMT"))%>%
  mutate(match = if_else(docking_date %within% interval, "true", "false"))%>%
  filter(match == "true")%>%
  mutate(nominal_mass = landed_mass*raising_factor) %>%
  select(-c(match, raising_factor, interval, docking_date))


#fix the id column name in drags no effort table

drags_no_effort.1 <- dit_drags_no_effort %>%
  rename(land_ID = land_id)

#fix the mistakes between trip and trawl catches.


drag.sub <- dit_drag_catches %>%
  inner_join(select(drags.1,
                    land_ID,
                    drag_ID), by = "drag_ID")

drag_catches.1 <- dit_drag_catches %>%
  inner_join(select(drags.1,
                    land_ID,
                    drag_ID), by = "drag_ID")

test <- drag_catches.1 %>%
  filter(is.na(vessel_number))

#panga
test <- drag_catches.1 %>%
  filter(species_code == "PANG")%>%
  filter(category_code == "RND UNG")%>%
  distinct(land_ID)

test.1 <- landings_catches.1 %>%
  inner_join(test, by = "land_ID")%>%
  group_by(land_ID, species_code)%>%
  summarise(nominal_mass = sum(nominal_mass))

test.2 <- drag.sub %>%
  inner_join(test, by = "land_ID")%>%
  group_by(land_ID, species_code)%>%
  summarise(cleaned_mass = sum(cleaned_mass))

test.3 <- test.1 %>%
  inner_join(test.2, by = c("land_ID", "species_code"))%>%
  filter(species_code== "PANG")%>%
  mutate(mass = nominal_mass/cleaned_mass)

drag.sub <- drag.sub %>%
  left_join(select(test.3,land_ID, species_code, mass))%>%
  mutate(nominal_mass = if_else(!is.na(mass),
                                cleaned_mass*mass, nominal_mass))%>%
  select(-mass)


drags_no_effort.1 <- test %>%
  mutate(fixed = "a")%>%
  full_join(drags_no_effort.1)%>%
  mutate(nominal_mass = if_else(is.na(fixed),
                                nominal_mass,
                                if_else(species_code == "PANG", 0, nominal_mass)))%>%
  select(-fixed)

#panga
test <- drag_catches.1 %>%
  filter(species_code == "PANG")%>%
  filter(category_code == "GUT UNG")%>%
  distinct(land_ID)

test.1 <- landings_catches.1 %>%
  inner_join(test, by = "land_ID")%>%
  group_by(land_ID, species_code)%>%
  summarise(nominal_mass = sum(nominal_mass))

test.2 <- drag.sub %>%
  inner_join(test, by = "land_ID")%>%
  group_by(land_ID, species_code)%>%
  summarise(cleaned_mass = sum(cleaned_mass))

test.3 <- test.1 %>%
  inner_join(test.2, by = c("land_ID", "species_code"))%>%
  filter(species_code== "PANG")%>%
  mutate(mass = nominal_mass/cleaned_mass)

drag.sub <- drag.sub %>%
  left_join(select(test.3,land_ID, species_code, mass))%>%
  mutate(nominal_mass = if_else(!is.na(mass),
                                cleaned_mass*mass, nominal_mass))%>%
  select(-mass)

#bc now some panga has been fixed
drags_no_effort.1 <- test %>%
  mutate(fixed = "a")%>%
  full_join(drags_no_effort.1)%>%
  mutate(nominal_mass = if_else(is.na(fixed),
                                nominal_mass,
                                if_else(species_code == "PANG", 0, nominal_mass)))%>%
  select(-fixed)

#hake
test <- drag_catches.1 %>%
  filter(species_code == "HAKE")%>%
  filter(category_code == "UNFILL")%>%
  filter(nominal_mass==0)%>%
  distinct(land_ID)

test.1 <- landings_catches.1 %>%
  inner_join(test, by = "land_ID")%>%
  group_by(land_ID, species_code)%>%
  summarise(nominal_mass = sum(nominal_mass))

test.2 <- drag.sub %>%
  inner_join(test, by = "land_ID")%>%
  group_by(land_ID, species_code)%>%
  summarise(cleaned_mass = sum(cleaned_mass))

test.3 <- test.1 %>%
  inner_join(test.2, by = c("land_ID", "species_code"))%>%
  filter(species_code== "HAKE")%>%
  mutate(mass = nominal_mass/cleaned_mass)

drag.sub <- drag.sub %>%
  left_join(select(test.3,land_ID, species_code, mass))%>%
  mutate(nominal_mass = if_else(!is.na(mass),
                                cleaned_mass*mass, nominal_mass))%>%
  select(-mass)

#shark
test <-drag_catches.1 %>%
  filter(species_code == "SHRK")%>%
  filter(nominal_mass==0)%>%
  distinct(land_ID)

test.1 <- landings_catches.1 %>%
  inner_join(test, by = "land_ID")%>%
  filter(species_code %in% c("SFSH", "HNSH"))%>%
  ungroup()%>%
  distinct(land_ID)

test.2 <- landings_catches.1 %>%
  inner_join(test.1, by = "land_ID")%>%
  group_by(land_ID, species_code)%>%
  summarise(nominal_mass = sum(nominal_mass))

test.3 <- drag.sub %>%
  inner_join(test.1, by = "land_ID")%>%
  group_by(land_ID, species_code)%>%
  summarise(cleaned_mass = sum(cleaned_mass))

test.4 <- test.2 %>%
  mutate(species_code = ifelse(species_code == "SFSH"|
                                 species_code == "HNSH",
                               "SHRK", species_code)) %>%
  group_by(land_ID, species_code) %>%
  summarise(nominal_mass= sum(nominal_mass))%>%
  right_join(test.3, by = c("land_ID", "species_code"))%>%
  mutate(mass = nominal_mass/cleaned_mass)

drag.sub1 <- drag.sub %>%
  left_join(select(test.4,land_ID, species_code, mass))%>%
  mutate(nominal_mass = if_else(!is.na(mass),
                                cleaned_mass*mass, nominal_mass))%>%
  select(-mass)

#bc shark has been fixed

drags_no_effort.1 <- test.1 %>%
  mutate(fixed = "a")%>%
  full_join(drags_no_effort.1)%>%
  mutate(nominal_mass = if_else(is.na(fixed),
                                nominal_mass,
                                if_else(species_code %in% c("SFSH", "HNSH"), 0, nominal_mass)))%>%
  select(-fixed)


#DEMF
test <- drag_catches.1%>%
  filter(species_code == "DEMF")%>%
  #  filter(nominal_mass==0)%>%
  distinct(land_ID)

test.1 <- drags_no_effort.1 %>%
  inner_join(test)%>%
  group_by(land_ID)%>%
  summarise(nominal_mass = sum(nominal_mass))

test.2 <- drag.sub %>%
  inner_join(test, by = "land_ID")%>%
  group_by(land_ID, species_code)%>%
  summarise(cleaned_mass = sum(cleaned_mass))

test.3 <- test.1 %>%
  inner_join(test.2, by = c("land_ID"))%>%
  filter(species_code== "DEMF")%>%
  mutate(mass = nominal_mass/cleaned_mass)

drag.sub <- drag.sub %>%
  left_join(select(test.3,land_ID, species_code, mass))%>%
  mutate(nominal_mass = if_else(!is.na(mass),
                                nominal_mass+cleaned_mass*mass, nominal_mass))%>%
  select(-mass)

#sole
drag.sub <- drag.sub %>%
  filter(species_code!= "SOLE")

#monk

test <- drag_catches.1 %>%
  filter(species_code == "MONK")%>%
  distinct(land_ID)

test.1 <- drag.sub %>%
  inner_join(test, by = "land_ID")%>%
  count(land_ID, species_code, category_code)


drag.sub <- drag.sub %>%
  left_join(select(test.1,land_ID, species_code, category_code, n))%>%
  mutate(nominal_mass = if_else(species_code=="MONK",
                                nominal_mass/n,
                                nominal_mass))%>%
  select(- n)

#chok

test <- drag.sub %>%
  filter(species_code == "CHOK")%>%
  filter(nominal_mass==0)%>%
  distinct(land_ID)

test.1 <- landings_catches.1 %>%
  inner_join(test)%>%
  group_by(land_ID, species_code, category_code)%>%
  summarise(nominal_mass = sum(nominal_mass))

test.2 <- drag.sub %>%
  inner_join(test, by = "land_ID")%>%
  group_by(land_ID, species_code, category_code)%>%
  summarise(cleaned_mass = sum(cleaned_mass))

test.3 <- test.1 %>%
  inner_join(test.2, by = c("land_ID", "species_code", "category_code"))%>%
  filter(species_code== "CHOK")%>%
  mutate(mass = nominal_mass/cleaned_mass)

drag.sub <- drag.sub %>%
  left_join(select(test.3,land_ID, species_code, category_code, mass))%>%
  mutate(nominal_mass = if_else(!is.na(mass),
                                cleaned_mass*mass, nominal_mass))%>%
  select(-mass)


#Fix the species codes which are errors in the
#trip and trawl records

#trip
unique(landings_catches.1$species_code)
landings_catches.1 <- landings_catches.1 %>%
  mutate(species_code = ifelse(species_code == "SOLE"|
                                 species_code == "SOL"|
                                 species_code == "SO",
                               "ECSOLE", species_code))%>%
  mutate(species_code = ifelse(species_code == "ROM",
                               "ROMN", species_code))%>%
  mutate(species_code = ifelse(species_code == "HKO"|
                                 species_code =="HOT"|
                                 species_code =="LIS"|
                                 species_code =="MIL"|
                                 species_code =="SMO"|
                                 species_code =="COD"|
                                 species_code =="SAN",
                               "unknown", species_code))

#trawl
drag_catches.1 <- drag.sub %>%
  mutate(species_code = ifelse(species_code == "SOLE"|
                                 species_code == "SOL"|
                                 species_code == "SO",
                               "ECSOLE", species_code))%>%
  mutate(species_code = ifelse(species_code == "ROM",
                               "ROMN", species_code))%>%
  mutate(species_code = ifelse(species_code == "~B"|
                                 species_code == "GR" |
                                 species_code =="14X"|
                                 species_code =="1TJ"|
                                 species_code =="2IX"|
                                 species_code == "3IX"|
                                 species_code =="4IX"|
                                 species_code == "A"|
                                 species_code == "GNS"|
                                 species_code =="GUB"|
                                 species_code =="H\\O"|
                                 species_code =="IX"|
                                 species_code =="KAR"|
                                 species_code =="MIO"|
                                 species_code =="MIS"|
                                 species_code =="MIU"|
                                 species_code =="MJI"|
                                 species_code =="MMI"|
                                 species_code =="MNI"|
                                 species_code =="MUI"|
                                 species_code =="SRJ"|
                                 species_code =="STK"|
                                 species_code =="STU"|
                                 species_code =="X"|
                                 species_code == "000"|
                                 species_code == "JOS"|
                                 species_code == "SAN"|
                                 species_code == "SAR"|
                                 species_code =="SRA",
                               "unknown", species_code))


#get scientific names
species_code <- c("APMF",
                  "BLHT",
                  "BNST",
                  "BRDMAN",
                  "BTSN",
                  "CHOK",
                  "CRPN",
                  "CYNZAN",
                  "DEMF",
                  "DEMLIN",
                  "DGSH",
                  "ECSOLE",
                  "ELF",
                  "GLBK",
                  "GURN",
                  'HAKE',
                  'HMKC',
                  'HNSH',
                  'JCPV',
                  'JDRY',
                  'KKLP',
                  'KOB',
                  'MCKR',
                  'MONK',
                  'OCTOPS',
                  'OCTOPU',
                  'OMMAST',
                  'PANG',
                  'PGGY',
                  'RAJIDE',
                  'RDFS',
                  'RDST',
                  'ROMN',
                  'ROUGHY',
                  'ROVE',
                  'RSTM',
                  'SFSH',
                  'SGRN',
                  'SHRK',
                  'SJSH',
                  'SNOK',
                  'SNTR',
                  'SSLD',
                  'STNT',
                  'SWFS',
                  'TUNA',
                  'WBRBL',
                  'WCSOLE',
                  'WHST',
                  'WRFS',
                  'WSTM',
                  'MLLT',
                  'MRLN',
                  'SEPIA'
)

scientific_name <- c('Brama brama',
                     'Pachymetopon aeneum',
                     'Chirodactylus grandis',
                     'Umbrina canariensis',
                     'Lepidopus caudatus',
                     'Loligo vulgaris reynaudii',
                     'Argyrozona argyrozona',
                     'Cynoglossus zanzibarensis',
                     'Demersal',
                     'Demersal teleostei',
                     'Squalus',
                     'Austroglossus pectoralis',
                     'Pomatomus saltatrix',
                     'Atractoscion aequidens',
                     'Chelidonichthys',
                     'Merluccius',
                     'Trachurus capensis',
                     'Mustelus',
                     'Helicolenus dactylopterus',
                     "Zeus capensis",
                     'Genypterus capensis',
                     'Argyrosomus',
                     'Scomber japonicus',
                     'Lophius vomerinus',
                     'Octopoda',
                     'Octopus',
                     'Ommastrephidae',
                     'Pterogymnus laniarius',
                     'Pomadasys olivaceum',
                     'Rajiformes',
                     'Teleostei redfish',
                     'Petrus rupestris',
                     'Chrysoblephus laticeps',
                     'Hoplostethus atlanticus',
                     "Emmelichthys nitidus",
                     'Chrysoblephus gibbiceps',
                     'Galeorhinus galeus',
                     'Pomadasys commersonni',
                     'Selachii',
                     'Callorhinchus capensis',
                     'Thyrsites atun',
                     'Cheimerius nufar',
                     'Pagellus bellottii natalensis',
                     'Spondyliosoma emarginatum',
                     'Xiphias gladius',
                     'Scombridae',
                     'Galeichthys feliceps',
                     'Austroglossus microlepis',
                     'Lithognathus lithognathus',
                     'Polyprion americanus',
                     'Rhabdosargus globiceps',
                     'Mugilidae',
                     'Istiophoridae',
                     'Sepiida')

species_names <- data.frame(species_code, scientific_name)

#add scientific names

drag_catches.1 <- drag_catches.1 %>%
  left_join(species_names, by = "species_code") %>% 
  filter(species_code != "unknown") #remove unknown species codes

landings_catches.1 <- landings_catches.1 %>%
  left_join(species_names, by = "species_code")%>% 
  filter(species_code != "unknown") #remove unknown species codes

spp <- landings_catches.1 %>%
  distinct(species_code, scientific_name)
spp1 <- drag_catches.1 %>%
  distinct(species_code, scientific_name)

#make fields lower case

drags.1 <- drags.1 %>%
  rename(land_id = land_ID,
         drag_id = drag_ID)
landings.1 <- landings.1 %>%
  rename(land_id = land_ID)
drag_catches.1 <- drag_catches.1 %>%
  rename(drag_id = drag_ID)
landings_catches.1 <- landings_catches.1 %>%
  rename(land_id = land_ID)

#clean species names
# 
# drag_catches.1 <- drag_catches.1 %>%
#   createDB::clean_names(scientific_name)
# landings_catches.1 <- landings_catches.1 %>%
#   createDB::clean_names(scientific_name)

# add in missing longs and lats
#remove records with no grid codes and grid codes 921 and 916
grids <- read.csv("data/ComGrids.csv")

grid_midpnt <- grids %>%
  mutate(mid_lat = minimum_latitude + ((maximum_latitude - minimum_latitude)/2)) %>%
  mutate(mid_long = minimum_longitude + ((maximum_longitude - minimum_longitude)/2)) %>%
  select(code, mid_lat, mid_long)

drags.2  <- drags.1 %>%
  left_join(grid_midpnt, by = c("grid_code" = "code")) %>%
  # mutate(start_latitude = if_else(start_latitude == 0 | is.na(start_latitude), mid_lat, start_latitude))%>%
  # mutate(start_longitude = if_else(start_longitude == 0 | is.na(start_longitude), mid_long, start_longitude)) %>%
  filter(!is.na(grid_code))%>%
  filter(!grid_code %in% c("921", "916")) ##bc has no midpoint coords

##only take locations within the eez
# 
# drags.2 <- clean_area(drags.1, "start_longitude", "start_latitude", "eez_mainlandRSA_buffered_1km_allEEZversions.shp", 20)


### filter records
###--------------------------------------------------------------------------
##All trips lasting less than two
##days, or with less than two trawls were deemed incomplete trips and were also
##removed.
##All trawls lasting less than 30 minutes, or more than nine hours, were
##removed.
## Teleostei demersal and Teleostei redfish could not be assigned to any
##species and so were removed from the records.
##remove species codes that are not species
##only take vessel that fished in more than 50% years
##only take gridcells in the inshore trawl grounds

vessels <- landings.1 %>%
  distinct(docking_date_yy, vessel_number) %>%
  count(vessel_number) %>%
  filter(n>=15) %>%
  select(-n)

# gridcells <- grids %>%
#   filter(inshore_area==1) %>%
#   select(code) %>%
#   rename(grid_code = code)

gridcells <- c("512", 
               "513",
               "514", 
               "515", 
               "516", 
               "517", 
               "518", 
               "519", 
               "520", 
               "521", 
               "522", 
               "523", 
               "524", 
               "525", 
               "526", 
               "527", 
               "528", 
               "529", 
               "530", 
               "531", 
               "532", 
               "533", 
               "535", 
               "536", 
               "537", 
               "538", 
               "539", 
               "540", 
               "541", 
               "542", 
               "543", 
               "550", 
               "551", 
               "552", 
               "553", 
               "554", 
               "555", 
               "556", 
               "565", 
               "566", 
               "567", 
               "568", 
               "569", 
               "570", 
               "583", 
               "584", 
               "622", 
               "625", 
               "628", 
               "629", 
               "632", 
               "633", 
               "636", 
               "640")##all gridcells within the 200 meter contour line


## combine datasets and filter them

drag <- drags.2 %>%
  full_join(drag_catches.1)%>%
  group_by(land_id, drag_id, docking_date_yy, docking_date_mm,
           vessel_number, grid_code,mid_lat,mid_long, target_species_code,
           species_code, scientific_name, trawl_length_hh) %>%
  summarise(nominal_mass = sum(nominal_mass)) %>%
  ungroup() %>%
  right_join(vessels) %>%
#  right_join(gridcells) %>%
  filter(grid_code %in% gridcells) %>% 
  filter(!is.na(land_id))%>%
  filter(!species_code %in% c("DEMF", "DEMLIN", "RDFS")) %>%
  filter(between(trawl_length_hh, 0.5, 9))%>%
  filter(!is.na(species_code))%>%
  filter(!is.na(nominal_mass))

eez <- sf::st_read(dir("data", "eez_mainlandRSA_buffered_1km_allEEZversions.shp", full.names = T, recursive = T)[1])

map <- drag %>%
  distinct(grid_code,mid_lat,mid_long) %>% 
  sf::st_as_sf(coords = c("mid_long", "mid_lat"), crs = 4326)%>%
  sf::st_transform(crs = sf::st_crs(eez))

ggplot()+
  geom_sf(data = eez, fill = NA )+
  geom_sf(data = map, size = 1)+
  theme_classic()

test <- drag %>%
  count(grid_code)
  filter(is.na(nominal_mass))

trips <- drag %>%
  ungroup() %>%
  distinct(land_id)

landing <- landings.1 %>%
  full_join(landings_catches.1)%>%
  group_by(land_id, docking_date_yy, docking_date_mm, vessel_number, species_code, scientific_name,
           trip_length_dd, number_of_trawls)%>%
  summarise(nominal_mass = sum(nominal_mass)) %>%
  ungroup() %>%
  right_join(trips)%>%
  filter(!is.na(land_id))%>%
  filter(!species_code %in% c("DEMF", "DEMLIN", "RDFS"))%>%
  filter(trip_length_dd >2)%>%
  filter(number_of_trawls >2)%>%
  filter(!is.na(species_code))

trips <- landing %>%
  ungroup() %>%
  distinct(land_id)


drag <- drag %>%
  right_join(trips)



##split all remaining shark records evenly between HNSH and SFSH

drag.1 <- drag %>%
  select(-scientific_name) %>% 
  pivot_wider(names_from = species_code, values_from = nominal_mass)%>%
  mutate_all(~replace_na(.,0))%>%
  mutate(HNSH = HNSH+(SHRK/2))%>%
  mutate(SFSH = SFSH+(SHRK/2))%>%
  select(-c(SHRK))%>%
  pivot_longer(cols = 11:57,
               names_to = "species_code",
               values_to = "nominal_mass",
               values_drop_na = T) %>%
  mutate(nominal_mass = na_if(nominal_mass, 0)) %>%
  na.omit()%>%
  left_join(species_names, by = "species_code")


landing.1 <- landing%>%
  select(-scientific_name) %>% 
  pivot_wider(names_from = species_code, values_from = nominal_mass)%>%
  mutate_all(~replace_na(.,0))%>%
  mutate(HNSH = HNSH+(SHRK/2))%>%
  mutate(SFSH = SFSH+(SHRK/2))%>%
  select(-c(SHRK))%>%
  pivot_longer(cols = 7:53,
               names_to = "species_code",
               values_to = "nominal_mass",
               values_drop_na = T) %>%
  mutate(nominal_mass = na_if(nominal_mass, 0)) %>%
  na.omit()  %>%
  left_join(species_names, by = "species_code")

test <- drag.1 %>% 
  distinct(species_code, scientific_name)

## save tables to clean database

con <- sql_con("masters_paper")



dbWriteTable(con, "dit_drag_clean",drag.1,overwrite = T )
dbWriteTable(con, "dit_landing_clean",landing.1,overwrite = T )

dbDisconnect(con)

rm(list = ls())

print("FINISHED 2. clean_DIT")



