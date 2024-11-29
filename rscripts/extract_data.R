# ---
# title: "extract the demersal trawl catch database and put it into the database"
# author: "Natasha Besseling"
# date: "2023-04-26"
# ---

## load libraries
library(DBI)
library(tidyverse)
library(createDB)

## create a database
con <- dbConnect(RPostgres::Postgres(),
                 port = Sys.getenv("DB_PORT"),
                 user = Sys.getenv("DB_USER"),
                 password = Sys.getenv("DB_PASSWORD")) 

dbSendQuery(con, "create database masters_paper")

##close connection to the database
dbDisconnect(con)

##extract data

extract_accs("DemersalInshoreTrawl1990-2019_REQUESTS.accdb")


##add the tables into the databse

## connect to sql database

con <- sql_con("masters_paper")


## write tables to the Postgres database

DBI::dbWriteTable(con, "dit_drag_catches",drag_catches,overwrite = T )
DBI::dbWriteTable(con, "dit_drags",drags,overwrite = T )
DBI::dbWriteTable(con, "dit_drags_no_effort",drags_no_effort,overwrite = T )
DBI::dbWriteTable(con, "dit_landings",landings,overwrite = T )
DBI::dbWriteTable(con, "dit_landings_catches",landings_catches,overwrite = T )
DBI::dbWriteTable(con, "dit_REF_dbo_product_category_factors",REF_dbo_product_category_factors,overwrite = T)
print ("DIT: finished writing extracted tables to Postgres")

## close connection
DBI::dbDisconnect(con)

## remove all objects
rm(list=setdiff(ls(), "run_scripts"))








































