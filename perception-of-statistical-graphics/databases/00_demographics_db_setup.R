# demographics database

# Load Libraries ---------------------------------------------------------------

library(RSQLite)
library(DBI)
library(tidyverse)
library(patchwork)
library(here)

# Connect to data base ---------------------------------------------------------

filename <- "perception-of-statistical-graphics/databases/00_demographics_db.db"
sqlite.driver <- dbDriver("SQLite")
db_con <- dbConnect(sqlite.driver, dbname = filename)
dbListTables(db_con)

# Users Data -------------------------------------------------------------------

users <- dbReadTable(db_con,"users")
# users <- tibble(nick_name       = "test",
#                 ip_address      = NA,
#                 study_starttime = NA,
#                 prolific_id     = NA,
#                 age             = NA,
#                 gender          = NA,
#                 academic_study  = NA,
#                 recruitment     = NA
#                 )
# users <- users[0,]
# dbRemoveTable(db_con, "users")
# dbWriteTable(db_con, "users", users)
# users <- dbReadTable(db_con,"users")
users

# Disconnect from database -----------------------------------------------------

dbDisconnect(db_con)

