library(RSQLite)
library(DBI)
library(tidyverse)
library(patchwork)
library(here)

# Connect to data base ---------------------------------------------

filename <- "00-prolific/demographics.db"
sqlite.driver <- dbDriver("SQLite")
db_con <- dbConnect(sqlite.driver, dbname = filename)
dbListTables(db_con)

# Users Data -------------------------------------------------------------------

users <- dbReadTable(db_con,"users")
# users <- tibble(nick_name       = "test",
#                 study_starttime = NA,
#                 prolific_id     = NA,
#                 age             = NA,
#                 gender          = NA,
#                 academic_study  = NA,
#                 recruitment     = NA,
#                 ip_address      = NA
#                 )
# users <- users[0,]
# dbRemoveTable(db_con, "users")
# dbWriteTable(db_con, "users", users)
# users <- dbReadTable(db_con,"users")
users

# Disconnect to data base ---------------------------------------------

dbDisconnect(db_con)
