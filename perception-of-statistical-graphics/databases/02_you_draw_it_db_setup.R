# you draw it database

# Load Libraries ---------------------------------------------------------------

library(RSQLite)
library(DBI)
library(tidyverse)
library(patchwork)
library(here)

# Connect to database ----------------------------------------------------------

filename <- "perception-of-statistical-graphics/databases/02_you_draw_it_db.db"
sqlite.driver <- dbDriver("SQLite")
db_con <- dbConnect(sqlite.driver, dbname = filename)
dbListTables(db_con)

# Experiment Details -----------------------------------------------------------

experiment_details <- dbReadTable(db_con,"experiment_details")
# experiment_details <- experiment_details[0,]
# experiment_details <- data.frame(experiment = "emily-you-draw-it-pilot-app",
# question   = "Use your mouse to fill in the trend in the yellow box region.",
# ydi_pp     = 12,
# trials_req = 0
# )
# dbRemoveTable(db_con, "experiment_details")
# dbWriteTable(db_con, "experiment_details", experiment_details)
# experiment_details <- dbReadTable(db_con,"experiment_details")
experiment_details

# Exponential Scale Study Parameter Details ------------------------------------

exp_parameter_details <- dbReadTable(db_con,"exp_parameter_details")
# exp_parameter_details <- data.frame(beta = c(0.1, 0.23),
#                                     sd = c(0.09, 0.25)) %>%
#                           expand_grid(N = 30,
#                                       x_min = 0,
#                                       x_max = 20,
#                                       x_by = 0.25
#                           )
# dbRemoveTable(db_con, "exp_parameter_details")
# dbWriteTable(db_con,  "exp_parameter_details", exp_parameter_details)
# exp_parameter_details <- dbReadTable(db_con,"exp_parameter_details")
exp_parameter_details

# Eye Fitting Parameter Details ------------------------------------------------

eyefitting_parameter_details <- dbReadTable(db_con,"eyefitting_parameter_details")
# eyefitting_parameter_details <- tibble(parm_id = c("S", "F", "V", "N"),
#                                        y_xbar = c(3.88, 3.9, 3.89, 4.11),
#                                        slope  = c(0.66, 0.66, 1.98, -0.70),
#                                        sigma  = c(1.3, 2.8, 1.5, 2.5),
#                                        x_min   = c(0, 0, 4, 0),
#                                        x_max   = c(20, 20, 16, 20)) %>%
#                                 expand_grid(x_by = 0.25)
# dbRemoveTable(db_con, "eyefitting_parameter_details")
# dbWriteTable(db_con,  "eyefitting_parameter_details", eyefitting_parameter_details)
# eyefitting_parameter_details <- dbReadTable(db_con,"eyefitting_parameter_details")
eyefitting_parameter_details

# Feedback Drawn Data ----------------------------------------------------------

feedback <- dbReadTable(db_con,"feedback")
# feedback <- tibble(parm_id    = "test",
#                    x          = NA,
#                    y          = NA,
#                    ydrawn     = NA,
#                    linear     = NA,
#                    nick_name  = "test",
#                    ip_address = "test",
#                    prolific_id = "test",
#                    study_starttime = NA,
#                    start_time = NA,
#                    end_time   = NA
#                    )
# feedback <- feedback[0,]
# dbRemoveTable(db_con, "feedback")
# dbWriteTable(db_con, "feedback", feedback)
# feedback <- dbReadTable(db_con, "feedback")
feedback

with(feedback, table(prolific_id, parm_id))

feedback %>%
  filter(x == 10) %>%
  group_by(prolific_id, parm_id) %>%
  unique()

feedback %>%
  filter(x == 10) %>%
  group_by(prolific_id, parm_id) %>%
  unique() %>%
  arrange(parm_id)

# Simulated Data ---------------------------------------------------------------
simulated_data <- dbReadTable(db_con,"simulated_data")
# simulated_data <- tibble(parm_id    = "test",
#                          dataset    = NA,
#                          x          = NA,
#                          y          = NA,
#                          ip_address = "test",
#                          nick_name  = "test",
#                          study_starttime = NA,
#                          prolific_id = "test"
#                          )
# simulated_data <- simulated_data[0,]
# dbRemoveTable(db_con, "simulated_data")
# dbWriteTable(db_con, "simulated_data", simulated_data)
# simulated_data <- dbReadTable(db_con, "simulated_data")
simulated_data

simulated_data %>%
  filter(x == 10) %>%
  group_by(prolific_id, parm_id) %>%
  unique()

# Disconnect from database -----------------------------------------------------

dbDisconnect(db_con)
