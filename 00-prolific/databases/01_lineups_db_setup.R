# lineups database

# Load Libraries ---------------------------------------------------------------

library(RSQLite)
library(DBI)
library(tidyverse)
library(patchwork)
library(here)

# Connect to data base ---------------------------------------------------------

filename <- "00-prolific/databases/01_lineups_db.db"
sqlite.driver <- dbDriver("SQLite")
db_con <- dbConnect(sqlite.driver, dbname = filename)
dbListTables(db_con)

# Experiment Details -----------------------------------------------------------

experiment_details <- dbReadTable(db_con,"experiment_details")
experiment_details <- tibble(experiment = "emily-log2",
                            question   = "Which plot is the most different?",
                            reasons    = "Clustering,Different range,Different shape,Different slope,Outlier(s),Other",
                            lpp        = 13,
                            trials_req = 0
                            )
dbRemoveTable(db_con, "experiment_details")
dbWriteTable(db_con, "experiment_details", experiment_details)
experiment_details <- dbReadTable(db_con,"experiment_details")
experiment_details

# Feedback ---------------------------------------------------------------------

feedback <- dbReadTable(db_con,"feedback")
# feedback <- tibble(ip_address      = "test",
#                    nick_name       = "test",
#                    study_starttime = NA,
#                    prolific_id     = "test",
#                    order           = NA,
#                    start_time      = NA,
#                    end_time        = NA,
#                    pic_id          = NA,
#                    param_value     = "test",
#                    set             = NA,
#                    test_param      = "test",
#                    correct         = NA,
#                    response_no     = NA,
#                    conf_level      = "test",
#                    choice_reason   = "test"
#                   )
# feedback <- feedback[0,]
# dbRemoveTable(db_con, "feedback")
# dbWriteTable(db_con, "feedback", feedback)
# feedback <- dbReadTable(db_con, "feedback")
feedback

# Picture Details --------------------------------------------------------------
# The only thing that affects sampling strategy at the moment is experiments/.../randomization.R
# which creates a vector of pic_id and a vector of trial_id values

picture_details <- dbReadTable(db_con,"picture_details")
# Create the empty database table - this is here for documentation
# picture_details <- tibble::tibble(
#   pic_id = NULL, # this should be numeric and unique to the picture
#   sample_size = NULL, # this can be anything -- used as a metadata field
#   test_param = NULL, # this can be anything -- used as a metadata field
#   param_value = NULL, # this can be anything -- metadata
#   p_value = NULL, # this can be anything -- metadata -- most useful for any numerical simulation evaluation measure
#   obs_plot_location = NULL, # a number, or multiple comma-separated numbers. For Rorschach lineups, use 0
#   pic_name = NULL, # picture name without folder/path info
#   experiment = NULL, # experiment shorthand
#   difficulty = NULL, # this can be anything -- metadata -- useful for separating blocks
#   data_name = NULL, # name/file of saved dataset to generate the lineup
#   trial = NULL # Used to indicate if a picture is a trial plot or not...
#   # added when picture_details and picture_details_trial are combined.
# )
# 
# picture_details <- readr::read_csv(here("00-prolific", "plots", "picture-details.csv")) %>%
#   mutate(trial = 0)
# dbRemoveTable(db_con, "picture_details")
# dbWriteTable(db_con, "picture_details", picture_details)
# picture_details <- dbReadTable(db_con,"picture_details")
picture_details

# Disconnect from database -----------------------------------------------------

dbDisconnect(db_con) 
