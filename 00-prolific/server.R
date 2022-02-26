# prolific sever

# ------------------------------------------------------------------------------
# LOAD LIBRARIES ---------------------------------------------------------------
# ------------------------------------------------------------------------------

# Shiny specific
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyhelper)

# Data Management and Plotting
library(tidyverse)
library(scales)
library(purrr)
library(lubridate)

# Plotting Specifics
library(r2d3)
library(gridSVG)

# Data Importing and Exporting
library(readr)
# library(here)
library(RSQLite)
library(DBI)
sqlite.driver <- dbDriver("SQLite")

# ------------------------------------------------------------------------------
# OVERALL SET UP ---------------------------------------------------------------
# ------------------------------------------------------------------------------

window_dim_min <- 600

# add resource paths so Shiny can see them
addResourcePath("plots", "plots")
addResourcePath("trials", "trials")
addResourcePath("examples", "examples")

# ------------------------------------------------------------------------------
# SETUP STUDY 1: LINEUPS -------------------------------------------------------
# ------------------------------------------------------------------------------

lineups_experiment_name <- "emily-log-1"

# define folders
plots_folder <- "plots" # subfolders for data, pdf, png, svg. picture_details.csv in this folder
trials_folder <- "trials" # subfolders for svg. picture_details_trial.csv in this folder

con <- dbConnect(SQLite(), dbname = "databases/01_lineups_data.db")
lineups_experiment <- dbReadTable(con, "experiment_details")
dbDisconnect(con)

# ------------------------------------------------------------------------------
# SETUP STUDY 2: YOU DRAW IT ---------------------------------------------------
# ------------------------------------------------------------------------------

# Turn a list of data into a json file -------

data_to_json <- function(data) {
  jsonlite::toJSON(data,
                   dataframe = "rows",
                   auto_unbox = FALSE,
                   rownames = TRUE)
}

# Redefine drawr function -----

drawr <- function(data,
                  linear           = "true",
                  draw_start        = NULL,
                  points_end        = NULL,
                  x_by              = 0.25,
                  free_draw         = T,
                  points            = "partial",
                  aspect_ratio      = 1.5,
                  title             = "",
                  x_range           = NULL,
                  y_range           = NULL,
                  x_lab             = "",
                  y_lab             = "",
                  drawn_line_color  = "steelblue",
                  data_tab1_color   = "steelblue",
                  x_axis_buffer     = 0.01,
                  y_axis_buffer     = 0.05,
                  show_finished     = T,
                  shiny_message_loc = NULL) {
  
  line_data  <- data$line_data
  point_data <- data$point_data
  
  x_min <- min(line_data$x)
  x_max <- max(line_data$x)
  y_min <- min(line_data$y)
  y_max <- max(line_data$y)
  
  if (is.null(x_range)) {
    x_buffer <- (x_max - x_min) * x_axis_buffer
    x_range <- c(x_min - x_buffer, x_max + x_buffer)
  }
  if (is.null(y_range)) {
    y_buffer <- (y_max - y_min) * y_axis_buffer
    y_range <- c(y_min - y_buffer, y_max + y_buffer)
    if (linear != "true") {
      if (y_range[1] <= 0) {
        y_range[1] <- min(y_min, y_axis_buffer)
      }
    }
  } else {
    if (y_range[1] > y_min | y_range[2] < y_max) {
      stop("Supplied y range doesn't cover data fully.")
    }
  }
  
  if ((draw_start <= x_min) | (draw_start >= x_max)) {
    stop("Draw start is out of data range.")
  }
  
  r2d3::r2d3(data   = data_to_json(data),
             script = "www/js/shinydrawr-d3v5.js",
             dependencies = c("d3-jetpack"),
             d3_version = "5",
             options = list(draw_start        = draw_start,
                            points_end        = points_end,
                            linear            = as.character(linear),
                            free_draw         = free_draw,
                            points            = points,
                            aspect_ratio      = aspect_ratio,
                            pin_start         = T,
                            x_range           = x_range,
                            x_by              = x_by,
                            y_range           = y_range,
                            line_style        = NULL,
                            data_tab1_color   = data_tab1_color,
                            drawn_line_color  = drawn_line_color,
                            show_finished     = show_finished,
                            shiny_message_loc = shiny_message_loc,
                            title             = title)
  )
  
}

you_draw_it_experiment_name <- "emily-log-you-draw-it-pilot-app"

con <- dbConnect(sqlite.driver, dbname = "databases/02_you_draw_it_data.db")
you_draw_it_experiment <- dbReadTable(con, "experiment_details")
dbDisconnect(con)

# ------------------------------------------------------------------------------
# SETUP STUDY 3: ESTIMATION ----------------------------------------------------
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# BEGIN SERVER -----------------------------------------------------------------
# ------------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # shinyjs::disable(selector = '.navbar-nav a')
  study_starttime = now()
  
# ------------------------------------------------------------------------------
# INTRO / WELCOME --------------------------------------------------------------
# ------------------------------------------------------------------------------
  
  # Provide a message if the browser is too small
  observeEvent(input$dimension, {
    if (any(input$dimension < window_dim_min))
      showModal(
        modalDialog(
          title = "Window Size is too small",
          sprintf("You must view this experiment in a browser window which is at least %s x %s", window_dim_min, window_dim_min),
          size = "s",
          easyClose = T
        )
      )
    else {
      removeModal()
    }
  })
  
  observeEvent(input$beginexp, {
    
    # Only start experiment when consent is provided
    if (input$consent){
    # move to demographics
    updateTabsetPanel(session, "inNavBar",selected = "demographics-tab")
    } else {
      showNotification("Please provide informed consent.")
    }
    
  })
  
# ------------------------------------------------------------------------------
# DEMOGRAPHIC INFORMATION ------------------------------------------------------
# ------------------------------------------------------------------------------
  
  # add demographic information to the database
  observeEvent(input$submitdemo, {
    
    if (!is.null(input$nickname) && nchar(input$nickname) > 0 && !any(input$dimension < window_dim_min)) {
      
      if (!is.null(input$age) && 
         (!is.null(input$gender) & input$gender != "") && 
         (!is.null(input$education) & input$education != "") && 
         (!is.null(input$recruitment) & input$recruitment != "") && 
         (!is.null(input$prolificID) & input$prolificID != "")) {
      # connect to data base
      con <- dbConnect(SQLite(), dbname = "databases/00_demographics.db")
      
      age <- ifelse(is.null(input$age), "", input$age)
      gender <- ifelse(is.null(input$gender), "", input$gender)
      academic_study <- ifelse(is.null(input$education), "", input$education)
      
      demoinfo <- data.frame(nick_name = input$nickname,
                             study_starttime = study_starttime,
                             prolific_id = as.character(input$prolificID),
                             age = age,
                             gender = gender,
                             academic_study = academic_study,
                             recruitment = input$recruitment,
                             ip_address = input$ipid)
      
      dbWriteTable(con, "users", demoinfo, append = TRUE, row.names = FALSE)
      dbDisconnect(con)
      
      # move to study 1
      updateTabsetPanel(session, "inNavBar", selected = "study1-lineups-tab")
      
      } else {
        showNotification("Please complete demographics.")
      }
    }
  })


# ------------------------------------------------------------------------------
# STUDY 1: LINEUP --------------------------------------------------------------
# ------------------------------------------------------------------------------
  
  # This needs to be run every connection, not just once.
  source("code/lineup/randomization.R")

  # reactive lineup_values to control the trials
  lineup_values <- reactiveValues(
        experiment = lineups_experiment$experiment,
        question = lineups_experiment$question,
        pics = NULL,
        submitted = FALSE, choice = NULL,
        reasons = strsplit(lineups_experiment$reasons, ",")[[1]],
        starttime = NULL,
        trialsreq = lineups_experiment$trials_req,
        trialsleft = lineups_experiment$trials_req,
        lpp = lineups_experiment$lpp,
        lppleft = lineups_experiment$lpp,
        pic_id = 0, choice = NULL,
        correct = NULL, result = "")

    output$debug <- renderText({lineups_experiment$question})

    # Show other text input box if other is selected
    observe({
        if (length(lineup_values$reasons) == 1) {
            updateCheckboxInput(session, "otheronly", value = TRUE)
            updateTextInput(session, "other", label = "Reason")
        }
    })

    # Provide experiment-appropriate reasoning boxes
    observe({
        updateCheckboxGroupInput(session, "reasoning",
                                 choices = lineup_values$reasons, selected = NA)
    })

# LINEUP EXAMPLES --------------------------------------------------------------
  
    # Lineup Text and Instructions
    output$lineups_text <- renderUI({
      HTML("The following examples illustrate the types of questions you may encounter during this experiment.")
    })
    
    # Lineup Example 1
    output$lineups_example1_q <- renderText({
        return(paste0("Example 1: ", lineup_values$question))
    })

    output$lineups_example1_plot <- renderImage({
        if (is.null(lineup_values$experiment)) return(NULL)

        list(src = file.path("examples", "lineups", "example1.png"),
             contentType = 'image/png',
             style = "max-width:100%; max-height = 100%")
    }, deleteFile = FALSE)

    output$lineups_example1_a <- renderUI({
        return(HTML(paste0("
            Your choice: <b>Plot 4</b><br/>
            Reasoning: <b>", lineup_values$reasons[1], "</b><br/>
            How certain are you: <b>Very Certain</b><br/>
        ")))
    })

    # Lineup Example 2
    output$lineups_example2_q <- renderText({
        return(paste0("Example 2: ", lineup_values$question))
    })

    output$lineups_example2_plot <- renderImage({
        if (is.null(lineup_values$experiment)) return(NULL)

        list(src = file.path("examples", "lineups", "example2.png"),
             contentType = 'image/png',
             style = "max-width:100%; max-height = 100%")
    }, deleteFile = FALSE)

    output$lineups_example2_a <- renderUI({
        return(HTML(paste0("
                    Your choice: <b>Plot 1</b><br/>
                    Reasoning: <b>", lineup_values$reasons[2], "</b><br/>
                    How certain are you: <b>Certain</b><br/>
                    ")))
    })
    
    # Start lineup study
    observeEvent(input$begin_lineups, {
      updateCheckboxInput(session, "lineups_go", value = TRUE)
    })
    
    
    # LINEUP QUESTION FLOW -----------------------------------------------------
    
    
    
# ------------------------------------------------------------------------------
# STUDY 2: YOU DRAW IT ---------------------------------------------------------
# ------------------------------------------------------------------------------
    
    source("code/you-draw-it/data-generation.R")
    
    # reactive values to control the trials
    you_draw_it_values <- reactiveValues(
      experiment = you_draw_it_experiment$experiment,
      question = you_draw_it_experiment$question,
      practicetext = "",
      practicegif_file = "",
      pics = NULL,
      submitted = FALSE,
      done_drawing = FALSE,
      choice = NULL,
      starttime = NULL,
      practicereq  = nrow(practice_data),
      practiceleft = nrow(practice_data),
      ydipp   = you_draw_it_experiment$ydi_pp,
      ydippleft = you_draw_it_experiment$ydi_pp,
      parms = 0,
      taskNum = NA,
      linear  = NULL,
      result = "")
    
    
    # ---- You Draw It Examples ------------------------------------------------
    
    # You Draw It welcome text and instructions
    output$you_draw_it_text <- renderUI({
      HTML("The following examples illustrate the types of questions you may encounter during this experiment.")
    })
    
    output$you_draw_it_example1_q <- renderText({
      return(paste0("Example 1: ", you_draw_it_values$question))
    })
  
    output$you_draw_it_example2_q <- renderText({
      return(paste0("Example 2: ", you_draw_it_values$question))
    })
    
    # Start you draw it study
    observeEvent(input$begin_you_draw_it, {
      updateCheckboxInput(session, "you_draw_it_go", value = TRUE)
    })
    
    # ---- You Draw It Question Flow -------------------------------------------
    
    
    
# ------------------------------------------------------------------------------
# STUDY 3: ESTIMATION ----------------------------------------------------------
# ------------------------------------------------------------------------------

    
    # ---- You Draw It Examples ------------------------------------------------
    
    output$estimation_text <- renderUI({
      HTML("The following examples illustrate the types of questions you may encounter during this experiment.")
    })
    
    # Start estimation study
    observeEvent(input$begin_estimation, {
      updateCheckboxInput(session, "estimation_go", value = TRUE)
    })
    
    # ---- Estimation Question Flow -------------------------------------------
    
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

}) # end server
