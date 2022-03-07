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
library(RSQLite)
library(DBI)
sqlite.driver <- dbDriver("SQLite")

# ------------------------------------------------------------------------------
# OVERALL SET UP ---------------------------------------------------------------
# ------------------------------------------------------------------------------

window_dim_min <- 600

# add resource paths so Shiny can see them
addResourcePath("plots", "plots")
addResourcePath("examples", "examples")

# ------------------------------------------------------------------------------
# SETUP STUDY 1: LINEUPS -------------------------------------------------------
# ------------------------------------------------------------------------------

lineups_experiment_name <- "emily-log-1"

# define folders
plots_folder <- "plots" # subfolders for data, pdf, png, svg. picture_details.csv in this folder
trials_folder <- "trials" # subfolders for svg. picture_details_trial.csv in this folder

con <- dbConnect(SQLite(), dbname = "databases/01_lineups_db.db")
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

con <- dbConnect(sqlite.driver, dbname = "databases/02_you_draw_it_db.db")
you_draw_it_experiment <- dbReadTable(con, "experiment_details")
dbDisconnect(con)

# ------------------------------------------------------------------------------
# SETUP STUDY 3: ESTIMATION ----------------------------------------------------
# ------------------------------------------------------------------------------

options(shiny.sanitize.errors = TRUE)

#' validate calculator input
#' 
#' validate calculator: textInput
#' 
#' @param x input
#' @param pattern that input has to match (regexp)
#' @param many TRUE if more than 1 string (checkboxGroupInput)
#' @return validated input
#' @export
#' @examples
#' \dontrun{validinp_character(input$txt)}
#' \dontrun{validinp_character(input$radiobox, pattern="^((ab)|(cd))$")}
#' \dontrun{validinp_character(input$chkboxgrp, many=TRUE}
validinp_calculator <- function(x, pattern="^[[:digit:]\\. _+-/* log\\(\\)]*$", many=FALSE) {
  if(many && is.null(x)) return(character(0))  ## hack for checkboxGroupInput
  if(!( is.character(x) && (many || length(x)==1) && 
        all(!is.na(x)) && all(grepl(pattern,x)) )) {
    stop("Invalid input from shiny UI")
  }
  x
}

# connect to database
con <- dbConnect(sqlite.driver, dbname = "databases/03_estimation_db.db")

estimation_experiment <- dbReadTable(con, "experiment_details")
estimation_simulated_data  <- dbReadTable(con,"simulated_data")
estimation_questions <- dbReadTable(con, "estimation_questions")
scenario_text_data <- dbReadTable(con, "scenario_text_data")

dbDisconnect(con)

# ------------------------------------------------------------------------------
# BEGIN SERVER -----------------------------------------------------------------
# ------------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  shinyjs::disable(selector = '.navbar-nav a')
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
      con <- dbConnect(SQLite(), dbname = "databases/00_demographics_db.db")
      
      age <- ifelse(is.null(input$age), "", input$age)
      gender <- ifelse(is.null(input$gender), "", input$gender)
      academic_study <- ifelse(is.null(input$education), "", input$education)
      
      demoinfo <- data.frame(nick_name = input$nickname,
                             ip_address = input$ipid,
                             study_starttime = study_starttime,
                             prolific_id = as.character(input$prolificID),
                             age = age,
                             gender = gender,
                             academic_study = academic_study,
                             recruitment = input$recruitment
                             )
      
      dbWriteTable(con, "users", demoinfo, append = TRUE, row.names = FALSE)
      dbDisconnect(con)
      
      # mark demographics complete
      updateCheckboxInput(session, "demographics_done", value = TRUE)
      
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
    
    # Move to Lineup Study Tab
    observeEvent(input$begin_lineups, {
      updateCheckboxInput(session, "lineups_go", value = TRUE)
    })
    
    
    # LINEUP QUESTION FLOW -----------------------------------------------------
    
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
      pic_id = 0,
      param_value = "",
      set = NA,
      scale = "",
      choice = NULL,
      order = NA,
      correct = NULL, 
      result = "")
    
    # Provide experiment-appropriate reasoning boxes
    output$lineups_sidebar_buttons1 <- renderUI({
      if(lineup_values$lppleft > 0) {
        tagList(
            h4("Selection"),
            textInput("response_no", "Choice", value = "", placeholder = "Click the plot to select"),
            checkboxGroupInput("reasoning", "Reasoning", choices = lineup_values$reasons, selected = NA)
        )
      }
      })
    output$lineups_sidebar_buttons2 <- renderUI({
      if(lineup_values$lppleft > 0 && "Other" %in% input$reasoning){
        textInput("other", "Other Reason", value = "")
      }
    })
    output$lineups_sidebar_buttons3 <- renderUI({
      if(lineup_values$lppleft > 0) {
            tagList(
            selectizeInput("certain", "How certain are you?",
                           choices = c("", "Very Uncertain", "Uncertain",
                                       "Neutral", "Certain", "Very Certain")),
            br(),
            actionButton("lineups_submit", "Submit", icon = icon("caret-right"), class = "btn btn-info") ,
            hr(),
            h4("Status")
        )
      } else {
      actionButton("lineups_complete", "Continue", icon = icon("caret-right"), class = "btn btn-info")
      }    
    })
    
    output$lineups_question <- renderText({
      return(lineup_values$question)
    })
    
    # Output info on how many trials/lineups left
    output$lineups_status <- renderText({
      
      if(lineup_values$lppleft > 0) {
        paste(
          ifelse(lineup_values$trialsleft > 0, "Trial", ""),
          "Plot",
          ifelse(lineup_values$trialsleft > 0,
                 paste(lineup_values$trialsreq - lineup_values$trialsleft + 1, "of", lineup_values$trialsreq),
                 paste(lineup_values$lpp - lineup_values$lppleft + 1, "of", lineup_values$lpp)))
      }
    })
    
    # Enable submit button if the experiment progresses to ___ stage
    observe({
      if (is.null(input$response_no) || input$response_no == "") {
        enable("lineups_submit")
      }
    })
    
    observeEvent(input$lineups_submit, {
      response <- as.character(input$response_no)
      
      
      if (nchar(response) > 0 &&
          all(strsplit(response, ",")[[1]] %in% 1:20) &&
          lineup_values$lppleft > 0 &&
          (length(input$reasoning) > 0) &&
          nchar(input$certain) > 0 &&
          !any(input$dimension < window_dim_min)) {
        
        # Things to do when responses are all filled in and submitted
        disable("lineups_submit")
        
        reason <- input$reasoning
        if ("Other" %in% reason) {
          reason <- c(reason, input$other)
        }
        reason <- paste(reason, collapse = ", ")
        
        lineup_values$choice <- response
        
        if (lineup_values$trialsleft == 0 && lineup_values$lppleft > 0) {
          # This applies to the lineups, not to the trials
          lineup_values$result <- "Submitted!"
          
          lineup_feedback <- data.frame(ip_address      = input$ipid, 
                                        nick_name       = input$nickname,
                                        study_starttime = study_starttime,
                                        prolific_id     = input$prolificID %>% as.character(),
                                        order           = lineup_values$order,
                                        start_time      = lineup_values$starttime, 
                                        end_time        = now(),
                                        pic_id          = lineup_values$pic_id,
                                        param_value     = lineup_values$param_value,
                                        set             = lineup_values$set,
                                        test_param      = lineup_values$scale,
                                        correct         = lineup_values$correct,
                                        response_no     = lineup_values$choice,
                                        conf_level      = input$certain,
                                        choice_reason   = reason
                                        )
          
          # Write results to database
          con <- dbConnect(SQLite(), dbname = "databases/01_lineups_db.db")
          dbWriteTable(con, "feedback", lineup_feedback, append = TRUE, row.names = FALSE)
          dbDisconnect(con)
          
          # Update variables for next trial
          lineup_values$lppleft <- lineup_values$lppleft - 1
          lineup_values$choice <- ""
          
          # Generate completion code
          if (lineup_values$lppleft == 0) {
            lineup_values$question <- "Study 1 Complete! Hit continue to move to the next study."
          }
          
        } else {
          # This applies to the trials, not the lineups
          if (any(strsplit(lineup_values$choice, ",")[[1]] %in% lineup_values$correct)) {
            lineup_values$trialsleft <- lineup_values$trialsleft - 1
            lineup_values$result <- "Correct! :)"
          } else {
            lineup_values$result <- "Incorrect :("
          }
        }
        
        lineup_values$submitted <- TRUE
      } else {
        # Don't let them move ahead without doing the trial
        showNotification("Please fill in all of the boxes.")
      }
    })
    
    # This renders the trial/lineup image
    output$lineup <- renderUI({
      if (lineup_values$lppleft == 0 || any(input$dimension < window_dim_min)) return(NULL)
      
      withProgress(
        # Message: Loading (trial) plot i of n
        message = paste(lineup_values$result, "Loading",
                        ifelse(lineup_values$trialsleft > 0, "trial", ""), "plot",
                        ifelse(lineup_values$trialsleft > 0,
                               paste(lineup_values$trialsreq - lineup_values$trialsleft + 1, "of", lineup_values$trialsreq),
                               paste(lineup_values$lpp - lineup_values$lppleft + 1, "of", lineup_values$lpp))),
        expr = {
          lineup_values$submitted
          lineup_values$starttime <- now()
          
          this_picture <- picture_details_order[lineup_values$lpp - lineup_values$lppleft + 1, ]
          
          # Update reactive values
          lineup_values$pic_id       <- this_picture$pic_id
          lineup_values$order        <- this_picture$order
          lineup_values$param_value  <- this_picture$param_value
          lineup_values$set          <- this_picture$set
          lineup_values$scale        <- this_picture$test_param
          lineup_values$correct      <- this_picture$obs_plot_location
          
          # Reset UI selections
          lineup_values$submitted <- FALSE
          
          updateSelectizeInput(
            session, "certain",
            choices = c("", "Very Uncertain", "Uncertain", "Neutral", "Certain", "Very Certain"),
            selected = NULL)
          updateTextInput(session, "response_no", value = "")
          updateTextInput(session, "other", value = "")
          updateCheckboxGroupInput(session, "reasoning", selected = "")
          
          # Read svg and remove width/height
          tmp <- readLines(file.path(this_picture$pic_name))
          tmp[2] <- str_replace(tmp[2], "width=.*? height=.*? viewBox", "viewBox")
          
          # Include the picture
          div(
            class="full-lineup-container",
            HTML(tmp)
          )
          
        }) # end WithProgress
      
    }) # end renderUI
    
    
    observeEvent(input$lineups_complete, {
      # mark lineups as done
      updateCheckboxInput(session, "lineups_done", value = TRUE)
      
      # Move to You Draw It Study 
      updateTabsetPanel(session, "inNavBar", selected = "study2-you-draw-it-tab")
    })
    
    
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
      
      con <- dbConnect(sqlite.driver, dbname = "databases/02_you_draw_it_db.db")
      
        simulated_data_db <- simulated_data %>%
          unnest(data) %>%
          dplyr::select(parm_id, dataset, x, y) %>%
          mutate(ip_address = input$ipid,
                 nick_name = input$nickname,
                 study_starttime = study_starttime,
                 prolific_id = input$prolificID %>% as.character(),
                 parm_id = as.character(parm_id)
          )
        
        dbWriteTable(con, "simulated_data", simulated_data_db, append = TRUE, row.names = FALSE)
      
      dbDisconnect(con)
      
    })
    
    # ---- You Draw It Question Flow -------------------------------------------
    
    output$you_draw_it_action_buttons <- renderUI({
      
      if (you_draw_it_values$ydippleft > 0) {
        tagList(
          actionButton("reset", "Reset"),
          br(),
          br(),
          actionButton("you_draw_it_submit", "Submit", icon = icon("caret-right"), class = "btn btn-info"),
          hr(),
          h4("Status"),
          h5(textOutput("you_draw_it_status"))
        )
      } else {
        actionButton("you_draw_it_study_complete", "Continue", icon = icon("caret-right"), class = "btn btn-info")
      }
    })
    
    output$you_draw_it_question <- renderText({
      return(you_draw_it_values$question)
    })
    
    output$isPractice <- reactive({
      you_draw_it_values$practiceleft > 0
    })
    outputOptions(output, 'isPractice', suspendWhenHidden = FALSE)
    
    output$you_draw_it_practicetext <- renderText({
      return(you_draw_it_values$practicetext)
    })
    
    output$you_draw_it_practicegif <- renderImage({
      # Return a list
      list(src = you_draw_it_values$practicegif_file,
           alt = "",
           width = 350)
    }, deleteFile = FALSE)
    
    # Output info on how many practices/you draw it tasks left
    output$you_draw_it_status <- renderText({
      paste(
        ifelse(you_draw_it_values$practiceleft > 0, "Practice", ""),
        "Plot",
        ifelse(you_draw_it_values$practiceleft > 0,
               paste(you_draw_it_values$practicereq - you_draw_it_values$practiceleft + 1, "of", you_draw_it_values$practicereq),
               paste(you_draw_it_values$ydipp - you_draw_it_values$ydippleft + 1, "of", you_draw_it_values$ydipp)))
    })
    
    # Enable submit button if the experiment progresses to ___ stage
    observe({
      if (!(you_draw_it_values$done_drawing)) {
        enable("you_draw_it_submit")
      }
    })
    
    observeEvent(input$you_draw_it_submit, {
      
      if (
        you_draw_it_values$ydippleft > 0 &&
        you_draw_it_values$done_drawing &&
        !any(input$dimension < window_dim_min)) {
        
        # Things to do when you draw it line is finished and submitted
        disable("you_draw_it_submit")
        
        if (you_draw_it_values$practiceleft == 0 && you_draw_it_values$ydippleft > 0) {
          # This applies to the you draw it plots, not to the practices
          you_draw_it_values$result <- "Submitted!"
          
          test <- drawn_data() %>%
            mutate(nick_name       = input$nickname,
                   ip_address      = input$ipid,
                   prolific_id     = input$prolificID %>% as.character(),
                   study_starttime = study_starttime,
                   start_time      = you_draw_it_values$starttime,
                   end_time        = now(),
                   parm_id         = as.character(parm_id)
            )
          
          # Write results to database
          con <- dbConnect(sqlite.driver, dbname = "databases/02_you_draw_it_db.db")
          dbWriteTable(con, "feedback", test, append = TRUE, row.names = FALSE)
          dbDisconnect(con)
          
          # Update variables for next trial
          you_draw_it_values$ydippleft <- you_draw_it_values$ydippleft - 1
          you_draw_it_values$choice <- ""
          
          # Generate completion code
          if (you_draw_it_values$ydippleft == 0) {
            
            you_draw_it_values$question <- "Study 2 Complete! Hit continue to move to the next study."
            #you_draw_it_values$question <- paste("All done! Congratulations! Please click the URL to complete the study:")
            # updateCheckboxInput(session, "done", value = TRUE)
          }
          
        } else {
          # This applies to the practices, not the you draw it's
          you_draw_it_values$practiceleft <- you_draw_it_values$practiceleft - 1
        }
        
        you_draw_it_values$submitted <- TRUE
      } else {
        # Don't let them move ahead without doing the trial
        showNotification("Please finish drawing the trend for the entire yellow box region.")
      }
    })
    
    # Create storage for response values
    message_loc <- session$ns("drawr_message")
    drawn_data <- shiny::reactiveVal()
    line_data_storage <- shiny::reactiveVal()
    done_drawing  <- shiny::reactiveVal()
    
    # This renders the you draw it graph
    output$shinydrawr <- r2d3::renderD3({
      if (you_draw_it_values$ydippleft == 0 || any(input$dimension < window_dim_min)) return(NULL)
      input$reset
      
      withProgress(
        # Message: Loading (trial) plot i of n
        message = paste(you_draw_it_values$result, "Loading",
                        ifelse(you_draw_it_values$practiceleft > 0, "trial", ""), "plot",
                        ifelse(you_draw_it_values$practiceleft > 0,
                               paste(you_draw_it_values$practicereq - you_draw_it_values$practiceleft + 1, "of", you_draw_it_values$practicereq),
                               paste(you_draw_it_values$ydipp - you_draw_it_values$ydippleft + 1, "of", you_draw_it_values$ydipp))),
        expr = {
          you_draw_it_values$submitted
          
          you_draw_it_values$starttime <- now()
          trial <- as.numeric(you_draw_it_values$practiceleft > 0)
          
          # Reset UI selections
          you_draw_it_values$submitted    <- FALSE
          you_draw_it_values$done_drawing <- FALSE
          
          # This part applies to only the practice rounds
          if(you_draw_it_values$practiceleft > 0){
            # Update reactive values
            practiceID <- (you_draw_it_values$practicereq - you_draw_it_values$practiceleft + 1)
            
            # Obtain Parameters & Data
            you_draw_it_values$practicetext  <- practice_text[practiceID] %>% as.character()
            you_draw_it_values$practicegif_file  <- practicegif_files[practiceID] %>% as.character()
            isLinear     <- practice_data[practiceID, "linear"] %>% as.character()
            isFreeDraw   <- practice_data[practiceID, "free_draw"] %>% as.logical()
            drawStart    <- practice_data[practiceID, "draw_start"] %>% as.numeric()
            showFinished <- practice_data[practiceID, "show_finished"] %>% as.logical()
            
            point_data <- practice_data[practiceID,] %>%
              unnest(data) %>%
              filter(dataset == "point_data")
            
            line_data <- practice_data[practiceID,] %>%
              unnest(data) %>%
              filter(dataset == "line_data")
            
            data <- list(point_data = point_data, line_data = line_data)
            
            # Set up ranges
            y_range <- c(min(data$point_data[,"y"]), max(max(data$point_data[,"y"]), max(data$line_data[,"y"]))) * c(0.5, 1.5)
            x_range <- c(0,20)
            
            # Include the you draw it graph
            drawr(data              = data,
                  aspect_ratio      = 1,
                  linear            = isLinear,
                  free_draw         = isFreeDraw,
                  points            = "full",
                  x_by              = 0.25,
                  draw_start        = drawStart,
                  # points_end        = 0.5,
                  show_finished     = showFinished,
                  shiny_message_loc = message_loc,
                  x_range           = x_range,
                  y_range           = y_range)
            
          } else {
            # This part applies to only the you draw it's, not the practice rounds
            # Update reactive values
            taskID <- (you_draw_it_values$ydipp - you_draw_it_values$ydippleft + 1)
            
            # Obtain Parameters & Data
            isLinear   <- simulated_data[taskID, "linear"] %>% as.character()
            isFreeDraw <- simulated_data[taskID, "free_draw"] %>% as.logical()
            drawStart  <- simulated_data[taskID, "draw_start"] %>% as.numeric()
            
            point_data <- simulated_data[taskID,] %>%
              unnest(data) %>%
              filter(dataset == "point_data")
            
            line_data <- simulated_data[taskID,] %>%
              unnest(data) %>%
              filter(dataset == "line_data")
            
            data <- list(point_data = point_data, line_data = line_data)
            
            if(isFreeDraw){
              
              # Store data for feedback later
              line_data %>%
                select(parm_id, linear, x, y) %>%
                line_data_storage()
              
              # Set up ranges
              y_range <- eyefitting_yrange * c(1.1, 1.1)
              x_range <- c(0,20)
              
            } else {
              
              # Store data for feedback later
              line_data %>%
                select(parm_id, linear, x, y) %>%
                filter(x >= drawStart) %>%
                line_data_storage()
              
              # Set up ranges
              y_range <- range(data$line_data[,"y"]) * c(0.5, 2)
              x_range <- range(data$line_data[,"x"])
            }
            
            # Include the you draw it graph
            drawr(data              = data,
                  aspect_ratio      = 1,
                  linear            = isLinear,
                  free_draw         = isFreeDraw,
                  points            = "full",
                  x_by              = 0.25,
                  draw_start        = drawStart,
                  # points_end        = 0.5,
                  show_finished     = FALSE,
                  shiny_message_loc = message_loc,
                  x_range           = x_range,
                  y_range           = y_range)
          }
        })
    }) # end renderD3
    
    shiny::observeEvent(input$drawr_message, {
      
      you_draw_it_values$done_drawing <- TRUE
      
      if(you_draw_it_values$practiceleft == 0){
        line_data <- line_data_storage()
        
        line_data %>%
          mutate(ydrawn = input$drawr_message) %>%
          select(parm_id, x, y, ydrawn, linear) %>%
          drawn_data()
      }
      
    })
    
    observeEvent(input$you_draw_it_study_complete, {
      # mark you draw it as done
      updateCheckboxInput(session, "you_draw_it_done", value = TRUE)
      # move to estimation study
      updateTabsetPanel(session, "inNavBar",selected = "study3-estimation-tab")
    })
    
    
# ------------------------------------------------------------------------------
# STUDY 3: ESTIMATION ----------------------------------------------------------
# ------------------------------------------------------------------------------

    
    # ---- Estimation Examples -------------------------------------------------
    
    output$estimation_text <- renderUI({
      HTML("The following examples illustrate the types of questions you may encounter during this experiment.")
    })
    
    # Start estimation study
    observeEvent(input$begin_estimation, {
      updateCheckboxInput(session, "estimation_go", value = TRUE)
    })
    
    # ---- Estimation Prep -----------------------------------------------------
    
    # Randomization --------------------------------------------------------------
    # This needs to be run every connection, not just once.
    
    estimation_rand <- tibble(
      dataset = sample(c("dataset1", "dataset2"), 2, replace = F),
      creature = sample(unique(estimation_questions$qtext[estimation_questions$q_id == "scenario"]), 2, replace = F),
      scale    = sample(c("linear", "log2"), 2, replace = F)
    ) 
    
    estimation_randomization1 <- estimation_rand[1,] %>%
      expand_grid(q_id = c("scenario", "Q0", sample(c("QE1", "QE2", "QI1", "QI2", "QI3"), 5)))
    
    estimation_randomization2 <- estimation_rand[2,] %>%
      expand_grid(q_id = c("scenario", "Q0", sample(c("QE1", "QE2", "QI1", "QI2", "QI3"), 5)))
    
    estimation_randomization <- rbind(estimation_randomization1, estimation_randomization2) %>%
      left_join(estimation_questions, by = c("creature", "q_id"))
    
    estimation_simulated_data <- estimation_rand %>%
      right_join(estimation_simulated_data, by = "dataset") %>%
      mutate(date = ifelse(creature == "tribble", x + 1500, x - 3000))
    
    # reactive values to control the trials
    estimation_values <- reactiveValues(
      experiment = estimation_experiment$experiment,
      question   = "",
      scenario   = as.character(scenario_text_data[scenario_text_data$creature == as.character(estimation_randomization[1, "creature"]), "text"]),
      qreq       = estimation_experiment$num_qs,
      qleft      = estimation_experiment$num_qs,
      qcounter   = 1,
      trialsreq  = estimation_experiment$trialsreq,
      trialsleft = NA,
      
      submitted = FALSE,
      starttime = NULL,
      
      scale         = as.character(estimation_randomization[1, "scale"]),
      q_id          = as.character(estimation_randomization[1, "q_id"]),
      creature_name = as.character(estimation_randomization[1, "creature"]),
      dataset_id    = as.character(estimation_randomization[1, "dataset"])
    )
    
    # ---- Estimation Question Flow --------------------------------------------
    
    output$estimation_action_buttons <- renderUI({
      
       if (estimation_values$qcounter <= estimation_values$qreq) {
        
         tagList(
           actionButton("estimation_submit", "Submit", icon = icon("caret-right"), class = "btn btn-info"),
           hr(),
           h4("Status")
         )
        
      } else if (estimation_values$qcounter > estimation_values$qreq) {
        
        actionButton("estimation_study_complete", "Continue", icon = icon("caret-right"), class = "btn btn-info")
        
      }
      
    })
    
    # Enable submit button if the experiment progresses to ___ stage
    # observe({
    #   if (is.null(input$estimation_response) || input$estimation_response == "") {
    #     enable("estimation_submit")
    #   }
    # })
    
    # Saves responses to feedback database
    observeEvent(input$estimation_submit, {
      
      if (estimation_values$qleft > 0 &&
          (!is.null(input$question_text) && input$question_text != "" && !is.na(input$question_text)) &&
          estimation_values$q_id != "scenario" &&
          !any(input$dimension < window_dim_min)) {
        
        estimation_values$result <- "Submitted!"
        
        response_data <-    tibble(nick_name       = input$nickname,
                                   ip_address      = input$ipid,
                                   prolific_id     = input$prolificID %>% as.character(),
                                   study_starttime = study_starttime,
                                   start_time      = estimation_values$starttime,
                                   end_time        = now(),
                                   order           = estimation_values$qcounter,
                                   q_id            = estimation_values$q_id,
                                   creature        = estimation_values$creature_name,
                                   dataset         = estimation_values$dataset_id,
                                   scale           = estimation_values$scale,
                                   response        = input$question_text,
                                   scratchpad      = input$notes
        )
        
        # Write results to database
        con <- dbConnect(sqlite.driver, dbname = "databases/03_estimation_db.db")
        dbWriteTable(con, "feedback", response_data, append = TRUE, row.names = FALSE)
        dbDisconnect(con)
        
        # Update variables for next question
        estimation_values$qleft    = estimation_values$qleft - 1
        estimation_values$qcounter = estimation_values$qcounter + 1
        
        estimation_values$creature_name = as.character(estimation_randomization[estimation_values$qcounter, "creature"])
        estimation_values$dataset_id    = as.character(estimation_randomization[estimation_values$qcounter, "dataset"])
        estimation_values$scale         = as.character(estimation_randomization[estimation_values$qcounter, "scale"])
        estimation_values$q_id          = as.character(estimation_randomization[estimation_values$qcounter, "q_id"])
        estimation_values$scenario      = as.character(scenario_text_data[scenario_text_data == estimation_values$creature_name, "text"])
        
        if (estimation_values$qcounter > estimation_values$qreq) { # Generate completion code
          
          estimation_values$scenario <- "Study 3 Complete! Hit continue to recieve your Prolific confirmation code."
          #estimation_values$scenario <- paste("All done! Congratulations! Please click the URL to complete the study:")
          # updateCheckboxInput(session, "done", value = TRUE)
        }
        
        estimation_values$submitted <- TRUE
        
      } else if (estimation_values$q_id == "scenario") {
        
        # Update variables for next question
        estimation_values$qleft    = estimation_values$qleft - 1
        estimation_values$qcounter = estimation_values$qcounter + 1
        estimation_values$result   = NULL
        
        estimation_values$creature_name = as.character(estimation_randomization[estimation_values$qcounter, "creature"])
        estimation_values$scale         = as.character(estimation_randomization[estimation_values$qcounter, "scale"])
        estimation_values$q_id          = as.character(estimation_randomization[estimation_values$qcounter, "q_id"])
        estimation_values$scenario      = as.character(scenario_text_data[scenario_text_data == estimation_values$creature_name, "text"])
        
        estimation_values$submitted <- TRUE
        
      } else {
        # Don't let them move ahead without answering the question
        showNotification("Please provide an answer.")
      }
    })
    
    observeEvent(input$calcEval, {
      calc_data <-    tibble(nick_name       = input$nickname,
                             ip_address      = input$ipid,
                             prolific_id     = input$prolificID %>% as.character(),
                             study_starttime = study_starttime,
                             q_id            = estimation_values$q_id,
                             creature        = estimation_values$creature_name,
                             dataset         = estimation_values$dataset_id,
                             scale           = estimation_values$scale,
                             expression      = input$calc,
                             evaluated       = calculationVals())
      
      # Write results to database
      con <- dbConnect(sqlite.driver, dbname = "databases/03_estimation_db.db")
      dbWriteTable(con, "calc_feedback", calc_data, append = TRUE, row.names = FALSE)
      dbDisconnect(con)
    })
    
    # Output info on which question/page you're on
    output$estimation_status <- renderText({
      
      if (estimation_values$qcounter <= estimation_values$qreq) {
        
        paste("Question", estimation_values$qcounter, "of", estimation_values$qreq)
        
      }
      
    })
    
    # This renders the scenario text
    output$scenario_text <- renderUI({
      
      if (estimation_values$q_id == "scenario" || estimation_values$qcounter > estimation_values$qreq) {
        HTML(estimation_values$scenario)
      }
      
    })
    
    # This renders the question text
    output$question_textUI <- renderUI({
      input$estimation_submit
      
      estimation_values$starttime <- now()
      
      # Reset UI selections
      # estimation_values$submitted    <- FALSE
      
      
      if (estimation_values$qcounter <= estimation_values$qreq) {
      
      if (estimation_values$q_id == "Q0") {
        
        tagList(
          textInput("question_text",
                    estimation_randomization[estimation_values$qcounter, "qtext"],
                    value = ""),
          bsTooltip("question_text", "In words, provide a description of the population."),
          
        )
        
      } else if (estimation_values$q_id != "Q0" && estimation_values$q_id != "scenario") {
        
        tagList(
          numericInput("question_text",
                       estimation_randomization[estimation_values$qcounter, "qtext"],
                       value = ""),
          bsTooltip(id = "question_text", 
                    title = "Provide a numerical approximation.")
        )
        
      } else if (estimation_values$q_id == "scenario") {
        
        helpText(h5(paste("Hit 'Submit' to begin answering questions about the", 
                          str_to_title(as.character(estimation_randomization[estimation_values$qcounter, "creature"])),
                          "population.", sep = " "))
        )
      }
        
      }
      
    }) # End Render Question Text
    
    # This renders the plot
    output$data_plot <- renderPlot({
      
      # create base scatterplot
      
      basePlot <- estimation_simulated_data %>%
        filter(creature == estimation_values$creature_name) %>%
        ggplot(aes(x = date, y = y)) +
        geom_point() +
        theme_bw() +
        theme(aspect.ratio = 1,
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12)
        )
      
      if(estimation_values$creature_name == "tribble"){
        
        basePlot <- basePlot +
          scale_x_continuous("Stardate", expand = c(0.01,0.01))
        
      } else if (estimation_values$creature_name == "ewok"){
        
        basePlot <- basePlot +
          scale_x_continuous("ABY \n (After the Battle of Yavin)", expand = c(0.01,0.01))
        
      }
      
      if(estimation_values$scale == "linear"){
        
        finalPlot <- basePlot + 
          scale_y_continuous(paste(str_to_title(estimation_values$creature_name), "Population \n (Linear Scale)"),
                             limits = c(100, 55000),
                             breaks = seq(0, 55000, 5000),
                             labels = comma,
                             minor_breaks = c())
        
      } else if (estimation_values$scale == "log2"){
        
        finalPlot <- basePlot + 
          scale_y_continuous(paste(str_to_title(estimation_values$creature_name), "Population \n (Log Scale)"),
                             trans = "log2",
                             limits = c(100, 55000),
                             breaks = 2^seq(0,10000,1),
                             labels = comma,
                             minor_breaks = c())
        
      }
      
      finalPlot
      
    }) # End renderPlot
    
    # This outputs either plot or tribble/ewok image
    output$figure <- renderUI({
      
      if (estimation_values$q_id != "scenario" && estimation_values$qcounter <= estimation_values$qreq) {
        
        plotOutput("data_plot", height = "500px")
        
      } else if (estimation_values$qcounter > estimation_values$qreq) {
        imagepath <- "ewok-tribble.jpg"
        tags$figure(
          div(img(src = imagepath, width="60%"), style="text-align: center"),
          br(),
          tags$figcaption("Artwork modified from @allison_horst", style = "text-align: center; color: lightgray"))
      }
      
      else if (estimation_values$q_id == "scenario") {
        
        imagepath <- paste(estimation_values$creature_name, ".jpg", sep = "")
        tags$figure(
          div(img(src = imagepath, width="60%"), style="text-align: center"),
          br(),
          tags$figcaption("Artwork modified from @allison_horst", style = "text-align: center; color: lightgray"))
        
      }
      
    })
    
    output$simple_calculator <- renderUI({
      input$estimation_submit
      
      if (estimation_values$q_id != "scenario" && estimation_values$qcounter <= estimation_values$qreq) {
        if(estimation_values$q_id != "scenario" && estimation_values$q_id != "Q0") {
          
          tagList(
            
            helpText(h5("Below are resources for you to use as you are making numerical approximations.")),
            br(),
            column(width = 9,
                   textInput("calc",
                             "Basic Calculator (e.g. 2 + 2 = 4)",
                             value = "")),
            
            bsTooltip("calc", 
                      title = "Valid Calculator Inputs: + - / * log() log2() log10()"),
            
            column(width = 3,
                   actionButton("calcEval", "Evaluate")),
            
          )
        }
      }
    })
    
    calculationVals <- eventReactive(input$calcEval, {
      
      shiny::validate(
        need(try(calc_expression <- validinp_calculator(input$calc)), "Please provide a valid calculator expression. Valid inputs: + - / * log() log2() log10()")
      )
      
      shiny::validate(
        need(try(calc_evaluation <- eval(parse(text = input$calc))), "Please provide a valid calculator expression. Valid inputs: + - / * log() log2() log10()")
      )
      
      if(!is.null(input$calc) && input$calc != "" && is.numeric(calc_evaluation)) {
        eval(parse(text = input$calc)) %>% as.character() 
      }
      
    })
    
    output$calculation <- renderText({
      input$estimation_submit
      
      if (estimation_values$q_id != "scenario" && estimation_values$qcounter <= estimation_values$qreq) {
        calculationVals()
      }
    })
    
    output$notepad <- renderUI({
      input$estimation_submit
      
      if (estimation_values$q_id != "scenario" && estimation_values$qcounter <= estimation_values$qreq) {
        
        if(estimation_values$q_id != "scenario" && estimation_values$q_id != "Q0") {
          tagList(
            textAreaInput("notes", "Scratchpad", "Put scratch-work here...", width = "500px", height = "250px"),
            # actionButton("examplePopup", "Show Examples", onclick = "window.open('examples-popup.png')")
            actionButton("examplePopup", "Show Examples")
          )
        }
      }
    })
    
    observeEvent(input$examplePopup, {
      showModal(modalDialog(
        includeHTML("www/test.html"),
        easyClose = TRUE,
        size = "l"
      )
      )
    })
    
    observeEvent(input$estimation_study_complete, {
        
        # mark estimation study as done
        updateCheckboxInput(session, "estimation_done", value = TRUE)
      
        # move to done page
        updateTabsetPanel(session, "inNavBar",selected = "done-tab")
      
    })
    
    
# ------------------------------------------------------------------------------
# Done Page --------------------------------------------------------------------
# ------------------------------------------------------------------------------
    
    output$done_UI <- renderUI({
      
      if(input$demographics_done && input$lineups_done && input$you_draw_it_done && input$estimation_done){
        tagList(
          h5("Thank you for participating in our studies. Copy paste the Prolific completion code:"),
          br(),
          # h4("52BD9173")
          h4("######")
        ) 
      }
      
      if(!input$demographics_done){
        tagList(
          h5("Please return to complete your demographics."),
          actionButton("return_to_demographics", "Return to Demographics", class = "btn btn-info")
        ) 
      }
      
      if(!input$lineups_done){
        tagList(
          h5("Please return to complete your Study 1: Lineups."),
          actionButton("return_to_lineup_study", "Return to Study 3", class = "btn btn-info")
        )
      }
      
      if(!input$you_draw_it_done){
        tagList(
          h5("Please return to complete your Study 2: You Draw It."),
          actionButton("return_to_you_draw_it_study", "Return to Study 2", class = "btn btn-info")
        )
      }
      
      if(!input$estimation_done){
        tagList(
          h5("Please return to complete your Study 3: Estimation."),
          actionButton("return_to_estimation_study", "Return to Study 3", class = "btn btn-info")
        )
      }
      
    })

}) # end server
