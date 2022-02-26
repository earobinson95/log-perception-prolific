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

# connect to database
con <- dbConnect(sqlite.driver, dbname = "databases/03_estimation_data.db")

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
    
    # Move to Lineup Study Tab
    observeEvent(input$begin_lineups, {
      updateCheckboxInput(session, "lineups_go", value = TRUE)
    })
    
    
    # LINEUP QUESTION FLOW -----------------------------------------------------
    
    output$lineups_action_buttons <- renderUI({
      # actionButton("lineups_submit", "Submit", icon = icon("caret-right"), class = "btn btn-info"),
      actionButton("lineups_complete", "Continue", icon = icon("caret-right"), class = "btn btn-info")
    })
    
    
    # Move to You Draw It Study 
    observeEvent(input$lineups_complete, {
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
    })
    
    # ---- You Draw It Question Flow -------------------------------------------
    
    output$you_draw_it_action_buttons <- renderUI({
        actionButton("you_draw_it_study_complete", "Continue", icon = icon("caret-right"), class = "btn btn-info")
    })
    
    
    
    
    # move to estimation study
    observeEvent(input$you_draw_it_study_complete, {
      updateTabsetPanel(session, "inNavBar",selected = "study3-estimation-tab")
    })
    
    
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
    observe({
      if (is.null(input$response) || input$response == "") {
        enable("estimation_submit")
      }
    })
    
    # Saves responses to feedback database
    observeEvent(input$estimation_submit, {
      
      if (estimation_values$qleft > 0 &&
          (!is.null(input$question_text) && input$question_text != "" && !is.na(input$question_text)) &&
          estimation_values$q_id != "scenario" &&
          !any(input$dimension < window_dim_min)) {
        
        # Things to do when estimate is given and submitted
        # disable("submit")
        
        estimation_values$result <- "Submitted!"
        
        response_data <-    tibble(ip_address      = input$ipid,
                                   nick_name       = input$nickname,
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
        con <- dbConnect(sqlite.driver, dbname = "databases/03_estimation_data.db")
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
          updateCheckboxInput(session, "done", value = TRUE)
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
      calc_data <-    tibble(ip_address      = input$ipid,
                             nick_name       = input$nickname,
                             study_starttime = study_starttime,
                             q_id            = estimation_values$q_id,
                             creature        = estimation_values$creature_name,
                             dataset         = estimation_values$dataset_id,
                             scale           = estimation_values$scale,
                             expression      = input$calc,
                             evaluated       = calculationVals())
      
      # Write results to database
      con <- dbConnect(sqlite.driver, dbname = "databases/03_estimation_data.db")
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
            
            column(width = 3,
                   actionButton("calcEval", "Evaluate")),
            
          )
        }
      }
    })
    
    calculationVals <- eventReactive(input$calcEval, {
      
      if(!is.null(input$calc) && input$calc != "" & is.numeric(eval(parse(text=input$calc)))) {
        eval(parse(text=input$calc)) %>% as.character()
      } else {
        "Please enter in a valid expression."
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
      
        # move to done page
        updateTabsetPanel(session, "inNavBar",selected = "done-tab")
      
    })
    
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

}) # end server
