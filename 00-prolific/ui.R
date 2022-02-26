# prolific ui

# ------------------------------------------------------------------------------
# LOAD LIBRARIES ---------------------------------------------------------------
# ------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinythemes)
library(r2d3)

# ------------------------------------------------------------------------------
# GRAB USER ID INFO-------------------------------------------------------------
# ------------------------------------------------------------------------------

inputUserid <- function(inputId, value='') {
  #   print(paste(inputId, "=", value))
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
  )
}

inputIp <- function(inputId, value=''){
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/action.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/tinycolor.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
  )
}

inputBrowserDims <- tags$head(
  tags$script('
    var dimension = [0, 0];
    $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.setInputValue("dimension", dimension);
    });
    $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.setInputValue("dimension", dimension);
    });
  '))

# ------------------------------------------------------------------------------
# BEGIN NAVPAR PAGE ------------------------------------------------------------
# ------------------------------------------------------------------------------

navbarPage("Perception of Statistical Graphics", id = "inNavBar", inverse = TRUE, theme = shinytheme("spacelab"),
           
  tags$style(type = "text/css",
             # Define the col widths
             ".left {width:25%;height:100%;padding:10px;}",
             ".right {width:75%;height:100%;padding:10px;}",
             # Define the column position and background
             "#one {float:left; background-color:#fbfbfb;color:#555555;}",
             "#two {float:right;background-color:#ffffff;}",
             # Fix text highlighting
             ".left .checkbox span {color: #555555;}",
             ".left label {color: #555555;}",
             # Scale image to 80% of browser window height
             ".full-lineup-container img {object-fit:contain; height:80vh; max-width:100%;}",
             ".ex-lineup-container img {object-fit:contain; height:20vh;}",
             ".full-lineup-container svg {object-fit:contain; height:80vh; max-width:100%; padding:0; margin:0;}",
             ".ex-lineup-container svg {object-fit:contain; height:20vh;}"),
  inputBrowserDims,
  useShinyjs(),
  
  # ----------------------------------------------------------------------------
  # INFORMED CONSENT -----------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Informed Consent",
           value = "informed-consent-tab",
           
    fluidRow(
      column(id = "informed_consent_sidebar", width = 3,

                helpText("Your response is voluntary and any information we collect from you will be kept confidential.",
                         "Please read the informed consent document (click the button below) before you decide whether to participate."),
                      
                a("Show Informed Consent Document", href = "informed_consent.html", target = "_blank"),
                      
                checkboxInput("consent","I have read the informed consent document and agree.", width = "100%"),
                    
                actionButton("beginexp", "Begin Experiment", class = "btn btn-info")
                    
      ), # end informed consent sidebar panel column
             
      column(id = "informed_consent_main", width = 9,

            h4("Welcome to a Survey on Graphical Inference"),
            HTML("This web site is designed to conduct a series of three surveys on graphical inference which will help us understand human perception of graphics for use in communicating statistics. <br><br>
                 In this study, you will guided through each of the three studies:<br><br>
                 1. examines the ability to differentiate curves shown on charts and graphs (12 questions; ~ 15 minutes to complete).<br><br>
                 2. examines the ability to predict data by using your mouse to draw a trend line (12 questions; ~ 15 minutes to complete).<br><br>
                 3. examines graph comprehension of different types of charts and graphs (12 questions; ~ 15 minutes to complete).<br><br>
                 Finally we would like to collect some information about you (age category, education, and gender)."
                 )
            
      ) # end informed consent main column
      
    ) # end informed consent fluid row
  ), # end informed consent tab
  
  # ----------------------------------------------------------------------------
  # DEMOGRAPHICS ---------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Demographics",
           value = "demographics-tab",
           
    fluidRow(
      column(id = "informed_consent_sidebar", width = 3,
             
          h4("Demographic Information"),
          inputIp("ipid"),
          inputUserid("nickname"),
          
          textInput("prolificID", "Prolific ID", ""),
          
          selectizeInput("age", "Age Range",
                          choices = c("", 
                                      "Under 19",
                                      "19-25", 
                                      "26-30", 
                                      "31-35", 
                                      "36-40", 
                                      "41-45", 
                                      "46-50", 
                                      "51-55", 
                                      "56-60", 
                                      "Over 60", 
                                      "Prefer not to answer")),
          
          radioButtons("gender", "Gender Identity",
                          choices = c("Female", 
                                      "Male", 
                                      "Variant/Nonconforming", 
                                      "Prefer not to answer"),
                          selected = NA),
          
          selectizeInput("education", "Highest Education Level",
                         choices = c("", "High School or Less",
                                        "Some Undergraduate Courses",
                                        "Undergraduate Degree",
                                        "Some Graduate Courses",
                                        "Graduate Degree",
                                        "Prefer not to answer")),
          
          selectizeInput("recruitment",
                         "How were you recruited for participation in this study?",
                         choices = c("", 
                                     "Prolific",
                                     "Reddit",
                                     "Graphics Group",
                                     "Direct Email",
                                     "I am the researcher",
                                     "Other")),
             
          actionButton("submitdemo", "Submit Demographics", class = "btn btn-info")
             
      ), # end demographics sidebar column
      
      column(id = "demographics_main", width = 9,
             
             helpText(h4("Please fill out the demographic information to begin."))
             
      ) # end demographics main column
      
    ) # end demographics fluid row
  ), # end demographics tab
  
  # ----------------------------------------------------------------------------
  # STUDY 1: LINEUPS -----------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Study 1: Lineups",
           value = "study1-lineups-tab",
           
           fluidRow(
             column(id = "lineups_sidebar", width = 3,
                    
                    # create indicator for starting lineups within this tab
                    hidden(
                      checkboxInput("lineups_go", "Start Lineup Study", value = FALSE)
                    ),
                    
                    # This panel shows if the participant has not hit start for the lineup study
                    conditionalPanel(condition = "!input.lineups_go",
                      
                        h4("Study 1: Lineups"),
                        helpText("In this survey a series of similar looking charts will be presented.",
                                 "We would like you to respond to the following questions."),
                        helpText("1. Pick the plot based on the survey question"),
                        helpText("2. Provide reasons for choice"),
                        helpText("3. How certain are you?"),
                        br(),
                        actionButton("begin_lineups", "Begin Study 1", class = "btn btn-info")
                      
                    ), # end lineup example condition (sidebar)
                    
                    conditionalPanel(condition = "input.lineups_go",
                                     
                        h4("Selection"),
                        textInput("response_no", "Choice", value = "", placeholder = "Click the plot to select"),
                        # selectizeInput("response_no", "Choice", choices = 1:20, selected = NULL, multiple = T),
                                     
                        # Handle other reasoning logic
                        conditionalPanel(condition = "!input.otheronly",
                                         checkboxGroupInput("reasoning", "Reasoning", choices = "")
                        ),
                        conditionalPanel(condition = "input.reasoning.indexOf('Other') > -1 || input.otheronly",
                                         textInput("other", "Other Reason")
                        ),
                                     
                        selectizeInput("certain", "How certain are you?",
                                       choices = c("", "Very Uncertain", "Uncertain",
                                                   "Neutral", "Certain", "Very Certain")),
                        actionButton("submit", "Submit", icon = icon("caret-right"), class = "btn btn-info"),
                        hr(),
                        h4("Status"),
                        h5(textOutput("lineup_status"))
                        
                    ) # end lineup question flow condition (sidebar)
                    
             ), # end sidebar panel column
             
             column(id = "lineups_main", width = 9,
                    
                    # this is the lineup study example page
                    conditionalPanel(condition = "!input.lineups_go",
                                     
                        h4(textOutput("lineups_header")),
                        uiOutput("lineups_text"),
                                     
                        h4(textOutput("lineups_example1_q")),
                        div(
                            class = "ex-lineup-container",
                            imageOutput("lineups_example1_plot", height = "auto")
                        ),
                        br(),
                        uiOutput("example1_a"),
                                     
                        h4(textOutput("lineups_example2_q")),
                        div(
                            class = "ex-lineup-container",
                            imageOutput("lineups_example2_plot", height = "auto")
                        ),
                        br(),
                        uiOutput("lineups_example2_a")
                                     
                    ), # end lineup example condition (main)
                    
                    # This is the question flow
                    conditionalPanel(condition = "input.lineups_go",
                                     
                        h4("Lineup plot goes here.")
                                     
                    ) # end lineup question flow condition (main)
                    
      ), # end main column
    
      # Javascript action script for lineups -- may not be necessary
      includeScript("www/js/action.js")
             
    ) # end fluid row
           
  ), # end lineups tab
  
  # ----------------------------------------------------------------------------
  # STUDY 2: YOU DRAW IT -------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Study 2: You Draw It",
           value = "study2-you-draw-it-tab",
           
           fluidRow(
             column(id = "you_draw_it_sidebar", width = 3,
                    
                    # create indicator for starting lineups within this tab
                    hidden(
                      checkboxInput("you_draw_it_go", "Start You Draw It Study", value = FALSE)
                    ),
                    
                    # This panel shows if the participant has not hit start for the lineup study
                    conditionalPanel(condition = "!input.you_draw_it_go",
                                     
                                     h4("Study 2: You Draw It"),
                                     
                                     actionButton("begin_you_draw_it", "Begin Study 2", class = "btn btn-info")
                                     
                    ), # end lineup example condition (sidebar)
                    
                    conditionalPanel(condition = "input.you_draw_it_go",
                                     
                                     h4("You Draw It Sidebar question flow.")
                                     
                                     
                    ) # end you draw it question flow condition (sidebar)
                    
             ), # end sidebar panel column
             
             column(id = "you_draw_it_main", width = 9,
                    
                    # this is the you draw it study example page
                    conditionalPanel(condition = "!input.you_draw_it_go",
                        
                                     h4("You Draw it examples go here.")             
                             
                    ), # end lineup example condition (main)
                    
                    # This is the question flow
                    conditionalPanel(condition = "input.you_draw_it_go",
                          
                                     h4("You Draw It Study Flow")
                                     
                    ) # end you draw it question flow condition (main)
                    
      ) # end you draw it main column
           
    ) # end you draw it fluid row
  
  ), # end you draw it tab
  
  # ----------------------------------------------------------------------------
  # STUDY 3: ESTIMATION --------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Study 3: Estimation",
           value = "estimation-tab",
           
           fluidRow(
             column(id = "estimation_sidebar", width = 3,
                    
                    # create indicator for starting lineups within this tab
                    hidden(
                      checkboxInput("estimation_go", "Start You Draw It Study", value = FALSE)
                    ),
                    
                    # This panel shows if the participant has not hit start for the lineup study
                    conditionalPanel(condition = "!input.estimation_go",
                                     
                                     h4("Study 3: Estimation"),
                                     
                                     actionButton("begin_estimation", "Begin Study 3", class = "btn btn-info")
                                     
                    ), # end lineup example condition (sidebar)
                    
                    conditionalPanel(condition = "input.estimation_go",
                                     
                                     h4("Estimation Sidebar question flow.")
                                     
                                     
                    ) # end you draw it question flow condition (sidebar)
                    
             ), # end sidebar panel column
             
             column(id = "estimation_main", width = 9,
                    
                    # this is the you draw it study example page
                    conditionalPanel(condition = "!input.estimation_go",
                                     
                                     h4("Estimation examples go here.")             
                                     
                    ), # end lineup example condition (main)
                    
                    # This is the question flow
                    conditionalPanel(condition = "input.estimation_go",
                                     
                                     h4("Estimation Study Flow")
                                     
                    ) # end you draw it question flow condition (main)
                    
             ) # end you draw it main column
             
           ) # end you draw it fluid row
           
           
           ), # end estimation tab
  
  # ----------------------------------------------------------------------------
  # COMPLETE STUDY -------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Done",
           value = "done-tab",
           
           h5("Thank you for participating in our studies. Copy paste the Prolific completion code:"),
           br(),
           h4("52BD9173")
           ) # end done tab
  
  # ----------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
) # end navbar page
