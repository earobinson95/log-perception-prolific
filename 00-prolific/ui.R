library(shiny)
library(shinyjs)
library(shinythemes)

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

navbarPage("Perception of Statistical Graphics", inverse = TRUE, theme = shinytheme("spacelab"),
           
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
           
           fluidRow(
             column(id = "lineups_sidebar", width = 3,
                    # div(
                    # id = "one", class = "left",
                    # This panel shows if the experiment was chosen and informed consent hasn't been given
                    conditionalPanel(
                      condition = "!input.welcome",
                      h4("Study 1: Lineups"),
                      helpText(
                        "In this survey a series of similar looking charts will be presented.",
                        "We would like you to respond to the following questions."),
                      helpText("1. Pick the plot based on the survey question"),
                      helpText("2. Provide reasons for choice"),
                      helpText("3. How certain are you?"),
                      
                      actionButton("beginexp", "Begin Study 1", class = "btn btn-info")
                      
                    ) # end condition
                    
             ), # end sidebar panel column
             
             column(id = "lineups_main", width = 9,
                    conditionalPanel(condition = "!input.welcome",
                                     h4(textOutput("welcome_header")),
                                     uiOutput("welcome_text"),
                                     
                                     h4(textOutput("example1_q")),
                                     div(
                                       class = "ex-lineup-container",
                                       imageOutput("example1_plot", height = "auto")
                                     ),
                                     br(),
                                     br(),
                                     uiOutput("example1_a"),
                                     
                                     h4(textOutput("example2_q")),
                                     div(
                                       class = "ex-lineup-container",
                                       imageOutput("example2_plot", height = "auto")
                                     ),
                                     br(),
                                     br(),
                                     uiOutput("example2_a")
                    ),
                    conditionalPanel(condition = "input.welcome && !input.ready",
                                     h4(textOutput("demo_text"))
                    )
             ) # end main column
             
           ) # end fluid row
           
           ), # end lineups tab
  
  # ----------------------------------------------------------------------------
  # STUDY 2: YOU DRAW IT -------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Study 2: You Draw It"
           
           ), # end you draw it tab
  
  # ----------------------------------------------------------------------------
  # STUDY 3: ESTIMATION --------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Study 3: Estimation"
           
           ), # end estimation tab
  
  # ----------------------------------------------------------------------------
  # COMPLETE STUDY -------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Done"
           
           ) # end done tab
  
  # ----------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
) # end navbar page
