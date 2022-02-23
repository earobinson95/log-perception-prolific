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

navbarPage("Perception of Statistical Graphics", inverse = TRUE, 
           theme = shinytheme("spacelab"),
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
           
           helpText(
             "Your response is voluntary and any information we collect from you will be kept confidential.",
             "Please read the informed consent document (click the button below) before you decide whether to participate."),
           
           a("Show Informed Consent Document", href = "informed_consent.html", target = "_blank"),
           
           checkboxInput("consent","I have read the informed consent document and agree.", width = "100%"),
           
           actionButton("beginexp", "Begin Experiment", class = "btn btn-info")
           
           ), # end informed consent tab
  
  # ----------------------------------------------------------------------------
  # DEMOGRAPHICS ---------------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Demographics"
           
           ), # end demographics tab
  
  # ----------------------------------------------------------------------------
  # STUDY 1: LINEUPS -----------------------------------------------------------
  # ----------------------------------------------------------------------------
  
  tabPanel("Study 1: Lineups"
           
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
