#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),
  #### Load various .js files enabling interactivity ----
  useShinyjs(), # this is for enabling/disabling buttons from shinyjs
  useShinyFeedback(), # enabling/disabling feedback from shinyFeedback
  withAnim(), # enable animations from shinyanimate

  # showing icons for required items
  shiny::singleton(
    shiny::tags$head(
      shiny::tags$script(
        src = "js/toggleChecker.js"
      ),
      shiny::tags$script(
        src = "js/toggleCheckerColor.js"
      ),
      shiny::tags$script(
        src = "js/toggleSectionIcon.js"
      )
    )
  ),
  # hack to load font-awesome when Shiny loads
  tags$div(
    style = "display: none;",
    shiny::icon("user")
  ),
  
  
  #### Application outline ----
  # Application title
  headerPanel(column(12, "TRANSPARENCY CHECKLIST (short, 12 items)", align = "center"), windowTitle = "Transparency Checklist"),

  br(), br(), br(), br(), br(),
  
  fluidRow(column(1),
           column(10, tags$a("I prefer to fill out the full (36-item) checklist.", 
                             href="https://kucharssim.shinyapps.io/TransparencyChecklist/",
                             target="_blank")),
           column(1)
  ),
  
  # The header (basic information about the paper and authors)
  fluidRow(
    column(1),
    column(10,
      wellPanel(h4(textOutput("currentTime")), br(), headHTML)),#uiOutput("head"))),
    column(1)
  ),
  
  # Show initial instructions:
  fluidRow(
    column(1),
    column(10, 
           h3("Please select an answer for each item below. 
              If you want to elaborate on your answers, you can do so in the comment box that follows each section.")
           ),
    column(1)
  ),
  
  br(), br(),
  
  # Show questions
  #uiOutput("questions"),
  sectionsHTML,
  
  # Switching between sections
  fluidRow(column(2),
           column(2, actionButton("previousButton", "Go to previous section", icon = icon("arrow-circle-left"))),
           column(4),
           column(2, actionButton("nextButton", "Go to next section", icon = icon("arrow-circle-right"))),
           column(2)
  ),
  br(), br(),
  ##### Report menu (downloading) ----
  absolutePanel(
    dropdown(
      h4("Generate Report"),
      pickerInput(inputId = "save.as", label = "Format", 
                  choices = c("pdf", "html", "word", "rtf"), 
                  multiple = FALSE, width = 'auto', inline = FALSE),
      div(style = "display:inline-block",
        actionBttn(inputId = "preview", label = "Preview", icon = icon("eye"),
                   style = "simple",
                   color = "primary",
                   size = "xs",
                   no_outline = FALSE),# br(), br(), 
        actionBttn(inputId = "showcode", label = "Show code", icon = icon("code"),
                   style = "simple",
                   color = "primary",
                   size = "xs",
                   no_outline = FALSE)
        ), br(), br(),
      downloadButton('report', 'Download report', class = "downbutt"),
      
      icon = icon("file-alt"), up = TRUE, 
      tooltip = tooltipOptions(title = "Click here to create and download report", placement = "top"),
      style = "unite", label = "Report",
      size = "lg", inputId = "generatereport", width = "27vw", class = "fixedButton"),
   bottom = "2.5%", left = "2%", fixed = TRUE, width = "auto"),
  
  # Open window for a preview
  shinyBS::bsModal(id = "previewer", title = "Preview", trigger = "preview", size = "large",
                   uiOutput("generatePreview")),
  
  # Open window for a code
  shinyBS::bsModal(id = "codeshower", title = "Code", trigger = "showcode", size = "large",
                   verbatimTextOutput("code")),
  
  # Show tooltip which says that the download is not ready
  shinyBS::bsTooltip(id = "report",
                     title = "A report can be downloaded after all questions in each section have been answered.",
                     # Please, respond to all displayed items to download the pdf report (comments are optional).
                     trigger = "manual",
                     placement = "right"),
  uiOutput("trigger"), # this trigger displays or hides the explaining tooltip
  br(), br()
  
  
  # temporary (for debugging): showing the current status of the answers
  # ,br(),
  # verbatimTextOutput("answers")
  )
)