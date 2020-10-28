library(shiny)

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
  headerPanel(column(12, textOutput("applicationTitle", inline=TRUE), align = "center"),
              windowTitle = "Transparency Checklist 1.0"),
  
  fluidRow(column(1),
           column(10, tags$a(textOutput("preferenceOther", inline=TRUE), 
                             href="http://www.shinyapps.org/apps/TransparencyChecklist/",
                             target="_blank"), align = "center"),
           column(1)
  ),
  br(), 
  # The header (basic information about the paper and authors)
  fluidRow(
    column(1),
    column(10,
      wellPanel(uiOutput("head"))),
    column(1)
  ),
  
  # Show initial instructions:
  fluidRow(
    column(1),
    column(10, 
           h3(textOutput("generalInstruction", inline=TRUE))
           ),
    column(1)
  ),
  
  br(), br(),
  tags$div(id = "scrollAnchor"), # for scrolling up
  # Show questions
  uiOutput("questions"),
  
  # Switching between sections
  fluidRow(column(2),
           column(2, align = "center",
                  actionButton("previousButton", textOutput("goToPrevious", inline=TRUE), icon = icon("arrow-circle-left"))),
           column(4),
           column(2, align = "center",
                  actionButton("nextButton", textOutput("goToNext", inline=TRUE), icon = icon("arrow-circle-right"))),
           column(2)
  ),
  br(), br(),
  ##### Report menu (downloading) ----
  absolutePanel(
    dropdown(
      #h4("Generate & Download Report"),
      h4(textOutput("generateDownloadReportLabel", inline=TRUE)),
      pickerInput(inputId = "save.as", label = textOutput("formatLabel", inline=TRUE), 
                  choices = c("pdf", "html", "word", "rtf"), 
                  multiple = FALSE, width = 'auto', inline = FALSE),
      div(style = "display:inline-block",
        actionBttn(inputId = "preview", label = textOutput("previewLabel", inline=TRUE), icon = icon("eye"),
                   style = "simple",
                   color = "primary",
                   size = "xs",
                   no_outline = FALSE),# br(), br(), 
        actionBttn(inputId = "showcode", label = textOutput("showCodeLabel", inline=TRUE), icon = icon("code"),
                   style = "simple",
                   color = "primary",
                   size = "xs",
                   no_outline = FALSE)
        ), br(), br(),
      downloadButton('report', textOutput('downloadLabel', inline=TRUE), class = "downbutt"),
      
      icon = icon("file-alt"), up = TRUE, 
      style = "unite", label = textOutput("generateReportLabel", inline=TRUE),
      size = "lg", inputId = "generatereport", width = "20vw", class = "fixedButton"),
    bottom = "2.5%", left = "50%", fixed = TRUE, width = "auto",
    style = "transform: translate(-50%, +0%); z-index: 1000;"),
  
  # tooltip for the dropdown
  uiOutput("generateReportTooltip"),
  
  # Open window for a preview
  shinyBS::bsModal(id = "previewer", title = textOutput("previewLabel2", inline=TRUE), trigger = "preview", size = "large",
                   shinycssloaders::withSpinner(uiOutput("generatePreview"))),

  # Open window for a code
  shinyBS::bsModal(id = "codeshower", title = textOutput("codeLabel", inline=TRUE), trigger = "showcode", size = "large",
                   shinycssloaders::withSpinner(verbatimTextOutput("code"))),

  # Show tooltip which says that the download is not ready
  uiOutput("reportTooltip"),
  uiOutput("trigger"), # this trigger displays or hides the explaining tooltip
  br(), br(),

  # info modal
  shinyBS::bsModal(id = "intro", title = textOutput("aboutLabel", inline=TRUE), trigger = "triggerIntro", size = "large",
                   fluidRow(
                     column(8),
                     column(4, selectInput("languageModal", textOutput("selectLanguageModal", inline=TRUE), languageList, width = "auto"))
                   ),
                   #includeMarkdown("www/doc/introText.Rmd"),
                   uiOutput("introText"),
                   br(),
                   tags$a(tags$img(src = "img/GitHub-Mark-32px.png"),
                          href = "https://github.com/BalazsAczel/TransparencyChecklist",
                          target = "_blank")),
  absolutePanel(
    actionBttn(inputId = "triggerIntro", label = textOutput("aboutLabel2", inline=TRUE), icon = icon("info-circle")),
    top = "3%", left = "2%", fixed = TRUE, width = "auto"
  ),
  
  absolutePanel(
    selectInput("language", textOutput("selectLanguage", inline=TRUE), languageList, width = "auto"),
    top = "3%", right = "2%", fixed = TRUE, width = "10%"
  )
  
  #temporary (for debugging): showing the current status of the answers
  # ,br(),
  # verbatimTextOutput("answers")
  )
)
