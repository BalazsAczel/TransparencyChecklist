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
  headerPanel(column(12, uiOutput("applicationTitle"), align = "center"),
              windowTitle = "Transparency Checklist 1.0"),
  
  fluidRow(column(1),
           column(10, tags$a(uiOutput("preferenceOther"), 
                             href="http://www.shinyapps.org/apps/TransparencyChecklist/",
                             target="_blank"), align = "center"),
           column(1)
  ),
  br(), 
  selectInput("language", uiOutput("selectLanguage"), list("en", "cz")),
  # The header (basic information about the paper and authors)
  fluidRow(
    column(1),
    column(10,
      #wellPanel(h4(textOutput("currentTime")), br(), headHTML)),
      #wellPanel(headHTML)),
      wellPanel(uiOutput("head"))),
    column(1)
  ),
  
  # Show initial instructions:
  fluidRow(
    column(1),
    column(10, 
           h3(uiOutput("generalInstruction"))
           ),
    column(1)
  ),
  
  br(), br(),
  tags$div(id = "scrollAnchor"), # for scrolling up
  # Show questions
  uiOutput("questions"),
  #sectionsHTML,
  
  # Switching between sections
  fluidRow(column(2),
           column(2, align = "center",
                  actionButton("previousButton", uiOutput("goToPrevious"), icon = icon("arrow-circle-left"))),
           column(4),
           column(2, align = "center",
                  actionButton("nextButton", uiOutput("goToNext"), icon = icon("arrow-circle-right"))),
           column(2)
  ),
  br(), br(),
  ##### Report menu (downloading) ----
  absolutePanel(
    dropdown(
      #h4("Generate & Download Report"),
      h4(uiOutput("generateDownloadReportLabel")),
      pickerInput(inputId = "save.as", label = uiOutput("formatLabel"), 
                  choices = c("pdf", "html", "word", "rtf"), 
                  multiple = FALSE, width = 'auto', inline = FALSE),
      div(style = "display:inline-block",
        actionBttn(inputId = "preview", label = uiOutput("previewLabel"), icon = icon("eye"),
                   style = "simple",
                   color = "primary",
                   size = "xs",
                   no_outline = FALSE),# br(), br(), 
        actionBttn(inputId = "showcode", label = uiOutput("showCodeLabel"), icon = icon("code"),
                   style = "simple",
                   color = "primary",
                   size = "xs",
                   no_outline = FALSE)
        ), br(), br(),
      downloadButton('report', uiOutput('downloadLabel'), class = "downbutt"),
      
      icon = icon("file-alt"), up = TRUE, 
      tooltip = tooltipOptions(title = uiOutput("clickToDownloadLabel"), placement = "left"),
      style = "unite", label = uiOutput("generateReportLabel"),
      size = "lg", inputId = "generatereport", width = "20vw", class = "fixedButton"),
    bottom = "2.5%", left = "50%", fixed = TRUE, width = "auto",
    style = "transform: translate(-50%, +0%); z-index: 1000;"),
  
  # Open window for a preview
  shinyBS::bsModal(id = "previewer", title = uiOutput("previewLabel2"), trigger = "preview", size = "large",
                   shinycssloaders::withSpinner(uiOutput("generatePreview"))),

  # Open window for a code
  shinyBS::bsModal(id = "codeshower", title = uiOutput("codeLabel"), trigger = "showcode", size = "large",
                   shinycssloaders::withSpinner(verbatimTextOutput("code"))),

  # Show tooltip which says that the download is not ready
  shinyBS::bsTooltip(id = "report",
                     title = uiOutput("reportDownloadableLabel"),
                     # Please, respond to all displayed items to download the pdf report (comments are optional).
                     trigger = "manual",
                     placement = "right"),
  uiOutput("trigger"), # this trigger displays or hides the explaining tooltip
  br(), br(),

  # info modal
  shinyBS::bsModal(id = "intro", title = uiOutput("aboutLabel"), trigger = "triggerIntro", size = "large",
                   includeMarkdown("www/doc/introText.Rmd"),
                   br(),
                   tags$a(tags$img(src = "img/GitHub-Mark-32px.png"),
                          href = "https://github.com/BalazsAczel/TransparencyChecklist",
                          target = "_blank")),
  absolutePanel(
    actionBttn(inputId = "triggerIntro", label = "About", icon = icon("info-circle")),
    top = "3%", left = "2%", fixed = TRUE, width = "auto"
  )
  # temporary (for debugging): showing the current status of the answers
  # ,br(),
  # verbatimTextOutput("answers")
  )
)
