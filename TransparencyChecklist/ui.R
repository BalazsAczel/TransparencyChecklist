library(shiny)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  #### Load various .js files enabling interactivity ----
  useShinyjs(), # this is for enabling/disabling buttons from shinyjs
  useShinyFeedback(), # enabling/disabling feedback from shinyFeedback
  withAnim(), # enable animations from shinyanimate
  shiny.i18n::usei18n(i18n),

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
  headerPanel(column(12, i18n$t("CREATING TRANSPARENCY CHECKLIST 1.0 (full, 36 items)"), align = "center"),
              windowTitle = "Transparency Checklist 1.0"),
  
  fluidRow(column(1),
           column(10, tags$a(i18n$t("I prefer to fill out the short (12-item) checklist."),
                             href="http://www.shinyapps.org/apps/ShortTransparencyChecklist/",
                             target="_blank"), align = "center"),
           column(1)
  ),
  br(),
  
  # The header (basic information about the paper and authors)
  headUI(),
  
  # Show initial instructions:
  instrUI(),
  
  br(), br(),
  tags$div(id = "scrollAnchor"), # for scrolling up
  # Show questions
  #uiOutput("questions"),
  #sectionsHTML,
  sectionsUI(),
  
  # Switching between sections
  fluidRow(column(2),
           column(2, align = "center",
                  actionButton("previousButton", i18n$t("Go to previous section"), icon = icon("arrow-circle-left"))),
           column(4),
           column(2, align = "center",
                  actionButton("nextButton", i18n$t("Go to next section"), icon = icon("arrow-circle-right"))),
           column(2)
  ),
  br(), br(),
  
  ##### Report menu (downloading) ----
  generateReportUI(),
  
  # tooltip for the dropdown
  #uiOutput("generateReportTooltip"),
  
  # Open window for a preview
  shinyBS::bsModal(id = "previewer", title = i18n$t("Preview"), trigger = "preview", size = "large",
                   shinycssloaders::withSpinner(uiOutput("generatePreview"))),
  
  # Open window for a code
  shinyBS::bsModal(id = "codeshower", title = i18n$t("Code"), trigger = "showcode", size = "large",
                   shinycssloaders::withSpinner(verbatimTextOutput("code"))),

  # Show tooltip which says that the download is not ready
  shinyBS::bsTooltip(id = "report",
                     title = i18n$t("A report can be downloaded after all questions in each section have been answered."),
                     # Please, respond to all displayed items to download the pdf report (comments are optional).
                     trigger = "manual",
                     placement = "right"),
  uiOutput("trigger"), # this trigger displays or hides the explaining tooltip
  br(), br(),

  # info modal
  aboutWindowUI(),
  
  absolutePanel(
    actionBttn(inputId = "triggerIntro", label = textOutput("aboutLabel2", inline=TRUE), icon = icon("info-circle")),
    top = "3%", left = "2%", fixed = TRUE, width = "auto"
  ),
  
  absolutePanel(
    selectInput("language", i18n$t("Select Language"), languageList, width = "auto"),
    top = "3%", right = "2%", fixed = TRUE, width = "10%"
  )
  
  #temporary (for debugging): showing the current status of the answers
  # ,br(),
  # verbatimTextOutput("answers")
  )
)
