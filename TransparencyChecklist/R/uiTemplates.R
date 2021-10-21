# write html code for heading
headUI <- function() {
  html <- lapply(headList, switchButtons)
  fluidRow(
    column(1),
    column(10,
           wellPanel(html)
           # ll
    ),
    column(1)
  )
}

sectionsUI <- function() {
  sectionsHTML <- lapply(sectionsList, renderSection)
  names(sectionsHTML) <- NULL
  sectionsHTML <- do.call(tabsetPanel, c(sectionsHTML, id = "sections"))
}

instrUI <- function() {
  fluidRow(
    column(1),
    column(10,
           h3(i18n$t("Please select an answer for each item below. If you want to elaborate on your answers, you can do so in the comment box that follows each section."))
    ),
    column(1)
  )
}

generateReportUI <- function() {
  absolutePanel(
    dropdown(
      h4(i18n$t("Generate & Download Report")),
      pickerInput(inputId = "save.as", label = i18n$t("Format"),
                  choices = c("pdf", "html", "word", "rtf"),
                  multiple = FALSE, width = 'auto', inline = FALSE),
      div(style = "display:inline-block",
          actionBttn(inputId = "preview", label = i18n$t("Preview"), icon = icon("eye"),
                     style = "simple",
                     color = "primary",
                     size = "xs",
                     no_outline = FALSE),# br(), br(),
          actionBttn(inputId = "showcode", label = i18n$t("Show code"), icon = icon("code"),
                     style = "simple",
                     color = "primary",
                     size = "xs",
                     no_outline = FALSE)
      ), br(), br(),
      downloadButton('report', i18n$t('Download'), class = "downbutt"),
      
      icon = icon("file-alt"), up = TRUE,
      tooltip = tooltipOptions(title = i18n$t("Click here to create and download report"), placement = "left"),
      style = "unite", label = i18n$t("Generate Report"),
      size = "lg", inputId = "generatereport", width = "20vw", class = "fixedButton"),
    bottom = "2.5%", left = "50%", fixed = TRUE, width = "auto",
    style = "transform: translate(-50%, +0%); z-index: 1000;")
}


aboutWindowUI <- function() {
  shinyBS::bsModal(id = "intro", title = textOutput("aboutLabel", inline=TRUE), trigger = "triggerIntro", size = "large",
                   fluidRow(
                     column(8),
                     column(4, selectInput("languageModal", textOutput("selectLanguageModal", inline=TRUE), languageList, width = "auto"))
                   ),
                   #includeMarkdown("www/doc/introText.Rmd"),
                   introTextUI(),
                   br(),
                   tags$a(tags$img(src = "img/GitHub-Mark-32px.png"),
                          href = "https://github.com/BalazsAczel/TransparencyChecklist",
                          target = "_blank")
                   )
}

introTextUI <- function() {
  tags$div(
    h3(i18n$t("What is the Transparency Checklist?")),
    tags$p(i18n$t("The Transparency Checklist is a comprehensive checklist that researchers can use to improve and document the transparency of their research. This checklist was developed for social and behavioral scientists who conduct and report confirmatory research on primary data. Nevertheless, several of the checklist items may also be relevant for other approaches and disciplines. For purely exploratory research, only the last 5 items of this short checklist apply.")),
    tags$br(),
    h3(i18n$t("How to use it?")),
    tags$ul(
      tags$li(i18n$t("The checklist refers to a single study of interest.")),
      tags$li(i18n$t("Please respond to each checklist item. If necessary, you can provide an explanation at the end of each section.")),
      tags$li(i18n$t("When the question refers to your manuscript, this includes all supplementary materials and appendices that are relevant to the study of interest.")),
      tags$li(i18n$t("After all the questions have been answered, you can generate a transparency report for your study by pressing the button labeled GENERATE REPORT at the bottom of the page.")),
      tags$li(i18n$t("Save your transparency report on your computer. Note that after you download your report, your responses on the checklist will not be saved by our webpage.")),
      tags$li(i18n$t("Upload your transparency report to a public repository.")),
    ),
    tags$br(),
    i18n$t("You can cite the Transparency Checklist as follows:"),
    tags$br(),
    tags$p("Aczel, B., Szaszi, B., Sarafoglou, A. Kekecs, Z., Kucharský, Š., Benjamin, D., ... & Wagenmakers, E.-J. (2019). A consensus-based transparency checklist.",
           tags$i("Nature Human Behaviour, "), "1--3.",
           tags$a("doi:10.1038/s41562-019-0772-6", href = "https://doi.org/10.1038/s41562-019-0772-6", target = "_blank")),
    tags$hr(),
    
    tags$p(i18n$t("Feedback and recommendations for an update of the checklist can be provided here:"),
           tags$a("https://forms.gle/raN7q1ucpov5sX316", href = "https://forms.gle/raN7q1ucpov5sX316", target = "_blank"))
  )
}