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