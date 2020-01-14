library(shiny)

shinyServer(function(input, output, session) {
  
  #### Send header and questions to UI ----
  # output$currentTime <- renderText({
  #   #invalidateLater(1000, session)
  #   shinyBS::toggleModal(session, "intro")
  #   paste("Created on: ", format(Sys.time(), '%d %B, %Y'))
  # })
  
  observe({
    shinyBS::toggleModal(session, "intro")
  })
  
  observe({
    i18n$set_translation_language(input$language)
  })
  # Deprecated rendering of sections (now created in global.R and passed in ui directly)
  # render the header section
  output$head <- renderUI({
    reactTo <- input$language
    lapply(headList, switchButtons)
  })
  
  # render sections as tab sections
  output$questions <- renderUI({
    reactTo <- input$language
    sectionsHTML <- lapply(sectionsList, renderSection)
    names(sectionsHTML) <- NULL
    do.call(tabsetPanel, c(sectionsHTML, id = "sections"))
  })
  
  #### Store answers, check whether checklist is complete ----
  # stores the answers in a list
  answers <- reactive({
    reactiveValuesToList(input)
  })
  
  #### Moving to the next or previous sections ----
  observeEvent(input$nextButton, {
    sectionId <- sapply(sectionsList, function(section) section$Value)
    currSection <- which(sectionId == input$sections)
    nextSection <- currSection + 1
    updateTabsetPanel(session, "sections", selected = sectionId[[nextSection]])
    shinyjs::runjs("document.getElementById('scrollAnchor').scrollIntoView({behavior: 'smooth'});")
  })
  observeEvent(input$previousButton, {
    sectionId <- sapply(sectionsList, function(section) section$Value)
    currSection <- which(sectionId == input$sections)
    nextSection <- currSection - 1
    updateTabsetPanel(session, "sections", selected = sectionId[[nextSection]])
    shinyjs::runjs("document.getElementById('scrollAnchor').scrollIntoView({behavior: 'smooth'});")
  })
  # disable moving next or previous for first and last sections
  observeEvent(input$sections, {
    sectionId <- sapply(sectionsList, function(section) section$Value)
    currSection <- which(sectionId == input$sections)
    if(currSection == 1){
      shinyjs::hide("previousButton")
    } else{
      shinyjs::show("previousButton")
    }
    
    if(currSection == length(sectionId)){
      shinyjs::hide("nextButton")
    } else{
      shinyjs::show("nextButton")
    }
  })

  # Temporary output that shows the current answers for debugging
  # output$answers <- renderPrint({
  #  answers()
  # })
  
  # checks which sections are complete
  whichComplete <- reactive({
    isComplete(answers = answers(), sectionsList = sectionsList, headList = headList)
  })
  
  # checks whether the report is complete
  isDownloadable <- reactive({
    all(whichComplete())
  })
  
  #### Reactive animations ----
  # whenever the input is complete, let's enable the download button
  observe({
    if(isDownloadable()){
      shinyjs::enable("report")
      
      # and start animation every 4 sec
      invalidateLater(4000, session)
      shinyanimate::startAnim(session, "generatereport", type = "bounce")
    } else{
      shinyjs::disable("report")
    }
  })
  
  # whenever the input is not complete, show the tooltip for explanation for the download button
  output$trigger <- renderUI({
    if(isDownloadable()){
      tags$script("$('#report').tooltip('hide');")
    } else{
      tags$script("$('#report').tooltip('show');")
    }

  })
  
  # changing icons when item is answered
  observe({
    items <- getItemList(sectionsList, all = FALSE) # loop only on items
    
    for(item in items){
      session$sendCustomMessage(
        type = "toggleChecker",
        message = list(id = paste0(item, "Checker"), val = input[[item]], divId = paste0("div", item, "Checker"))
      )
    }
  
  })
  
  observeEvent(input$generatereport, {
    sectionId <- sapply(sectionsList, function(section) section$Value)
    
    #if(!isDownloadable()){
      for(section in sectionId){
        # output[[paste0("icon_", section)]] <- renderText({"table"})
      }  
    #}
  })
  
  observeEvent(input$generatereport, {
    items <- getItemList(sectionsList, all = FALSE)
    ans   <- isolate(answers())
    
    for(item in items){
      if(ans[item] == "" || is.null(ans[[item]])){
        shinyanimate::startAnim(session, paste0(item, "Checker"), type = "shake")
      }
      
      session$sendCustomMessage(
        type = "toggleCheckerColor",
        message = list(id = paste0(item, "Checker"), val = input[[item]], divId = paste0("div", item, "Checker"))
      )
    }
  })
 
  # Change icons in Section headings (three state option)
  observe({
    sectionValues <- sapply(sectionsList, function(sec) sec$Value)
    for(i in seq_along(sectionValues)){
      session$sendCustomMessage(
        type = "toggleSectionIcon",
        # as long as the user does not click "report", do not display aggresive feedback (-> val = "init")
        message = list(id = paste0(".icon", sectionValues[[i]]),
                       val = ifelse(input$generatereport == 0 && !whichComplete()[[i]],
                                    "init", whichComplete()[[i]])
                       )
      )
    }
  })
  
  # validate title of the study
  observeEvent(input$studyTitle, {
    feedbackSuccess(
      inputId = "studyTitle",
      condition = input$studyTitle != "",
      text = NULL,
      color = "black"
    )
  })
  
  # validate author names
  observeEvent(input$authorNames, {
    feedbackSuccess(
      inputId = "authorNames",
      condition = input$authorNames != "",
      text = NULL,
      color = "black"
    )
  })
  
  
  # validate e-mail
  observeEvent(input$correspondingEmail, {
    if (input$correspondingEmail == "@" || input$correspondingEmail == ""){
      feedback(
        inputId   = "correspondingEmail",
        condition = TRUE,
        text      = " ",
        color     = "black",
        icon      = NULL
      )
    } else if (isValidEmail(input$correspondingEmail)){
      feedbackSuccess(
        inputId   = "correspondingEmail",
        condition = TRUE,
        text      = " ",
        color     = "black"
      )
    } else {
      feedbackWarning(
        inputId   = "correspondingEmail",
        condition = TRUE,
        text      = "Provided email appears invalid.",
        color     = "black"
      )
    }
  })
  
  # validate link
  observeEvent(input$linkToRepository, {
    if (input$linkToRepository == ""){
      feedback(
        inputId   = "linkToRepository",
        condition = TRUE,
        text      = " ",
        color     = "black",
        icon      = NULL
      )
    } else if (RCurl::url.exists(input$linkToRepository)){
      feedbackSuccess(
        inputId   = "linkToRepository",
        condition = TRUE,
        text      = " ",
        color     = "black"
      )
    } else {
      feedbackWarning(
        inputId   = "linkToRepository",
        condition = TRUE,
        text      = "The link cannot be accessed.",
        color     = "black"
      )
    }
  })
  
  
  #### Working with report ----
  # Stash current Rmd if report dropdown is opened or save_as is changed  
  RmdFile <- reactive({
    dontrun <- input$generatereport
    composeRmd(answers = isolate(answers()),
               sectionsList = sectionsList, headList = headList, answerList = answerList,
               save.as = input$save.as)
  })
  
  # render Rmd file in show code modal panel
  output$code <- renderText({
    RmdFile()
  })
  
  # render previews
  output$generatePreview <- renderUI({
    input$preview
    RmdPath <- file.path(tempdir(), "report.Rmd")
    writeLines(RmdFile(), con = RmdPath)

    if(input$save.as %in% c("word", "rtf")){
      showNotification("Word and rtf files cannot be previewed in the browser, displaying markdown file",
                       type = "warning", closeButton = FALSE, duration = 7)
      includeMarkdown(RmdPath)
    } else{
      save.as <- ifelse(input$save.as == "word", "docx", input$save.as)
      out_file <- paste0("preview.", save.as)

      rmarkdown::render(RmdPath, output_file = out_file, output_dir = "www/doc",
                        envir = new.env(parent = globalenv()))
      src_file <- file.path("doc", out_file)
      tags$iframe(style = "height:600px; width:100%", src = src_file)
    }
  })

  #### Download ----
  # This section deals with the pdf generation
  output$report <- downloadHandler(
    
    
    filename = function() {
      save.as <- ifelse(input$save.as == "word", "doc", input$save.as)
      paste("Transparency Report", save.as, sep = ".")
    },
    
    content = function(file) {
      # Create the report file in a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).

      # create a string which copies the structure of the desired rmd file
      RmdFile <- composeRmd(answers = answers(),
                            sectionsList = sectionsList, headList = headList, answerList = answerList,
                            save.as = input$save.as)
      
      # print the Rmd document in the console (for debugging)
      #writeLines(RmdFile)
      
      # store the string as a temporary report.Rmd file 
      tempReport <- file.path(tempdir(), "report.Rmd")
      writeLines(RmdFile, con = tempReport)
      
      # knit the temporary document into a proper pdf (which will be called "report.pdf/html/doc")
      rmarkdown::render(tempReport, output_file = file,
                        envir = new.env(parent = globalenv()))
      
      showNotification("Downloaded", type = "message", duration = 3, closeButton = FALSE)
    }
  )
  
  #### Translated buttons ----
  inAppTexts <- reactive({
    reactTo <- input$language
    list(
      applicationTitle            = i18n$t("CREATING TRANSPARENCY CHECKLIST 1.0 (short, 12 items)"),
      preferenceOther             = i18n$t("I prefer to fill out the full (36-item) checklist."),
      selectLanguage              = i18n$t("Select Language"),
      generalInstruction          = i18n$t("Please select an answer for each item below. If you want to elaborate on your answers, you can do so in the comment box that follows each section."),
      goToPrevious                = i18n$t("Go to previous section"),
      goToNext                    = i18n$t("Go to next section"),
      generateDownloadReportLabel = i18n$t("Generate & Download Report"),
      formatLabel                 = i18n$t("Format"),
      previewLabel                = i18n$t("Preview"),
      showCodeLabel               = i18n$t("Show code"),
      downloadLabel               = i18n$t("Download"),
      clickToDownloadLabel        = i18n$t("Click here to create and download report"),
      generateReportLabel         = i18n$t("Generate Report"),
      previewLabel2               = i18n$t("Preview"),
      codeLabel                   = i18n$t("Code"),
      reportDownloadableLabel     = i18n$t("A report can be downloaded after all questions in each section have been answered."),
      aboutLabel                  = i18n$t("About")
    )
  })
  output$applicationTitle            <- renderText({ inAppTexts()$applicationTitle            })
  output$preferenceOther             <- renderText({ inAppTexts()$preferenceOther             })
  output$selectLanguage              <- renderText({ inAppTexts()$selectLanguage              })
  output$generalInstruction          <- renderText({ inAppTexts()$generalInstruction          })
  output$goToPrevious                <- renderText({ inAppTexts()$goToPrevious                })
  output$goToNext                    <- renderText({ inAppTexts()$goToNext                    })
  output$generateDownloadReportLabel <- renderText({ inAppTexts()$generateDownloadReportLabel })
  output$formatLabel                 <- renderText({ inAppTexts()$formatLabel                 })
  output$previewLabel                <- renderText({ inAppTexts()$previewLabel                })
  output$showCodeLabel               <- renderText({ inAppTexts()$showCodeLabel               })
  output$downloadLabel               <- renderText({ inAppTexts()$downloadLabel               })
  output$clickToDownloadLabel        <- renderText({ inAppTexts()$clickToDownloadLabel        })
  output$generateReportLabel         <- renderText({ inAppTexts()$generateReportLabel         })
  output$previewLabel2               <- renderText({ inAppTexts()$previewLabel2               })
  output$codeLabel                   <- renderText({ inAppTexts()$codeLabel                   })
  output$reportDownloadableLabel     <- renderText({ inAppTexts()$reportDownloadableLabel     })
  output$aboutLabel                  <- renderText({ inAppTexts()$aboutLabel                  })
})
