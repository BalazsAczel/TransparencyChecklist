## Functions which help us to create some structure of the shiny app
## and defines the buttons which appear horizontal

renderSection <- function(section, answers = NULL){
  # creates a tab ( can be changed to a fluidrow if we do not want tabs)
  
  tabPanel(title = i18n$t(section$Name), 
           value = section$Value, 
           icon  = tags$i(class = paste0("icon", section$Value, " fa fa-eye")),#icon("table"),
           #id    = section$Value,
    br(),
    # create the header and an initial info about the section
    fluidRow(column(1),
             column(10,
                    #h3(section$Name),
                    ifelse(!is.null(section$Label), strong(i18n$t(section$Label)), "")
                    ),
             column(1)),

    # render all fields within this section
    lapply(section$Questions, customField, answers = answers),
    
    # a break line after each section
    fluidRow(hr())
  )
  
}

customField <- function(ind, answers = NULL){
  # if the input is not a question, it is assumed that it is some guidance text in between the items
  if(ind$Type == "text"){
    
    # the guidance text can itself be conditional
    if(is.null(ind$Depends)){
      fluidRow(column(1),
               column(10, br(), strong(i18n$t(ind$Label))),
               column(1))
    } else{
      conditionalPanel(condition = gsub(pattern = "\\.", replacement = "input.", ind$Depends),
                       fluidRow(column(1), 
                                column(10, br(), strong(i18n$t(ind$Label))),
                                column(1))
                       )
    }
  } else { # render questions
    customButton(ind, answers)
  }
}


customButton <- function(ind, answers = NULL){
    
  # Always display unconditional items  
  if(is.null(ind$Depends)){
    ind$Depends <- "true"
  } else { # or display depending on the state of the input
    ind$Depends <- gsub(pattern = "\\.", replacement = "input.", ind$Depends)
  }
  
  if(ind$Type != "comment"){ # when the item is not a comment, show the button in a 6:3:1 format (label:button:validation)
    
    fluidPage( # wrapping into another fluid page makes a slight indentation of the questions from the text fields
    conditionalPanel(condition = ind$Depends,
                     fluidRow(column(1),
                              column(6, br(), i18n$t(ind$Label)),#, 
                                     #a(ind$href, href = ind$href, target = "_blank"),
                                     #ind$LabelEnd), # this makes the buttons appear horizonally aligned
                              column(3, switchButtons(ind, answers)), # create a standard shiny button
                              column(1, br(), # adds exclamation circle next to the item
                                     tags$div(
                                       id = paste0("div", ind$Name, "Checker"),
                                       title = i18n$t("This question needs to be answered."),
                                       tags$i(id = paste0(ind$Name, "Checker"),
                                              class = 'fa fa-exclamation-circle')
                                       )
                                     ),
                              column(1)
                     )
    )
    )
  } else{ # when the item is a comment, show the commentary section as a standard textArea stretched over the width of the panel
    #fluidPage(
      conditionalPanel(condition = ind$Depends,
                       fluidRow(column(1),
                                column(10, br(), strong(i18n$t(ind$Label)), br(),
                                       tags$style(type = "text/css", "textarea {width:80%}"),
                                       tags$textarea(id = ind$Name, placeholder = ind$AnswerType,
                                                     rows = 5, class = "form-control")),
                                column(1)))
    #)
  }
}


switchButtons <- function(ind, answers = NULL){
  # if the AnswerType is specified in the answerList object (from .json), the button options should be rendered from 
  # those options
  # otherwise, the AnswerType is passed directly to the options
  if(ind$AnswerType %in% names(answerList)){
    answerOptions <- answerList[[ind$AnswerType]]
  } else{ 
    answerOptions <- ind$AnswerType
  }
  # trans
  if(is.list(answerOptions)){
    names(answerOptions) <- lapply(names(answerOptions), i18n$t)
  } else{
    answerOptions <- i18n$t(answerOptions)
  }
  
  # preserve selected values if translation was called
  answered <- ind$Name %in% names(answers)
  if(answered){
    selected <- answers[[ind$Name]]
  } else{
    selected <- NULL
  }
  
  # switch between different input types
  switch (ind$Type,
    "select"    = pickerInput(inputId = ind$Name, label = "", choices = c("", answerOptions),
                              selected = selected, multiple = FALSE,
                              options = pickerOptions(noneSelectedText = i18n$t("Please select an option"))),
    "radio"     = radioButtons(inputId = ind$Name, label = "", choices = answerOptions, selected = ifelse(is.null(selected), 0, selected),
                               inline = TRUE),
    "textInput" = textInput(inputId = ind$Name, label = i18n$t(ind$Label), value = ifelse(is.null(selected), i18n$t(ind$AnswerType), selected)),
    "textArea"  = textAreaInput(inputId = ind$Name, label = "", placeholder = answerOptions, rows = 6, value = ifelse(is.null(selected), "", selected))
  )
}


getItemList <- function(sectionsList, all = TRUE){
  items <- unlist(sapply(sectionsList, function(section) sapply(section$Questions, function(item) item$Name)))
  
  if(all){
    return(items)
  } else {
    return(items[grep("ind", items)])
  }
}