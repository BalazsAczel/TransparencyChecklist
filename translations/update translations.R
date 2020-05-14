library(here)
library(readxl)
library(readr)
library(tibble)
library(jsonlite)
source(here("translations", "functions.R"))

file_names <- list.files(here("translations", "original_translations", "READY"), full.names = FALSE)
files      <- list.files(here("translations", "original_translations", "READY"), full.names = TRUE)

languages <- gsub(" Translations - Transparency Checklist.xlsx", "", file_names)

sheet1 <- readxl::read_excel(files[1], sheet = 2)

translation_df <- tibble(.rows = nrow(sheet1))
translation_df$English <- sheet1$`in English`
translation_df[1,1] <- "English"

for(i in seq_along(files)){
  sheet <- readxl::read_excel(files[i], sheet = 2)
  if(nrow(sheet) != nrow(sheet1)) {
    sheet <- readxl::read_excel(files[i], sheet = "EN1 to Your language")
  }
  
  translation_df[[languages[i]]] <- sheet[, 2, drop=TRUE]
  
  if(is.na(translation_df[[languages[i]]][1])){ # language name not provided
    translation_df[[languages[i]]][1] <- languages[i]
  }
  
  translation_df[[languages[i]]][1] <- gsub("\\[", "", translation_df[[languages[i]]][1])
  translation_df[[languages[i]]][1] <- gsub("\\]", "", translation_df[[languages[i]]][1])

}

translations <- df2list(translation_df)

jsonlite::write_json(translations, here("translations/translations.json"), pretty=TRUE)
jsonlite::write_json(translations, here("TransparencyChecklist/data/translations.json"), pretty=TRUE)
jsonlite::write_json(translations, here("ShortTransparencyChecklist/data/translations.json"), pretty=TRUE)
