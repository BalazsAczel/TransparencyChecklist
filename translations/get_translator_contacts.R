### Function that extracts names and e-mail adresses of the people who helped us translating the app
library(here)
library(readxl)
library(readr)
library(tibble)

file_names <- list.files(here("translations", "original_translations", "READY"), full.names = FALSE)
files      <- list.files(here("translations", "original_translations", "READY"), full.names = TRUE)

languages <- gsub(" Translations - Transparency Checklist.xlsx", "", file_names)

df <- tibble(language   = rep(languages, each = 2),
             translator = rep(1:2, length(languages)),
             name       = rep(NA, 2*length(languages)),
             e_mail     = rep(NA, 2*length(languages)))

for(i in seq_along(files)){
  sheet <- readxl::read_excel(files[i], sheet = "Instructions")
  df$name  [df$language == languages[i]] <- sheet$Name[1:2]
  df$e_mail[df$language == languages[i]] <- sheet$`email address`[1:2]
}

readr::write_excel_csv(df, 
                       here("translations", "original_translations", "contacts_transparencyChecklist_translations.csv"))

readr::write_excel_csv(df[complete.cases(df),], 
                       here("translations", "original_translations", "complete_contacts_transparencyChecklist_translations.csv"))

