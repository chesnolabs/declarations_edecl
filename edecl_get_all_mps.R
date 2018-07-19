options(scipen = 999)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # set correct Rtools path here
library(tidyverse)
library(stringr)
library(edecl)
library(jsonlite)
library(openxlsx)
source("edecl_functions.R")

decl_year <- 2017 # set year here

deputies_decl <- 
  download_declarations("народний депутат", deepsearch = T, declaration_year = decl_year,
                        doc_type = "Щорічна") %>% 
  get_corrected()

decl_names <- deputies_decl %>%
  extract_names() %>% 
  clean_names()

actual_dep_names <- get_mps_names() %>% 
  clean_names()

actual_dep_names[which(!(actual_dep_names%in%decl_names))]
index_not_deputies <- which(!(decl_names%in%actual_dep_names))

dep_decl <- deputies_decl[-index_not_deputies]

# add missing ids manually
ids_missing <- c("nacp_21a6e7bb-9b2d-4577-ac9e-a40d4349cfa6",
                 "nacp_9def314c-7afb-4e79-a75b-1de28c802f8d",
                 "nacp_3b8df879-d123-414d-b89f-0a29030c6aa2")
links_missing <- paste0("https://declarations.com.ua/declaration/",
                        ids_missing, "?format=opendata")

decl_missing <- map(links_missing, fromJSON) %>%
  map("declaration")

dep_decl <- c(dep_decl, decl_missing)

steps_to_write <- get_all_steps(dep_decl)

worksheet_names <- c("Інфо", "Члени сім'ї", "Нерухомість",
                           "Незавершена нерухомість", "Цінні предмети", "Транспорт", 
                           "Цінні папери", "Корпоративні права",
                           "Компанії", "Нематеріальні активи", "Доходи", "Грошові активи",
                           "Зобов'язання", "Видатки", "Сумісництво", "Членство")

dir.create("output")
stepswb <- createWorkbook(paste0("output/mps_", decl_year, ".xlsx"))
for(i in c(1:16)){
  addWorksheet(stepswb, worksheet_names[i])
  writeDataTable(stepswb, worksheet_names[i], steps_to_write[[i]], withFilter = F, rowNames = F)
}
saveWorkbook(stepswb, paste0("output/mps_", decl_year, ".xlsx"), overwrite = TRUE)

dir.create(paste0("output/", decl_year))
for(i in seq_along(steps_to_write)){
  write.csv(steps_to_write[[i]], file = paste0("output/", decl_year, "/step", i, ".csv"),
                     row.names = F, fileEncoding = "UTF-8")
}
