options(scipen = 999)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # set correct Rtools path here
library(tidyverse)
library(stringr)
library(edecl)
library(jsonlite)
library(openxlsx)
library(readxl)

source("edecl_functions.R")

councils <- vector("list", 4)
names(councils) <- excel_sheets("data/Декларации Херсонщина.xlsx")

for(i in names(councils)){
  councils[[i]] <- read_excel("data/Декларации Херсонщина.xlsx",
                                       sheet = i, col_names = c("fullname", "id")) %>% 
    filter(!is.na(id)) %>% 
    mutate(id = paste0(str_replace(id, "https://public.nazk.gov.ua/declaration/", "https://declarations.com.ua/declaration/nacp_"),
                       "?format=opendata"))
}

# test <- read_csv("output/by_id/Тернопільська міська рада/step11.csv")
councils_df <- bind_rows(councils, .id = "council") %>% 
  filter(!is.na(id))

# map(councils, ~print(.[, "id"]))
decl <- map(councils_df$id, fromJSON) %>%
  map("declaration")

all_deps_splitted <- split(decl, councils_df$council)

steps_to_write_by_council <- map(all_deps_splitted, get_all_steps)
# map(all_deps_splitted[[2]], ~.[["infocard"]]$created_date)

# dir.create("output/by_id")
map(paste0("output/by_id/", names(all_deps_splitted)), dir.create)

for(i in seq_along(steps_to_write_by_council)){
  for(j in seq_along(steps_to_write_by_council[[1]]))
    write.csv(steps_to_write_by_council[[i]][[j]], file = paste0(paste0("output/by_id/", dir("output/by_id/")[grep("рада$", dir("output/by_id/"))][i], "/step", j, ".csv")),
              row.names = F, fileEncoding = "UTF-8")
}

worksheet_names <- c("Інфо", "Члени сім'ї", "Нерухомість",
                     "Незавершена нерухомість", "Цінні предмети", "Транспорт", 
                     "Цінні папери", "Корпоративні права",
                     "Компанії", "Нематеріальні активи", "Доходи", "Грошові активи",
                     "Зобов'язання", "Видатки", "Сумісництво", "Членство")

for(i in seq_along(steps_to_write_by_council)){
  for(j in c(1:16)){
    if(nrow(steps_to_write_by_council[[i]][[j]]) == 0){
      steps_to_write_by_council[[i]][[j]] <- data.frame(fullname = character()) 
    }
}}

# to xls

stringnames <- paste0("output/by_id/", names(steps_to_write_by_council), ".xlsx")

for(i in seq_along(steps_to_write_by_council)){
  stepswb <- createWorkbook(stringnames[i])
  for(j in c(1:16)){
    addWorksheet(stepswb, worksheet_names[j])
    writeDataTable(stepswb, worksheet_names[j], steps_to_write_by_council[[i]][[j]], withFilter = F, rowNames = F)
  }
  saveWorkbook(stepswb, stringnames[i], overwrite = TRUE)
}

# --------------------

test[[1]]$unified_source[["step_3"]][[1]]$rights$`1`$rightBelongs

step_rights_to_df <- function(x){
  rights_df <- map(x[["unified_source"]][["step_3"]], ~.[["rights"]]) %>% 
  map(1) %>% data.table::rbindlist(., fill = TRUE) 
  
  return(rights_df)
}
test_df <- map(test, step_rights_to_df) %>% 
  data.table::rbindlist(., fill = TRUE, use.names = T) %>% 
  select(dnt_ownershipType_encoded, ua_lastname, ukr_firstname, ukr_middlename,
         rightBelongs, ua_company_name,
         `percent-ownership`, citizen, ua_company_code)
test_full <- bind_cols(steps_to_write_by_council[["Тернопільська обласна рада"]][["step3"]], test_df)

table(test_full$person == test_full$rightBelongs)

not_equal <- test_full[test_full$person != test_full$rightBelongs,]
