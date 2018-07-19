options(scipen = 999)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # set correct Rtools path here
library(tidyverse)
library(stringr)
library(edecl)
library(jsonlite)
library(openxlsx)
library(readxl)

source("edecl_functions.R")

# Вказати кількість вкладок і назву файлу з деклараціями
councils <- vector("list", 4)
names(councils) <- excel_sheets("data/Декларации Херсонщина.xlsx")

# Зчитуємо і уніфікуємо лінки з деклараціями
for(i in names(councils)){
  councils[[i]] <- read_excel("data/Декларации Херсонщина.xlsx",
                                       sheet = i, col_names = c("fullname", "id")) %>% 
    filter(!is.na(id)) %>% 
    filter(str_detect(id, "ua")) %>% 
    mutate(id = paste0(str_replace(id, "https://public.nazk.gov.ua/declaration/", "https://declarations.com.ua/declaration/nacp_"),
                       "?format=opendata"))
}

councils_df <- bind_rows(councils, .id = "council") %>% 
  filter(!is.na(id))

# Закачуємо з declarations.com.ua
decl <- map(councils_df$id, fromJSON) %>%
  map("declaration")

# Розкидаємо декларації по радах і розділах деклараціій
all_deps_splitted <- split(decl, councils_df$council)

steps_to_write_by_council <- map(all_deps_splitted, get_all_steps)

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
