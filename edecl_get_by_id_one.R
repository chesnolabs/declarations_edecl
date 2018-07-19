options(scipen = 999)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # set correct Rtools path here
library(tidyverse)
library(stringr)
library(edecl)
library(jsonlite)
library(openxlsx)
library(readxl)

source("edecl_functions.R")

# З цього треба зняти коментар лише в разі внесення змін до
# рад, раніше скачаних автоматично, щоб надалі скачати виправлений список.
# У звичайному випадку скачувати не треба

# dnipro <- read_excel("output/Дніпропетровська обласна рада.xlsx", sheet = 1) %>%
#   select(fullname, id)
# stepswb <- createWorkbook("data/Дніпропетровська облрада.xlsx")
# addWorksheet(stepswb, "Дніпропетровська облрада")
# writeDataTable(stepswb, "Дніпропетровська облрада", dnipro, withFilter = F, rowNames = F)
# saveWorkbook(stepswb, "data/Дніпропетровська облрада.xlsx", overwrite = TRUE)

# Вказати назву ради
council_string <- "Зміївська міська рада"

# Зчитати файл з лінками на декларації, попередньо розміщений
# у підпапці data папки проекту.
# Передбачає простий excel-файл з двома колонками: у першій ПІБ декларанта,
# у другій лінки на декларації на сайті НАЗК чи на declarations.com.ua
# Часом до вихідного файлу з лінками потрібно спершу внести зміни 
# (можна вручну), тому що регіональні координатори часто надсилають
# файли не в коректному форматі.

council <- read_excel("data/Зміїв.xlsx", sheet = 1, col_names = c("fullname", "id")) %>%
  select(1:2) 

# виключаємо депутатів без декларацій і уніфікуємо лінки для API declarations.com.ua
council <- council %>%
  filter(str_detect(id, "ua")) %>% 
  mutate(id = paste0(str_replace(id, "https://public.nazk.gov.ua/declaration/", "https://declarations.com.ua/declaration/nacp_"),
                     "?format=opendata"))

# Закачуємо декларації з declarations.com.ua
deputies_decl <- map(council$id, fromJSON) %>% 
  map("declaration")

# Переводимо в формат списку з розділами, що відповідають розділам декларації
steps_to_write <- get_all_steps(deputies_decl)
for(i in c(1:16)){
  if(nrow(steps_to_write[[i]]) == 0){
    steps_to_write[[i]] <- data.frame(fullname = character()) 
  }
}

# Записуємо в загальний excel-файл з відповідними вкладками
# і в папку csv-файлів, де кожен файл кроку відповідає розділам декларації

worksheet_names <- c("Інфо", "Члени сім'ї", "Нерухомість",
                     "Незавершена нерухомість", "Цінні предмети", "Транспорт", 
                     "Цінні папери", "Корпоративні права",
                     "Компанії", "Нематеріальні активи", "Доходи", "Грошові активи",
                     "Зобов'язання", "Видатки", "Сумісництво", "Членство")

stepswb <- createWorkbook(paste0("output/by_id/", council_string, ".xlsx"))
for(i in c(1:16)){
  addWorksheet(stepswb, worksheet_names[i])
  writeDataTable(stepswb, worksheet_names[i], steps_to_write[[i]], withFilter = F, rowNames = F)
}
saveWorkbook(stepswb, paste0("output/by_id/", council_string, ".xlsx"), overwrite = TRUE)

dir.create(paste0("output/by_id/", council_string))
for(i in seq_along(steps_to_write)){
  write.csv(steps_to_write[[i]], file = paste0("output/by_id/", council_string, "/step", i, ".csv"),
            row.names = F, fileEncoding = "UTF-8")
}
