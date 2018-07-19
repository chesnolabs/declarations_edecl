options(scipen = 999)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # set correct Rtools path here
library(tidyverse)
library(stringr)
library(edecl)
library(jsonlite)
library(openxlsx)

source("edecl_functions.R")

deputies_decl <- 
  download_declarations("депутат обласної ради обласна рада", deepsearch = T, declaration_year = "2017",
                        doc_type = "Щорічна") %>% 
  get_corrected()

decl_names <- deputies_decl %>%
  extract_names() %>% 
  clean_names()

obldep <- read_csv("data/all_deputies.csv") %>% 
  filter(council_type == "обласна")

actual_dep_names <- obldep$fullname %>% clean_names()

index_not_deputies <- which(!(decl_names%in%actual_dep_names))

dep_decl <- deputies_decl[-index_not_deputies]

actual_dep_names[which(!(actual_dep_names%in%decl_names))]

deps_not_in <- actual_dep_names[which(!(actual_dep_names%in%decl_names))]

deps_not_in_obl <- obldep %>% filter(fullname%in%deps_not_in) %>% 
  select(council) %>% 
  mutate(council = str_replace(council, " обласна рада", " область")) %>% 
  unlist() %>% unname()

# names manually checked - to be corrected every time
names_to_change <- c("Хребтій Ярослав Віталійович", "Яськов Микола
  Олексійович", "Рубльов Вячеслав Володимирович", "Цейко Юрій Феодосійович",
  "Кравчук Святослав Євгенович", "Волков Віктор Пилипович", "Мухтаров Гошгар
  Аладдін-огли", "Рибинський Ігор Євгенійович", "Лендєл Золтан Золтанович",
  "Демянчук Василь Юрійович", "Товт Міклош Міклошович", "Гулачі Гийза
  Людвикович", "Жєрдєв Ігор Сергійович", "Зотов Дмитро Іванович", "Остащук
  Руслан Васильович", "Шувар Андрій Йосипович", "Мельнеченко Оксана Михайлівна",
  "Цагареішвілі Жора", "Чередніченко Юрій Анатолійович", "Ревенко Наталя
  Олександрівна", "Лоцман Генадій Григорович", "Садрідінов Тахір Шамсідінович",
  "Садрідінов Рустамджан Шамсідінович", "Марін Григорій Афанасійович",
  "Чередниченко Анатолій Григорович", "Недозимований Ігор Сергійович",
  "Алексєйчук Лілія Олексіївна", "Кащі Олександр Родіонович", "Сухань Олександр
  Якович", "Макаруша Наталія Миколаївна", "Гаркавенко Олександр Михайлович",
  "Ундір Віталій Олександрович", "Женевський Сергій Юрієвич", "Меєрович Юрій
  Михайлович", "Кірюхін Дмитро Євгенович", "Калініченко Наталія Євгеніївна",
  "Тітов Микола Ілліч", "Забєлін Віктор Васильович", "Ставицький Микола
  Павлович", "Андрійчук Неоніла В’ячеславівна", "Янчук Микола Андрійович", "Заїц
  Мар’ян Дмитрович", "Мельничук Віталій Кузьміч", "Гайнічеру Михайло Іванович",
  "Блауш Дмитро Ярославович") %>%
  str_replace("\\n", "") %>% str_replace("  ", " ")

names_changed <- c("Хребтий Ярослав Витальевич", "Яськов Николай Алексеевич",
  "Рубльов В'ячеслав Володимирович", "Цейко Юрий Феодосьевич", "Кравчук
  Святослав Евгеньевич", "Волков Виктор Филиппович", "Мухтаров Гошгар Аладдін
  Огли", "Рибинський Ігор Євгенович", "Ленд'єл Золтан Золтанович", "Дем'янчук
  Василь Юрійович", "Товт Мікловш Мікловшович", "Гулачі Гейза Людвигович",
  "Жердєв Ігор Сергійович", "Зотов Дмитрий Иванович", "Осташук Руслан
  Васильович", "Шувар Андрій Йосифович", "Мельниченко Оксана Михайлівна",
  "Цагареішвілі Георгій", "Чередніченко Юрій Анатолійович", "Ревенко Наталя
  Олександрівна", "Лоцман Геннадій Григорович", "Садридинов Тахир Шамсидинович",
  "Садридинов Рустамджан Шамсидинович", "Марин Григорий Афанасьевич",
  "Чередніченко Анатолій Григорович", "Недозимованний Ігор Сергійович",
  "Алексейчук Лілія Олексіївна", "Кащі Олександр Радіонович", "Сухань Александр
  Яковлевич", "Макаруша Наталя Миколаївна", "Гаркавенко Александр Михайлович",
  "Ундір Віталій Олексадрович", "Женевський Сергій Юрійович", "Мєєровіч Юрій
  Михайлович", "Кірюхін Дмитро Євгенійович", "Калініченко Наталія Євгенівна",
  "Титов Николай Ильич", "Забелін Віктор Васильович", "Ставицкий Николай
  Павлович", "Андрійчук Неоніла Вячеславівна", "Янчук Николай Андреевич", "Заіц
  Маріан Дмитрович", "Мельничук Віталій Кузьмич", "Гайничеру Михайло Іванович",
  "Блауш Дмитрий Ярославович") %>%
  str_replace("\\n", "") %>% str_replace("  ", " ")

# deps_not_in[deps_not_in%in%names_to_change] <- names_changed
deps_not_in <- deps_not_in %>% str_replace("- ", "-") %>% 
  str_replace("'", "’")

other_deps <- vector("list", length(deps_not_in))
pb <- txtProgressBar(min = 0, max = length(other_deps), initial = 0) 
for(i in seq_along(other_deps)){
  other_deps[[i]] <- download_declarations(deps_not_in[i], declaration_year = "2017",
                                           doc_type = "Щорічна", region_value = deps_not_in_obl[i]) %>% 
    get_corrected()
  setTxtProgressBar(pb,i)
}
table(unlist(map(other_deps, length)))

other_deps_copy <- other_deps

# delete deputies with false names

index_1 <- integer()
index_2 <- integer()
for(i in seq_along(other_deps_copy)){
  length_i <- length(other_deps_copy[[i]])
  if(length_i > 0) {
  for(j in 1:length_i){
    fullname <- with(other_deps_copy[[i]][[j]]$infocard, 
                     c(last_name, first_name, patronymic)) %>% 
      paste(collapse = " ") %>% 
      str_replace("'", "’") %>% 
      str_replace("'", "’") %>%
      str_replace("Фоп ", "") %>%
      str_trim()
    if(fullname != deps_not_in[i]){
      print(i)
      print(j)
      print(fullname)
      index_1 <- c(index_1, i)
      index_2 <- c(index_2, j)
    }
  }
  }
}

index_2 <- split(index_2, index_1) %>% map(~unique(.))
index_1 <- unique(index_1)

other_deps_copy[index_1] <- map2(other_deps_copy[index_1], 
                                 index_2, 
                                 function(x, y){
  x <- x[-y]
})

# delete duplicated elements

list_duplicated <- vector("list", length(other_deps))
list_corrected <- vector("list", length(other_deps))
for(i in seq_along(other_deps_copy)){
  length_i <- length(other_deps_copy[[i]])
  if(length_i > 1) {
    basic_data <- integer()
    for(j in 1:length_i){
      basic_data <- c(basic_data,
                paste(with(other_deps_copy[[i]][[j]]$infocard, 
                        c(last_name, first_name, patronymic, office, position)), collapse = ", "))
      index_dup <- which(basic_data == basic_data[duplicated(basic_data)]) %>% unlist()
  }
    list_duplicated[[i]] <- index_dup  
    list_corrected[[i]] <- map(index_dup, ~other_deps_copy[[i]][[.]]$infocard$is_corrected)  
  }
}

index_corrected <- which(unlist(map(list_corrected, ~length(.) > 0)))
index_duplicated <- which(unlist(map(list_duplicated, ~length(.) > 0)))
list_corrected <- map(list_corrected, unlist)
index_to_delete <- map2(list_duplicated[index_duplicated], list_corrected[index_corrected],
     ~.x[!.y]) %>% unlist()

other_deps_copy[index_duplicated] <- map2(other_deps_copy[index_duplicated], 
                                 index_to_delete, 
                                 function(x, y){
                                   x <- x[-y]
                                 })
map(index_duplicated, ~length(other_deps_copy[[.]]))

other_deps_copy <- map(other_deps_copy, 1)

# join both lists
all_deps <- c(dep_decl, other_deps_copy)
# exclude sublists with empty infocards
index_to_exclude <- which(map(all_deps, ~length(.[["infocard"]])) %>% unlist() == 0)
all_deps <- all_deps[-index_to_exclude]

# steps to data frames

steps_to_write <- get_all_steps(all_deps)

worksheet_names <- c("Інфо", "Члени сім'ї", "Нерухомість",
                           "Незавершена нерухомість", "Цінні предмети", "Транспорт", 
                           "Цінні папери", "Корпоративні права",
                           "Компанії", "Нематеріальні активи", "Доходи", "Грошові активи",
                           "Зобов'язання", "Видатки", "Сумісництво", "Членство")

stepswb <- createWorkbook("output/obl_2017.xlsx")
for(i in c(1:16)){
  addWorksheet(stepswb, worksheet_names[i])
  writeDataTable(stepswb, worksheet_names[i], steps_to_write[[i]], withFilter = F, rowNames = F)
}

saveWorkbook(stepswb, "output/obl_2017.xlsx", overwrite = TRUE)
obldep %>% arrange(fullname) %>% select(fullname, council)

# by oblast

obldep %>% select(fullname, council)
names_to_change <- names_to_change[which(names_to_change %in% obldep$fullname[obldep$fullname%in%names_to_change])]
names_changed <- names_changed[which(names_to_change %in% obldep$fullname[obldep$fullname%in%names_to_change])]
head(obldep$fullname, 100)
head(steps_to_write[[1]]$fullname, 100)
obldep$fullname[obldep$fullname %in% names_to_change] <- names_changed
obldep$fullname <- obldep$fullname %>% 
  str_replace_all("’", "'") %>% 
  str_replace_all("- ", "-") 
steps_to_write[[1]]$fullname <- steps_to_write[[1]]$fullname %>% 
  str_replace_all("Фоп ", "")

df_with_councils <- steps_to_write[[1]] %>% select(fullname, id) %>%
  left_join(select(obldep, fullname, council))
# sum(is.na(df_with_councils$council))
# test <- df_with_councils[is.na(df_with_councils$council),]
councils_matched <- map(all_deps, ~.[["infocard"]]$id) %>% plyr::ldply() %>% 
  rename(id = "V1") %>% 
  left_join(df_with_councils, by = "id") %>% 
  distinct(id, council)
# !!! this is to change after the region is available! duplicated FIOs
# the result may be incorrect
dupl <- councils_matched[councils_matched$id %in% councils_matched$id[which(duplicated(councils_matched$id))],] %>% 
  left_join(df_with_councils) %>% 
  filter(council != "Рівненська обласна рада")
councils_matched <- anti_join(councils_matched, dupl, by = c("id", "council"))

all_deps_splitted <- split(all_deps, councils_matched$council)
map(paste0("output/", names(all_deps_splitted)), dir.create)

steps_to_write_by_council <- map(all_deps_splitted, get_all_steps)
for(i in seq_along(steps_to_write_by_council)){
  for(j in seq_along(steps_to_write_by_council[[1]]))
    write.csv(steps_to_write_by_council[[i]][[j]], file = paste0(paste0("output/", dir("output")[grep("рада$", dir("output"))][i], "/step", j, ".csv")),
              row.names = F, fileEncoding = "UTF-8")
}

worksheet_names <- c("Інфо", "Члени сім'ї", "Нерухомість",
                     "Незавершена нерухомість", "Цінні предмети", "Транспорт", 
                     "Цінні папери", "Корпоративні права",
                     "Компанії", "Нематеріальні активи", "Доходи", "Грошові активи",
                     "Зобов'язання", "Видатки", "Сумісництво", "Членство")

# to xls

# create empty df where nrow == 0
for(i in seq_along(steps_to_write_by_council)){
  for(j in seq_along(steps_to_write_by_council[[i]]))
    if(nrow(steps_to_write_by_council[[i]][[j]]) == 0){
      steps_to_write_by_council[[i]][[j]] <- data.frame(fullname = character()) 
    }
}

stringnames <- paste0("output/", dir("output")[grepl("обласна рада$", dir("output"))], ".xlsx")

for(i in seq_along(steps_to_write_by_council)){
  stepswb <- createWorkbook(stringnames[i])
  for(j in c(1:16)){
    addWorksheet(stepswb, worksheet_names[j])
    writeDataTable(stepswb, worksheet_names[j], steps_to_write_by_council[[i]][[j]], withFilter = F, rowNames = F)
  }
  saveWorkbook(stepswb, stringnames[i], overwrite = TRUE)
}

# write links to correct manually if needed

one_council_info <- steps_to_write_by_council[["Житомирська обласна рада"]][["step1"]] %>% 
  mutate(id = str_replace(id, "nacp_", "https://public.nazk.gov.ua/declaration/")) %>% 
  select(fullname, id)

stepswb <- createWorkbook("data/Житомирська обласна рада декларації.xlsx")
addWorksheet(stepswb, "Декларації лінки")
writeDataTable(stepswb, "Декларації лінки", one_council_info, withFilter = F, rowNames = F)
saveWorkbook(stepswb, "data/Житомирська обласна рада декларації.xlsx", overwrite = TRUE)
