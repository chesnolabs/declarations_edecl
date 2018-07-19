library(tidyverse)
library(readxl)
library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
source("edecl_summarize_functions.R")

council_string <- "Харківська обласна рада"
dir_steps <- paste0("output/by_id/", council_string)

info <- read_csv(paste0(dir_steps, "/step1.csv"))

income <- read_csv(paste0(dir_steps, "/step11.csv")) %>% 
  mutate(sizeIncome = as.numeric(sizeIncome))

table(income$objectType)
table(income$otherObjectType)
income_to_exclude <- c("Дохід від відчуження нерухомого майна",
                       "Дохід від відчуження рухомого майна ( крім цінних паперів та корпоративних прав)",
                       "Страхові виплати", "Подарунок у негрошовій формі",
                       "Спадщина", "Приз")

summary_income_by_dep <- income %>% 
  filter(!objectType %in% income_to_exclude) %>% 
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "бонус"))) %>% 
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "Оплата за договором відступлення права вимоги"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[бБ]онус"))) %>% 
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]родаж"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]овернення"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]оверенення"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[кК]редит|КРЕДИТ"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]озик"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]оворотн.*фінанс.*"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[сС]трахування"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[вВ]ідступлення права вимоги"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[вВ]инагорода за передачу права власності"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[дД]охід від відчуження"))) %>%
  group_by(fullname) %>% 
  summarize(total_income = sum(sizeIncome, na.rm = T)) %>%
  right_join(select(info, fullname), by = "fullname") %>% 
  replace_na(list(total_income = 0)) %>% 
  arrange(desc(total_income))

info_2016 <- read_csv(paste0("output/by_id/2016/", council_string, "/step1.csv"))

income_2016 <- read_csv(paste0("output/by_id/2016/", council_string, "/step11.csv")) %>% 
  mutate(sizeIncome = as.numeric(sizeIncome))
# income_2016 <- read_excel(paste0("output/2016/", council_string, ".xlsx"),
#                           sheet = "Доходи")
summary_income_by_dep_2016 <- income_2016 %>% 
  filter(!objectType %in% income_to_exclude) %>% 
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "бонус"))) %>% 
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "Оплата за договором відступлення права вимоги"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[бБ]онус"))) %>% 
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]родаж"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]овернення"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]оверенення"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[кК]редит|КРЕДИТ"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]озик"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]оворотн.*фінанс.*"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[сС]трахування"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[вВ]ідступлення права вимоги"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[вВ]инагорода за передачу права власності"))) %>%
  group_by(fullname) %>% 
  summarize(total_income_2016 = sum(sizeIncome, na.rm = T)) %>%
  right_join(select(info_2016, fullname), by = "fullname") %>% 
  replace_na(list(total_income_2016 = 0)) %>% 
  arrange(desc(total_income_2016)) %>% 
  mutate(fullname = str_replace(fullname, "Николай Алексеевич", "Микола Олексійович"))

income_diff <- summary_income_by_dep %>% 
  full_join(summary_income_by_dep_2016, by = "fullname") %>% 
  mutate(income_diff = total_income - total_income_2016) %>% 
  arrange(desc(income_diff)) %>% 
  rename(income = total_income, income_2016 = total_income_2016)

assets <- read_csv(paste0(dir_steps, "/step12.csv"))

summary_assets_by_dep <- assets %>% 
  group_by(fullname) %>% 
  summarize(total_assets = sum(in_hryvnias, na.rm = T)) %>% 
  right_join(select(info, fullname), by = "fullname") %>% 
  replace_na(list(total_assets = 0)) %>% 
  arrange(desc(total_assets)) 

# who holds in cash
# percent of different currencies (beware of of bitcoins etc., counted as hryvnias)

corporate_rights <- read_csv(paste0(dir_steps, "/step8.csv"))

summary_corporate_rights_by_dep <- corporate_rights %>% 
  group_by(fullname) %>% 
  summarize(total_corporate_rights = sum(cost, na.rm = T)) %>% 
  right_join(select(info, fullname), by = "fullname") %>% 
  replace_na(list(total_corporate_rights = 0)) %>% 
  arrange(desc(total_corporate_rights))
# it doesn't has much sense to calculate this by faction

assets_and_rights <- summary_assets_by_dep %>% 
  left_join(summary_corporate_rights_by_dep) %>% 
  mutate(total = total_assets + total_corporate_rights) %>% 
  arrange(desc(total))

assets_2016 <- read_csv(paste0("output/by_id/2016/", council_string, "/step12.csv"))

summary_assets_by_dep_2016 <- assets_2016 %>% 
  group_by(fullname) %>% 
  summarize(total_assets_2016 = sum(in_hryvnias, na.rm = T)) %>% 
  right_join(select(info_2016, fullname), by = "fullname") %>% 
  replace_na(list(total_assets_2016 = 0)) %>% 
  arrange(desc(total_assets_2016)) 

# who holds in cash
# percent of different currencies (beware of of bitcoins etc., counted as hryvnias)

corporate_rights <- read_csv(paste0("output/by_id/2016/", council_string, "/step8.csv"))

summary_corporate_rights_by_dep_2016 <- corporate_rights %>% 
  group_by(fullname) %>% 
  summarize(total_corporate_rights_2016 = sum(cost, na.rm = T)) %>% 
  right_join(select(info_2016, fullname), by = "fullname") %>% 
  replace_na(list(total_corporate_rights_2016 = 0)) %>% 
  arrange(desc(total_corporate_rights_2016))
# it doesn't has much sense to calculate this by faction

assets_and_rights_2016 <- summary_assets_by_dep_2016 %>% 
  left_join(summary_corporate_rights_by_dep_2016) %>% 
  mutate(total_2016 = total_assets_2016 + total_corporate_rights_2016) %>% 
  arrange(desc(total_2016)) %>% 
  mutate(fullname = str_replace(fullname, "Николай Алексеевич", "Микола Олексійович"))

assets_and_rights_diff <- assets_and_rights %>% 
  full_join(assets_and_rights_2016, by = "fullname") %>% 
  mutate(assets_diff = total_assets - total_assets_2016,
         corporate_diff = total_corporate_rights - total_corporate_rights_2016,
         total_diff = total - total_2016) %>% 
   rename(assets = total_assets, assets_2016 = total_assets_2016,
         corporate = total_corporate_rights, corporate_2016 = total_corporate_rights_2016) %>% 
  select(fullname, total, total_2016, total_diff,
         assets, assets_2016, assets_diff, 
         corporate, corporate_2016, corporate_diff) %>% 
  arrange(desc(total_diff))

# estate

estate <- read_csv(paste0("output/by_id/", council_string, "/step3.csv"))

summary_estate_by_dep <- estate %>% 
  group_by(fullname, dnt_objectType_encoded) %>% 
  summarize(total_area = sum(totalArea, na.rm = T)) %>% 
  right_join(select(info, fullname), by = "fullname") %>% 
  replace_na(list(total_area = 0)) %>%
  arrange(desc(total_area)) %>% 
  spread(dnt_objectType_encoded, total_area) %>%
  ungroup() %>% 
  mutate(total = select(., -fullname) %>% rowSums(na.rm = T)) %>% 
  mutate_at(vars(apt:other), ~ifelse(is.na(.), 0, .)) %>% 
  arrange(desc(total))

estate_2016 <- read_csv(paste0("output/by_id/2016/", council_string, "/step3.csv"))

summary_estate_by_dep_2016 <- estate_2016 %>% 
  group_by(fullname, dnt_objectType_encoded) %>% 
  summarize(total_area_2016 = sum(totalArea, na.rm = T)) %>% 
  right_join(select(info_2016, fullname), by = "fullname") %>% 
  replace_na(list(total_area_2016 = 0)) %>%
  arrange(desc(total_area_2016)) %>% 
  mutate(dnt_objectType_encoded = paste0(dnt_objectType_encoded, "_2016")) %>% 
  spread(dnt_objectType_encoded, total_area_2016) %>%
  ungroup() %>% 
  mutate(total_2016 = select(., -fullname) %>% rowSums(na.rm = T)) %>% 
  mutate_at(vars(apt_2016:other_2016), ~ifelse(is.na(.), 0, .)) %>% 
  arrange(desc(total_2016)) %>% 
  select(-NA_2016) %>% 
  mutate(fullname = str_replace(fullname, "Николай Алексеевич", "Микола Олексійович"))

estate_diff <- summary_estate_by_dep %>% 
  full_join(summary_estate_by_dep_2016, by = "fullname") %>% 
  mutate(apt_diff = apt - apt_2016,
         house_diff = house - house_2016,
         land_diff = land - land_2016,
         dacha_diff = dacha - dacha_2016,
         garage_diff = garage - garage_2016,
         office_diff = office - office_2016,
         other_diff = other - other_2016,
         total_diff = total - total_2016) %>% 
  arrange(desc(apt_diff))

names_order <- c("fullname", sort(c(names(estate_diff)[!grepl("_", names(estate_diff))][-1],
                  names(estate_diff)[grepl("2016", names(estate_diff))],
                  names(estate_diff)[grepl("diff", names(estate_diff))])))

estate_diff <- estate_diff[, names_order]

stepswb <- createWorkbook(paste0("output/summaries/Порівняння ", council_string, ".xlsx"))
addWorksheet(stepswb, "Доходи")
setColWidths(stepswb, "Доходи", cols = c(1:ncol(income_diff)),
             widths = c(30, rep(14, ncol(income_diff)-1)))
writeDataTable(stepswb, "Доходи", income_diff, withFilter = F, rowNames = F)
addWorksheet(stepswb, "Статки")
setColWidths(stepswb, "Статки", cols = c(1:ncol(assets_and_rights_diff)),
             widths = c(30, rep(14, ncol(assets_and_rights_diff)-1)))
writeDataTable(stepswb, "Статки", assets_and_rights_diff, withFilter = F, rowNames = F)
addWorksheet(stepswb, "Нерухомість")
writeDataTable(stepswb, "Нерухомість", estate_diff, withFilter = F, rowNames = F)
setColWidths(stepswb, "Нерухомість", cols = c(1:ncol(estate_diff)),
             widths = c(30, rep(14, ncol(estate_diff)-1)))

saveWorkbook(stepswb, paste0("output/summaries/Порівняння ", council_string, ".xlsx"), overwrite = TRUE)

