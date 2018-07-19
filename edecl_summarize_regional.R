library(tidyverse)
library(readxl)
library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
source("edecl_summarize_functions.R")

council_string <- "Зміївська міська рада"
dir_steps <- paste0("output/by_id/", council_string)

info <- read_csv(paste0(dir_steps, "/step1.csv"))

income <- read_csv(paste0(dir_steps, "/step11.csv")) %>% 
  mutate(sizeIncome <- as.numeric(sizeIncome))

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
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[кК]редит"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]озик"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]озичк"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[пП]оворотн.*фінанс.*"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[сС]трахування"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[вВ]ідступлення права вимоги"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[вВ]инагорода за передачу права власності"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[вВ]артість "))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[вВ]икористання кредитної картки"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[вВ]ідзнака"))) %>%
  filter(!(objectType == "Інше"&str_detect(otherObjectType, "[дД]охід від відчуження"))) %>%
  group_by(fullname) %>% 
  summarize(total_income = sum(sizeIncome, na.rm = T)) %>%
  right_join(select(info, fullname), by = "fullname") %>% 
  replace_na(list(total_income = 0)) %>% 
  arrange(desc(total_income))

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

if(ncol(corporate_rights) == 1){
  assets_and_rights <- summary_assets_by_dep %>% 
    mutate(total_corporate_rights = rep(0, nrow(summary_assets_by_dep)),
           total = total_assets + total_corporate_rights) %>% 
    arrange(desc(total))
} else {
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
}

liabilities <- read_csv(paste0(dir_steps, "/step13.csv"))

summary_liabilities_by_dep <- liabilities %>% 
  group_by(fullname) %>% 
  summarize(total_liabilities = sum(in_hryvnias, na.rm = T)) %>% 
  right_join(select(info, fullname), by = "fullname") %>% 
  replace_na(list(total_liabilities = 0)) %>% 
  arrange(desc(total_liabilities))

estate <- read_csv(paste0(dir_steps, "/step3.csv"))

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

# find poors

family <- read_csv(paste0(dir_steps, "/step2.csv")) %>% 
  group_by(fullname) %>% 
  count() %>% 
  right_join(select(info, fullname), by = "fullname") %>% 
  replace_na(list(n = 0)) %>% 
  mutate(n = n+1) %>%
  rename(family_n = n)

# minimum_2017 <- round((1544*4 + 1624*7 + 1700)/12, 1) # average per month
minimum_2017 <- sum(1544*4 + 1624*7 + 1700)

poors <- summary_income_by_dep %>% 
  left_join(family, by = "fullname") %>% 
  mutate(necessary_income = minimum_2017*family_n) %>% 
  filter(total_income < necessary_income) %>% 
  mutate(percent_of_necessary = round(total_income/necessary_income*100, 1)) %>% 
  arrange(percent_of_necessary)

worksheet_names <- c("Доходи за депутатом", "Статки за депутатом", "Злидні", "Нерухомість за депутатом")
list_dfs <- list(summary_income_by_dep, assets_and_rights, poors,
                 summary_estate_by_dep)

stepswb <- createWorkbook(paste0("output/summaries/", council_string, " резюме.xlsx"))
for(i in seq_along(worksheet_names)){
  addWorksheet(stepswb, worksheet_names[i])
  setColWidths(stepswb, worksheet_names[i], cols = 1, widths = 30)
  writeDataTable(stepswb, worksheet_names[i], list_dfs[[i]], withFilter = F, rowNames = F)
}
saveWorkbook(stepswb, paste0("output/summaries/", council_string, " резюме.xlsx"), overwrite = TRUE)
