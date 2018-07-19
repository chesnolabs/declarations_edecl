library(tidyverse)
library(readxl)
library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")

# Спершу запускаємо edecl_summarize_functions.R

dir_steps <- "data/mps_2017" # insert directory with steps here

# Доходи

factions <- get_factions_open()
income <- read_csv(paste0(dir_steps, "/step11.csv")) %>% 
  mutate(fullname = str_replace(fullname, " - ", "-")) %>% 
  left_join(select(factions, -id), by = "fullname")

summary_income_by_mp <- income %>% 
  group_by(fullname, faction) %>% 
  summarize(total_income = sum(sizeIncome, na.rm = T)) %>% 
  arrange(desc(total_income))

summary_income_by_faction <- income %>% 
  group_by(faction) %>% 
  summarize(median_income = median(sizeIncome, na.rm = T), 
            mean_income = mean(sizeIncome, na.rm = T)) %>% 
  arrange(desc(median_income))

# Грошові активи

assets <- read_csv(paste0(dir_steps, "/step12.csv")) %>% 
  mutate(fullname = str_replace(fullname, " - ", "-")) %>% 
  left_join(select(factions, -id), by = "fullname")

summary_assets_by_mp <- assets %>% 
  group_by(fullname, faction) %>% 
  summarize(total_assets = sum(in_hryvnias, na.rm = T)) %>% 
  arrange(desc(total_assets))

summary_assets_by_faction <- assets %>% 
  group_by(faction) %>% 
  summarize(median_assets = median(in_hryvnias, na.rm = T), 
            mean_assets = mean(in_hryvnias, na.rm = T)) %>% 
  arrange(desc(median_assets))

# Корпоративні права

corporate_rights <- read_csv(paste0(dir_steps, "/step8.csv")) %>% 
  left_join(select(factions, -id), by = "fullname")

summary_corporate_rights_by_mp <- corporate_rights %>% 
  group_by(fullname, faction) %>% 
  summarize(total_corporate_rights = sum(cost, na.rm = T)) %>% 
  arrange(desc(total_corporate_rights))

# Зобов'язання

liabilities <- read_csv(paste0(dir_steps, "/step13.csv")) %>% 
  left_join(select(factions, -id), by = "fullname")

summary_liabilities_by_mp <- liabilities %>% 
  group_by(fullname, faction) %>% 
  summarize(total_liabilities = sum(in_hryvnias, na.rm = T)) %>% 
  arrange(desc(total_liabilities))

summary_liabilities_by_mp <- liabilities %>% 
  group_by(fullname, faction) %>% 
  summarize(total_liabilities = sum(in_hryvnias, na.rm = T)) %>% 
  arrange(desc(total_liabilities))

# Нерухомість

estate <- read_csv(paste0(dir_steps, "/step3.csv")) %>% 
  left_join(select(factions, -id), by = "fullname") %>% 
  mutate(fullname = str_replace(fullname, " - ", "-"))

summary_estate_by_mp <- estate %>% 
  group_by(fullname, faction, dnt_objectType_encoded) %>% 
  summarize(total_area = sum(totalArea, na.rm = T)) %>% 
  arrange(desc(total_area)) %>% 
  spread(dnt_objectType_encoded, total_area) %>% 
  mutate(total = sum(apt, dacha, garage, house, land, office, other, room, na.rm = T)) %>% 
  arrange(desc(total))

# Цінні папери

valuables <- read_csv(paste0(dir_steps, "/step5.csv")) %>% 
  left_join(select(factions, -id), by = "fullname")
valuables_vr <- valuables %>% 
  filter(acqPeriod == "Майно набуто У ПЕРІОД здійснення суб'єктом декларування діяльності із виконання функцій держави або місцевого самоврядування")

ls() %>% str_subset("summary_")
worksheet_names <- c("Доходи за депутатом", "Доходи за фракцією",
                 "Статки за депутатом", "Статки за фракцією",
                 "Корпоративні права за депутатом",
                 "Зобов'язання за депутатом", "Нерухомість за депутатом")

list_dfs <- list(summary_income_by_mp, summary_income_by_faction, 
              summary_assets_by_mp, summary_assets_by_faction,
              summary_corporate_rights_by_mp,
              summary_liabilities_by_mp, summary_estate_by_mp)

dir.create("output/summaries")
stepswb <- createWorkbook("output/summaries/mps_2017_summaries.xlsx")
for(i in seq_along(worksheet_names)){
  addWorksheet(stepswb, worksheet_names[i])
  writeDataTable(stepswb, worksheet_names[i], list_dfs[[i]], withFilter = F, rowNames = F)
}
saveWorkbook(stepswb, "output/summaries/mps_2017_summaries.xlsx", overwrite = TRUE)

# Порівняння з 2016 роком

factions <- rbind(factions, 
                  c(id = "8757", fullname = "Денісова Людмила Леонтіївна", faction = "Народний фронт"),
                  c(id = "15674", fullname = "Пацкан Валерій Васильович", faction = "Блок Петра Порошенка"))

income_16 <- read_excel("data/mps_2016.xlsx", sheet = "Доходи") %>% 
  left_join(select(factions, -id), by = "fullname")

income_16_summary_by_mp <- summarize_income_by_mp(income_16) %>% 
  rename(total_income_16 = total_income)
income_16_summary_by_faction <- summarize_income_by_faction(income_16) %>% 
  rename(median_income_16 = median_income, mean_income_16 = mean_income)

assets_16 <- read_excel("data/mps_2016.xlsx", sheet = "Грошові активи") %>% 
  left_join(select(factions, -id), by = "fullname")

assets_16_summary_by_mp <- summarize_assets_by_mp(assets_16) %>% 
  rename(total_assets_16 = total_assets)
assets_16_summary_by_mp$total_assets_16[assets_16_summary_by_mp$fullname == "Урбанський Олександр Ігорович"] + 139081133

assets_16_summary_by_faction <- summarize_assets_by_faction(assets_16) %>% 
  rename(median_assets_16 = median_assets, mean_assets_16 = mean_assets)

income_difference <- summary_income_by_mp %>%
  left_join(select(income_16_summary_by_mp, -faction), by = "fullname") %>% 
  mutate(diff_income = total_income-total_income_16) %>% 
  arrange(desc(diff_income)) %>% 
  select(fullname, faction, diff_income, everything())

assets_difference <- summary_assets_by_mp %>%
  left_join(select(assets_16_summary_by_mp, -faction), by = "fullname") %>% 
  mutate(diff_assets = total_assets-total_assets_16) %>% 
  arrange(desc(diff_assets)) %>% 
  select(fullname, faction, diff_assets, everything())

income_difference_by_faction <- summary_income_by_faction %>% 
  left_join(income_16_summary_by_faction) %>% 
  mutate(diff_median_income = median_income - median_income_16,
         diff_mean_income = mean_income - mean_income_16) %>% 
  arrange(desc(diff_median_income)) %>% 
  select(faction, diff_median_income, diff_mean_income, everything())

assets_difference_by_faction <- summary_assets_by_faction %>% 
  left_join(assets_16_summary_by_faction) %>% 
  mutate(diff_median_assets = median_assets - median_assets_16,
         diff_mean_assets = mean_assets - mean_assets_16) %>% 
  arrange(desc(diff_median_assets)) %>% 
  select(faction, diff_median_assets, diff_mean_assets, everything())

worksheet_names_diff <- c("Зміна доходів", "Зміна доходів за фракцією",
                     "Зміна статків", "Зміна статків за фракцією",
                     "Цінності, період повноважень")

# estate

info_2016 <- read_excel("data/mps_2016.xlsx", sheet = "Інфо") %>% 
  mutate(fullname = str_replace(fullname, " - ", "-"))

estate_2016 <- read_excel("data/mps_2016.xlsx", sheet = "Нерухомість") %>% 
  left_join(select(factions, -id), by = "fullname") %>% 
  mutate(fullname = str_replace(fullname, " - ", "-"))

summary_estate_by_mp_2016 <- estate_2016 %>% 
  group_by(fullname, dnt_objectType_encoded) %>% 
  summarize(total_area_2016 = sum(totalArea, na.rm = T)) %>% 
  right_join(select(info_2016, fullname), by = "fullname") %>% 
  replace_na(list(total_area_2016 = 0)) %>%
  arrange(desc(total_area_2016)) %>% 
  mutate(dnt_objectType_encoded = paste0(dnt_objectType_encoded, "_2016")) %>%
  distinct() %>% 
  spread(dnt_objectType_encoded, total_area_2016) %>%
  ungroup() %>% 
  mutate(total_2016 = select(., -fullname) %>% rowSums(na.rm = T)) %>% 
  mutate_at(vars(apt_2016:other_2016), ~ifelse(is.na(.), 0, .)) %>% 
  arrange(desc(total_2016)) %>% 
  select(-NA_2016) %>% 
  mutate(fullname = str_replace(fullname, "Николай Алексеевич", "Микола Олексійович"))

estate_diff <- summary_estate_by_mp %>% 
  full_join(summary_estate_by_mp_2016, by = "fullname") %>% 
  mutate(apt_diff = apt - apt_2016,
         house_diff = house - house_2016,
         land_diff = land - land_2016,
         dacha_diff = dacha - dacha_2016,
         garage_diff = garage - garage_2016,
         office_diff = office - office_2016,
         other_diff = other - other_2016,
         total_diff = total - total_2016) %>% 
  arrange(desc(land_diff))

names_order <- c("fullname", sort(c(names(estate_diff)[!grepl("_", names(estate_diff))][-1],
                                    names(estate_diff)[grepl("2016", names(estate_diff))],
                                    names(estate_diff)[grepl("diff", names(estate_diff))])))

estate_diff <- estate_diff[, names_order]

list_diff_dfs <- list(income_difference, income_difference_by_faction, 
                 assets_difference, assets_difference_by_faction,
                 estate_diff)

stepswb <- createWorkbook("output/summaries/mps_2017_diff_summaries.xlsx")
for(i in seq_along(worksheet_names_diff)){
  addWorksheet(stepswb, worksheet_names_diff[i])
  writeDataTable(stepswb, worksheet_names_diff[i], list_diff_dfs[[i]], withFilter = F, rowNames = F)
}
saveWorkbook(stepswb, "output/summaries/mps_2017_diff_summaries.xlsx", overwrite = TRUE)

