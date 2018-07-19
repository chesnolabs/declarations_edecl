get_factions_open <- function(){
  posts <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mp-posts_ids.csv")
  posts_ids <- read_tsv("http://data.rada.gov.ua/ogd/mps/skl8/mp-posts_unit.txt", 
                        locale(encoding = "windows-1251"), col_names = F, col_types = NULL) %>% 
    rename(unit_id = X1, unit = X2)
  mps <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mps08-data.csv") %>% 
    mutate(shortname = paste(last_name, short_name))
  
  factions_full <- posts %>% 
    left_join(mps[, c("id", "full_name", "shortname")], by = c("mp_id" = "id")) %>% 
    left_join(posts_ids) %>% 
    filter(unit_type %in% c("grp", "fra")) %>% 
    select(mp_id, full_name, shortname, unit)
  
  factions_df <-  mps %>% 
    filter(is.na(resignation_text)) %>% 
    select(id, full_name, shortname) %>% 
    left_join(factions_full, by = c("id" = "mp_id", "full_name", "shortname")) %>% 
    mutate(unit = ifelse(is.na(unit), "Позафракційні", unit)) %>% 
    rename(faction = unit, fullname = full_name) %>% 
    mutate(faction = recode(faction,
                            `Група "Воля народу"` = "Воля народу",
                            `Група "Партія "Відродження"` = "Відродження",
                            `Група "Відродження"` = "Відродження",
                            `Група "Економічний розвиток"` = "Економічний розвиток",
                            `Фракція ПАРТІЇ "БЛОК ПЕТРА ПОРОШЕНКА"` = "Блок Петра Порошенка",
                            `Фракція політичної партії "Всеукраїнське об'єднання "Батьківщина" у Верховній Раді України` = "Батьківщина",
                            `Фракція Політичної партії "НАРОДНИЙ ФРОНТ"` = "Народний фронт",
                            `Фракція Політичної партії "Опозиційний блок" у Верховній Раді України восьмого скликання` = "Опозиційний блок",
                            `Фракція Радикальної партії Олега Ляшка` = "Радикальна партія Ляшка",
                            `Фракція Політичної партії "Об'єднання "САМОПОМІЧ"` = "Самопоміч")) %>% 
    mutate(shortname = str_replace(shortname, "'", "’"))
  factions_df$shortname[factions_df$fullname == "Тимошенко Юлія Володимирівна"] <- "Тимошенко Юлія В."
  factions_df$shortname[factions_df$fullname == "Тимошенко Юрій Володимирович"] <- "Тимошенко Юрій В."
  factions_df$shortname[factions_df$fullname == "Найєм Мустафа-Масі"] <- "Найєм М. ."
  factions_df$shortname[factions_df$fullname == "Джемілєв Мустафа"] <- "Джемілєв М. ."
  
  return(factions_df)
}

summarize_income_by_mp <- function(df_income){
  df_income %>% 
    group_by(fullname, faction) %>% 
    summarize(total_income = sum(sizeIncome, na.rm = T)) %>% 
    arrange(desc(total_income))
}

summarize_income_by_faction <- function(df_income){
  df_income %>% 
    group_by(faction) %>% 
    summarize(median_income = median(sizeIncome, na.rm = T), 
              mean_income = mean(sizeIncome, na.rm = T)) %>% 
    arrange(desc(median_income))
}

summarize_assets_by_mp <- function(df_assets){
  df_assets %>% 
    group_by(fullname, faction) %>% 
    summarize(total_assets = sum(in_hryvnias, na.rm = T)) %>% 
    arrange(desc(total_assets))
}

summarize_assets_by_faction <- function(df_assets){
  df_assets %>% 
    group_by(faction) %>% 
    summarize(median_assets = median(in_hryvnias, na.rm = T), 
              mean_assets = mean(in_hryvnias, na.rm = T)) %>% 
    arrange(desc(median_assets))
}

summarize_corporate_rights <- function(df_corp_rights){
  df_corp_rights %>% 
    group_by(fullname, faction) %>% 
    summarize(total_corporate_rights = sum(cost, na.rm = T)) %>% 
    arrange(desc(total_corporate_rights))
}

summarize_liabilities <- function(df_liab){
  df_liab %>% 
    group_by(fullname, faction) %>% 
    summarize(total_liabilities = sum(in_hryvnias, na.rm = T)) %>% 
    arrange(desc(total_liabilities))
}

summarize_estate <- function(df_estate){ 
  df_estate %>% 
    group_by(fullname, faction, dnt_objectType_encoded) %>% 
    summarize(total_area = sum(totalArea, na.rm = T)) %>% 
    arrange(desc(total_area)) %>% 
    spread(dnt_objectType_encoded, total_area) %>% 
    mutate(total = sum(apt, dacha, garage, house, land, office, other, room, na.rm = T)) %>% 
    arrange(desc(total))
}
