library(stringr)
library(tidyverse)

get_mps_names <- function(){
  actual_mps <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mps08-data.csv") %>% 
    filter(is.na(date_end)) %>% 
    select(full_name) %>%
    unlist()
  return(actual_mps)
}

clean_names <- function(string){
  string <- str_replace(string, " - ", "-") %>% 
    str_replace("- ", "-") %>% 
    str_trim()
  return(string)
}

get_in_hryvnias <- function(list_steps){

  currencies <- character()
  if(nrow(list_steps[["step12"]])>0){
    currencies12 <- list_steps[["step12"]] %>% 
      #    filter(assetsCurrency!="UAH") %>% 
      select(assetsCurrency) %>% 
      unlist() %>% unique()
    currencies <- c(currencies, currencies12)
  }
  
  if(nrow(list_steps[["step13"]])>0){
    currencies13 <- list_steps[["step13"]] %>% 
      #   filter(currency!="UAH") %>% 
      select(currency) %>% 
      unlist() %>% unique()
    currencies <- c(currencies, currencies13) %>% unique()
  }
  
  decl_years <- as.integer(str_sub(list_steps[["step1"]]$created_date, 1, 4))-1
  ref_year <- as.integer(names(table(decl_years)[which.max(table(decl_years))]))
  
if( length(unique(decl_years)) > 1 ) stop(paste0("Another declaration year: ", 
  decl_years[which(decl_years != ref_year)],
  ", ", list_steps[["step1"]]$fullname[which(decl_years != ref_year)], 
  ", row ", which(decl_years != ref_year), ", url: ",
  list_steps[["step1"]]$url[which(decl_years != ref_year)], "\n"))
  
  link_rate <- paste0("https://bank.gov.ua/NBUStatService/v1/statdirectory/exchange?date=",
                      unique(decl_years), "1231&json")

  if(length(currencies) > 0){
    all_rates <- fromJSON(link_rate) %>% 
      #  filter(cc %in% currencies) %>% 
      select(cc, rate) %>% 
      rbind(data.frame(cc = "UAH", rate = 1, stringsAsFactors = F))
  }
  
  if(nrow(list_steps[["step12"]])>0){
    list_steps[["step12"]] <- list_steps[["step12"]] %>% 
      left_join(all_rates, by = c("assetsCurrency" = "cc")) %>% 
      mutate(in_hryvnias = round(sizeAssets*rate, 2)) %>% 
      select(-rate)
  }
  
  if(nrow(list_steps[["step13"]])>0){
    list_steps[["step13"]] <- list_steps[["step13"]] %>% 
      left_join(all_rates, by = c("currency" = "cc")) %>% 
      mutate(in_hryvnias = round(sizeObligation*rate, 2)) %>% 
      select(-rate)
  }
  return(list_steps)
}

get_all_steps <- function(list_decl){
  step_dfs <- vector("list", 15)
  for(i in seq_along(step_dfs)){
    step_dfs[[i]] <- step_to_df(list_decl, i+1)[[1]]
  #  step_dfs[[i]]$fullname <- clean_names(step_dfs[[i]]$fullname)
  }

  names(step_dfs) <- paste0("step", c(2:16))
  
  steps_to_write <- vector("list", 16)
  names(steps_to_write) <- paste0("step", c(1:16))
  
  # 1. info
  
  steps_to_write[[1]] <- list_decl %>% extract_info() %>%
    select(-first_name, -patronymic, -last_name) %>% 
    distinct() %>% 
    arrange(fullname)
  
  # 2. relatives
  
  if(nrow(step_dfs[["step2"]]) > 0){
  step_dfs[["step2"]] <- step_dfs[["step2"]] %>% 
    mutate(fullname_relative = str_to_title(clean_names(paste(lastname, firstname, middlename)))) %>% 
    arrange(fullname, fullname_relative) 
  
  steps_to_write[[2]] <- step_dfs[["step2"]] %>% 
    select(id, fullname, fullname_relative, subjectRelation, object_id)
  } else {
    steps_to_write[[2]] <- data.frame(id = character(),
                                      fullname = character(),
                                      fullname_relative = character(),
                                      subjectRelation = character(),
                                      object_id = character(),
                                      stringsAsFactors = F)
  }
  # steps 3 - 16
  
  # steps_to_write[c(3:16)] <- map(step_dfs[c(2:15)], create_step_df)
  persons_lookup <- steps_to_write[[2]] %>% 
    select(-id, -fullname) %>% 
    rename(relative_id = object_id)
  
  for(i in 3:16){
      steps_to_write[[i]] <- as.data.frame(step_dfs[[i-1]])
      if("person" %in% colnames(step_dfs[[i-1]])){
        steps_to_write[[i]] <- steps_to_write[[i]] %>%
          left_join(persons_lookup, by = c("person" = "relative_id")) %>%
          rename(declarant = fullname_relative) %>%
          mutate(declarant = ifelse(is.na(declarant), fullname, declarant)) %>%
          arrange(fullname)
      }
  }

  steps_to_write <- get_in_hryvnias(steps_to_write)
 
  return(steps_to_write)
}

