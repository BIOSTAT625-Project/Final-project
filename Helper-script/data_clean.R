# Working directory: ~/Dropbox (University of Michigan)/Final project
setwd("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/")

library(dplyr)
library(tidyr)
library(readxl)
library(tools)
library(lme4)
library(lubridate)

state_list <- read.csv("state.csv")

# ===== GOOGLE TRENDS DATA =====

keywords_list <- c("depression", "depressed", "empty", "fatigue", 
                   "feeling sad", "guilty", "insomnia", "suicide")

# Harmonize relative search volumes of different time periods
relative_convert <- function(df_late, df_early) {
  df <- df_early %>%
    full_join(df_late,
              by = c("week" = "week", "keyword" = "keyword", "region" = "region"),
              suffix = c("_early", ""))
  
  lm_mods <- summary(lmList(formula = relative ~ relative_early - 1 | region,
                            data = df))$coefficients[, "Estimate", ] %>%
    data.frame(slope = .) %>% 
    mutate(region = rownames(.), 
           slope = replace_na(data = slope, replace = 1))
  
  df <- df %>%
    left_join(lm_mods, by = c("region" = "region")) %>%
    mutate(relative = ifelse(test = is.na(relative),
                             yes = round(x = slope * relative_early, digits = 0),
                             no = relative)) %>% 
    select(-relative_early, -slope) %>% 
    arrange(region, week)
  return(df)
}

# Combine data from different years
combine_vol <- function(keyword) {
  state_1722 <- read.csv(paste0(keyword, "_by_state.csv")) %>% 
    rename(week = Week, relative = Relative, absolute = Absolute, region = State) %>% 
    mutate(week = ymd(week), keyword = keyword, 
           region = gsub(pattern = "US-", replacement = "", x = region)) %>% 
    as_tibble()
  
  for (i in 4:1) {
    state_early <- read.csv(paste0("Clean_data/", 
                                   keyword, "_", sprintf("%02d", 4 * i), "_", 
                                   sprintf("%02d", 4 * (i + 1)), ".csv")) %>% 
      filter(geo != "US") %>% 
      select(date, hits, keyword, geo) %>% 
      rename(week = date, relative = hits, region = geo) %>% 
      mutate(week = mdy(week), 
             region = gsub(pattern = "US-", replacement = "", x = region)) %>% 
      as_tibble()
    
    state_1722 <- relative_convert(df_late = state_1722, df_early = state_early)
  }
  
  return(state_1722)
}

# Convert relative search volume to absolute search volume
absolute_convert <- function(df) {
  lm_mods <- summary(lmList(formula = absolute ~ relative - 1 | region,
                            data = df))$coefficients[, "Estimate", ] %>%
    data.frame(slope = .) %>%
    mutate(region = rownames(.), 
           slope = replace_na(data = slope, replace = 1))
  
  df <- df %>%
    left_join(lm_mods, by = c("region" = "region")) %>%
    mutate(absolute = ifelse(test = is.na(absolute),
                             yes = round(x = slope * relative, digits = 0),
                             no = absolute)) %>%
    select(-relative, -slope) %>% 
    group_by(region) %>% 
    distinct() %>% 
    ungroup() %>% 
    arrange(region, week)
  
  return(df)
}

create_keyword_data <- function(keyword) {
  # Search volume of US
  us_data <- read.csv(paste0("Clean_data/", toTitleCase(keyword), "_absolute.csv")) %>% 
    rename(week = Week, absolute = Absolute_mean) %>% 
    mutate(week = ymd(week), absolute = round(absolute, digits = 0), keyword = keyword, region = "US")
  
  # Search volume of each state
  state_data <- combine_vol(keyword = keyword) %>% 
    absolute_convert()
  
  # Combine two datasets
  return(rbind(us_data, state_data))
}

combine_all_trends <- function() {
  df <- data.frame()
  for (kw in keywords_list) {
    df <- rbind(df, create_keyword_data(kw))
  }
  
  write.csv(df, "Clean_data/google_trends_all.csv")
}

# ===== WEATHER DATA =====

combine_weather <- function() {
  us_weather <- read.csv("Clean_data/US Weather 2004-01-01 to 2022-12-01.csv") %>% 
    mutate(datetime = as.Date(datetime, tryFormats = c("%m/%d/%y", "%Y-%m-%d", "%Y/%m/%d")))
  for (st in state_list$state) {
    state_weather <- read.csv(paste0("Clean_data/", st, 
                                     " Weather 2004-01-01 to 2022-12-01.csv")) %>% 
      mutate(datetime = as.Date(datetime, tryFormats = c("%m/%d/%y", "%Y-%m-%d", "%Y/%m/%d")))
    us_weather <- rbind(us_weather, state_weather)
  }
  
  write.csv(us_weather, "Clean_data/weather.csv")
  #return(us_weather)
}

