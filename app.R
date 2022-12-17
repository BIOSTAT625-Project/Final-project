# Working directory: ~/Desktop/BIOSTAT 625 - Computing with Big Data/Final Project
# setwd("~/Desktop/BIOSTAT 625 - Computing with Big Data/Final Project")
setwd("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/")

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tools)
library(readxl)
library(usmap)
library(rsconnect)
library(rcartocolor)
library(showtext)
library(lubridate)

# setAccountInfo(name = "conchaespina",
#                token = "12CE5D455F897C2F0EF8017E62295CE9",
#                secret = "kWvE26n6im+fK7rnDyyU8L3o9Bq6joXb1eSe6Trz")
# deployApp("")

state_list <- read.csv("state.csv")
keywords_list <- c("depression", "depressed", "empty", "fatigue", 
                   "feeling sad", "guilty", "insomnia", "suicide")
aux_data_list <- c("weather", "AQI", "income", "education", "sunlight", "unemployment rate", "median age")

dat <- read.csv("Clean_data/google_trends_all.csv") %>% 
  mutate(week = as.Date(week)) %>% 
  filter(complete.cases(.))

get_aux <- function(aux_type, time_start, time_end, var_name = NULL) {
  
  if (aux_type == "weather") {
    aux <- read.csv("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/weather.csv") %>% 
      select("name", "datetime", all_of(var_name)) %>% 
      filter((datetime >= time_start) & (datetime <= time_end)) %>% 
      group_by(name) %>% 
      summarize(avg = as.character(round(mean(eval(parse(text = var_name)), na.rm = TRUE), 
                                         digits = 2))) 
    aux <- aux %>% 
      right_join(usmapdata::centroid_labels("states"), by = c("name" = "full"))
  }
  
  else if (aux_type == "AQI") {
    aux <- read.csv("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/aqi_complete.csv") %>% 
      select(state, date_local, Mean_aqi) %>% 
      mutate(date_local = as.Date(date_local, format = "%m/%d/%y")) %>% 
      filter((date_local >= time_start) & (date_local <= time_end)) %>% 
      group_by(state) %>% 
      summarize(avg = as.character(round(mean(Mean_aqi, na.rm = TRUE), 
                                         digits = 2)))
    aux <- aux %>% 
      right_join(usmapdata::centroid_labels("states"), by = c("state" = "full"))
  }
  
  else if (aux_type == "income") {
    aux <- read.csv("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/personal_income_by_state.csv") %>% 
      tidyr::pivot_longer(cols = !DATE) %>% 
      filter((DATE >= year(ymd(time_start))) & (DATE <= year(ymd(time_end)))) %>% 
      group_by(name) %>% 
      summarize(avg = as.character(round(mean(value, na.rm = TRUE), 
                                         digits = 2)))
    aux <- aux %>% 
      right_join(usmapdata::centroid_labels("states"), by = c("name" = "abbr"))
  }
  
  else if (aux_type == "education") {
    aux <- read.csv("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/EducationReport 2016-2020 (average).csv") %>% 
      mutate(avg = round(eval(parse(text = var_name)), digits = 2))
    aux <- aux %>% 
      right_join(usmapdata::centroid_labels("states"), by = c("State" = "full"))
  }
  
  else if (aux_type == "sunlight") {
    aux <- read.csv("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/Daily Sunlight (2004-2011).csv") %>% 
      group_by(State) %>% 
      summarize(avg = as.character(round(mean(Avg.Daily.Sunlight, na.rm = TRUE), digits = 2)))
    aux <- aux %>% 
      right_join(usmapdata::centroid_labels("states"), by = c("State" = "full"))
  }
  
  else if (aux_type == "unemployment rate") {
    aux <- read.csv("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/STATE-UI-RATES 2009-2022.csv") %>% 
      mutate(Month = sapply(X = Month, 
                            FUN = function(x) {which(month.abb == substr(x, start = 1, stop = 3))})) %>% 
      mutate(datetime = mapply(FUN = make_date, year = Year, month = Month)) %>% 
      mutate(datetime = as_date(datetime)) %>% 
      select(-Year, -Month) %>% 
      filter((datetime >= time_start) & (datetime <= time_end)) %>% 
      tidyr::pivot_longer(cols = !datetime) %>% 
      mutate(name = sapply(X = name, 
                           FUN = function(x) {paste(strsplit(x = x, split = "\\.")[[1]], 
                                                    collapse = " ")})) %>% 
      group_by(name) %>% 
      summarize(avg = as.character(round(mean(value, na.rm = TRUE), digits = 2)))
    
    aux <- aux %>% 
      right_join(usmapdata::centroid_labels("states"), by = c("name" = "full"))
  }
  
  else {
    aux <- read.csv("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/Median Age by state 2022.csv") %>% 
      rename(avg = Median.Age) %>%
      right_join(usmapdata::centroid_labels("states"), by = c("State" = "abbr"))
  }
  
  
  return(aux)
}

font_add_google(name = "News Cycle")
showtext_auto()

# Define UI for app ----
ui <- fluidPage(
  # Theme
  theme = bs_theme(bootswatch = "journal", 
                   base_font = "News Cycle"),
  
  # App title ----
  titlePanel("Population Depression Data from Google Trends"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      h2("Dashboard"),
      
      helpText("Create line graph for temporal trends or map for regional distribution 
               about Google search indices"),
      
      # Input: Radio button for plot types ----
      selectInput(
        inputId = "plot_type", 
        label = "Plot type", 
        choices = list(`Interest over time` = "time", `Interest by region` = "region")
      ),
      
      # Settings ----
      selectInput(
        inputId = "keyword", 
        label = "Keyword", 
        choices = split(x = keywords_list, f = toTitleCase(keywords_list)), 
        selected = "depression"
      ),
      
      dateRangeInput(
        inputId = "time_range", 
        label = "Time range", 
        start = "2020-01-01", 
        end = "2022-10-31", 
        min = "2004-01-01"
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'time'",
        selectInput(
          inputId = "region", 
          label = "Region", 
          choices = c(`United States` = "US", 
                      split(x = state_list$code, f = state_list$state)), 
          selected = "US"
        )
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'region'",
        selectInput(
          inputId = "indicator", 
          label = "Other indicators", 
          choices = c(`None` = "None", 
                      split(x = aux_data_list, f = toTitleCase(aux_data_list))), 
          selected = "None"
        )
      ),
      
      conditionalPanel(
        condition = "input.indicator == 'weather'",
        selectInput(
          inputId = "weather", 
          label = "Weather", 
          choices = list(`Average temperature` = "temp", 
                         `Average feels like temperature` = "feelslike", 
                         `Average dew point` = "dew", 
                         `Average humidity` = "humidity", 
                         `Average precipitation` = "precip", 
                         `Average windspeed` = "windspeed", 
                         `Average visibility` = "visibility")
        )
      ),
      
      conditionalPanel(
        condition = "input.indicator == 'education'",
        selectInput(
          inputId = "education", 
          label = "Education", 
          choices = list(`Complete college` = "Complete_college",	
                         `Complete high school only` = "Complete_high_school_only", 
                         `Not complete high school` = "Not_complete_high_school")
        )
      ),
      
      conditionalPanel(
        condition = "input.time_range[1] > '2022-11-20'", 
        selectInput(
          inputId = "algorithm", 
          label = "Prediction algorithm", 
          choices = list(`None` = "None"), 
          selected = "None"
        )
      ),
      
      width = 3
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Line graph ----
      conditionalPanel(
        condition = "input.plot_type == 'time'", 
        plotly::plotlyOutput(outputId = "line_graph")
      ),
      
      # Output: Map ----
      conditionalPanel(
        condition = "input.plot_type == 'region'", 
        plotly::plotlyOutput(outputId = "map", height = 500)
      )
      
    )
  )
)

# Define server logic required to draw plots ----
server <- function(input, output) {
  
  output$line_graph <- plotly::renderPlotly(
    expr = {
      dat %>% 
        filter((week >= input$time_range[1]) & (week <= input$time_range[2])) %>% 
        filter(keyword == input$keyword) %>% 
        filter(region == input$region) %>% 
        ggplot(data = ., mapping = aes(x = week, y = absolute, group = 1)) + 
        geom_line(color = "#63a6a0", size = 1) + 
        labs(x = "Week", y = "Absolute search volume") + 
        scale_x_date(breaks = seq.Date(from = input$time_range[1], 
                                       to = input$time_range[2], 
                                       length.out = 20)) + 
        theme_bw(base_size = 13) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
              text = element_text(family = "News Cycle"), 
              plot.margin = margin(0, 0, 0, 1, "cm"))
    }
  )
  
  output$map <- plotly::renderPlotly(
    expr = {
      # Dataset to create basic map
      gt_data <- dat %>% 
        filter((week >= input$time_range[1]) & (week <= input$time_range[2])) %>% 
        filter(keyword == input$keyword) %>% 
        filter(region != "US") %>% 
        group_by(region) %>% 
        summarize(absolute = sum(absolute)) %>% 
        left_join(state_list, by = c("region" = "code"))
      
      # Add prediction
      
      # Basic map
      map_plot <- plot_usmap(data = gt_data, values = "absolute", regions = "states", 
                             theme = theme_classic(), color = "#bcbcbc", size = 0.6) + 
        scale_fill_carto_c(palette = "Mint", name = "Absolute\nsearch\nvolume") + 
        theme_bw(base_size = 13) + 
        theme(axis.title = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank(), 
              text = element_text(family = "News Cycle"), 
              plot.margin = margin(0, 0, 0, 0, "cm")) 
      
      # Add labels for auxiliary indicators
      if (input$indicator != "None") {
        
        if (input$indicator == "weather") {
          aux <- get_aux(aux_type = "weather", 
                         time_start = as.character(input$time_range[1]), 
                         time_end = as.character(input$time_range[2]), 
                         var_name = input$weather)
        }
        
        else if (input$indicator == "education") {
          aux <- get_aux(aux_type = "education", 
                         time_start = as.character(input$time_range[1]), 
                         time_end = as.character(input$time_range[2]), 
                         var_name = input$education)
        }
        
        else {
          aux <- get_aux(aux_type = input$indicator, 
                         time_start = as.character(input$time_range[1]), 
                         time_end = as.character(input$time_range[2]))
        }
        
        map_plot <- map_plot + 
          geom_text(mapping = aes(x = x, y = y, label = avg), data = aux, 
                    inherit.aes = FALSE, size = 3)
        
      }
      
      plotly::ggplotly(p = map_plot, tooltip = c("fill"))
      
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)