# Working directory: ~/Desktop/BIOSTAT 625 - Computing with Big Data/Final Project
setwd("~/Desktop/BIOSTAT 625 - Computing with Big Data/Final Project")

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(rdrop2)
library(readxl)
library(usmap)

state_list <- read.csv("state.csv")

token <- drop_auth()
drop_read_excel <- function(file, skip = 0, dest = tempdir(), dtoken = rdrop2:::get_dropbox_token(), ...) {
  localfile = paste0(dest, "/", basename(file))
  drop_download(path = file, local_path = localfile, overwrite = TRUE, dtoken = dtoken)
  read_excel(path = localfile, skip = skip, ...)
}

create_keyword_data <- function(keyword, root_path = "/final project/bio625_final_project_rawdata/") {
  df <- drop_read_excel(file = paste0(root_path, 
                                      "biostat625 final raw-google trends/biostat625 final raw/", 
                                      keyword, " (us) – google trends export – glimpse.xlsx"), 
                        skip = 6) %>% 
    mutate(Week = as.Date(Week), Region = "US")
  
  for (state in state_list$code) {
    df_state <- drop_read_excel(file = paste0(root_path, keyword, " by state (recent 5 years)/", 
                                              keyword, " (us-", tolower(state), 
                                              ") – google trends export – glimpse.xlsx"), 
                                skip = 6) %>% 
      mutate(Week = as.Date(Week), Region = state)
    df <- rbind(df, df_state)
  }
  
  return(df)
}

depression <- create_keyword_data(keyword = "Depression")



# Define UI for app ----
ui <- fluidPage(
  # Theme
  theme = bs_theme(bootswatch = "journal"),
  
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
      dateRangeInput(
        inputId = "time_range", 
        label = "Time range", 
        start = "2022-01-01", 
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
        condition = "input.time_range[1] > '2022-11-20'", 
        selectInput(
          inputId = "algorithm", 
          label = "Prediction algorithm", 
          choices = list(`None` = "None"), 
          selected = "None"
        )
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Line graph ----
      conditionalPanel(
        condition = "input.plot_type == 'time'", 
        plotOutput(outputId = "line_graph")
      ),
      
      # Output: Map ----
      conditionalPanel(
        condition = "input.plot_type == 'region'", 
        plotOutput(outputId = "map")
      )
      
    )
  )
)

# Define server logic required to draw plots ----
server <- function(input, output) {
  
  output$line_graph <- renderPlot({
    
    depression %>% 
      filter((Week >= input$time_range[1]) & (Week <= input$time_range[2])) %>% 
      filter(Region == input$region) %>% 
      ggplot(mapping = aes(x = Week, y = `Search Volume (Absolute)`)) + 
      geom_line() + 
      theme_bw(base_size = 12)
    
  })
  
  output$map <- renderPlot({
    
    depression %>% 
      filter((Week >= input$time_range[1]) & (Week <= input$time_range[2])) %>% 
      group_by(Region) %>% 
      summarize(abs_volume = sum(`Search Volume (Absolute)`)) %>% 
      left_join(state_list, by = c("Region" = "code")) %>% 
      plot_usmap(data = ., values = "abs_volume", regions = "states") + 
      scale_fill_viridis_c(option = "magma", begin = 0.1, end = 0.9, direction = -1) + 
      theme_bw() + 
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank())
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)