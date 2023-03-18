library(shiny)
library(tidyverse)
library(highcharter)

total_sqm <- 1994

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  input <- read_delim("input/pris_hist.csv", delim = ";") %>%
    mutate(per_sqm = value / total_sqm)
  
  output$main_plot <- renderHighchart({
    
    highchart() %>%
      hc_add_series(
        data = input %>% filter(type == "samlet"),
        type = 'line',
        hcaes(x = year, y = per_sqm)
      )
    
    
  })
}

