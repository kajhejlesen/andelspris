
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  andel_price <- read_delim("input/andel_price.csv", delim = ";") %>%
    mutate(per_sqm = value / total_sqm)
  
  ejer_price <- read_delim("input/ejer_price.csv", delim = ";", skip = 3, na = c(" ", "")) %>%
    slice(c(-1, -2)) %>%
    select(-c(1, 2, 3))
  
  ejer_price_2 <- ejer_price %>%
    tidyr::pivot_longer(cols = everything()) %>%
    mutate(year = as.numeric(str_sub(name, 1, 4))) %>% 
    group_by(year) %>% 
    summarize(
      avg_price = mean(value)
    )
  
  create_index <- function(data, year) {
    return(NULL)
  }
  
  output$main_plot <- renderHighchart({
    
    highchart() %>%
      hc_add_series(
        data = andel_price %>% filter(type == "samlet"),
        type = 'line',
        hcaes(x = year, y = per_sqm)
      ) %>%
      hc_add_series(
        data = ejer_price_2,
        type = 'line',
        hcaes(x = year, y = avg_price)
      ) %>%
      hc_yAxis(
        min = 0
      )
    
    
  })
}

