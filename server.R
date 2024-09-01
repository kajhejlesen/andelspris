
# Define server logic required to draw a histogram
server <- function(input, output) {
  
    # Using data from December to calculate inflation for a year
  inflation_dk <- read_delim("input/inflation_dk.csv", delim = ";") %>%
    filter(stringr::str_detect(TID, ".*M12$")) %>%
    transmute(
      year       = as.numeric(stringr::str_sub(TID, 1, 4)),
      percentage = 1 + INDHOLD / 100
    ) %>%
    add_row(year = min_year, percentage = 1) %>% 
    arrange(year) %>%
    mutate(
      index = cumprod(percentage),
      compared_to_last = 1 / (index / .data$index[.data$year == max(.data$year)])
    ) %>%
    select(year, compared_to_last)
  
  # todo: take inflation in ongoing year into account
  if (max(inflation_dk$year) != max_year) {
    inflation_dk <- add_row(inflation_dk, year = max_year, compared_to_last = 1)
  }
  
  andel_price <- read_delim("input/andel_price.csv", delim = ";", col_types = "dddd") %>%
    tidyr::pivot_longer(cols = -year, names_to = c("type")) %>%
    mutate(per_sqm = value / total_sqm) %>%
    filter(year %in% seq(min_year, max_year))
  
  ejer_price <- read_delim("input/ejer_price.csv", delim = ";", skip = 3, na = c(" ", "", "..")) %>%
    slice(c(-1, -2)) %>%
    select(-c(1, 2)) %>%
    rename(postnummer = 1) %>%
    mutate(postnummer = word(postnummer,1, sep = " "))

  ejer_price_2 <- ejer_price %>%
    tidyr::pivot_longer(cols = -postnummer) %>%
    mutate(year = as.numeric(str_sub(name, 1, 4))) %>% 
    filter(year %in% seq(min_year, max_year)) %>%
    group_by(postnummer, year) %>% 
    summarize(
      per_sqm  = mean(value)
    )
  
  ejer_final <- reactive({
    if (input$inflation) {
      ejer_price_2 <- ejer_price_2 %>%
        left_join(inflation_dk, by = "year") %>%
        mutate(per_sqm  = per_sqm  * compared_to_last)
    }
    
    if (input$index == "Index") {
      ejer_price_2 <- ejer_price_2 %>%
        inner_join(ejer_price_2 %>% filter(year == input$index_years), by = c("postnummer")) %>%
        transmute(
          postnummer,
          year = year.x,
          per_sqm = per_sqm.x / per_sqm.y * 100
        )
    } else if (input$index == "2-værelses (56 m2)") {
      ejer_price_2 <- ejer_price_2 %>%
        mutate(per_sqm = 56 * per_sqm)
    } else if (input$index == "Samlet") {
      ejer_price_2 <- ejer_price_2 %>%
        mutate(per_sqm = total_sqm * per_sqm)
    }
    
    ejer_price_2 %>%
      mutate(per_sqm  = round(per_sqm, 0)) %>%
      filter(year %in% seq(input$years_to_show[1], input$years_to_show[2]))
  })
  
  andel_final <- reactive({
    if (input$inflation) {
      andel_price <- andel_price %>%
        left_join(inflation_dk, by = "year") %>%
        mutate(per_sqm = per_sqm * compared_to_last)
    }
    
    if (input$index == "Index") {
      andel_price <- andel_price %>%
        inner_join(andel_price %>% filter(year == input$index_years), by = c("type")) %>%
        transmute(
          type,
          year = year.x,
          per_sqm = per_sqm.x / per_sqm.y * 100
        )
    } else if (input$index == "2-værelses (56 m2)") {
      andel_price <- andel_price %>%
        mutate(per_sqm = 56.667 * per_sqm)
    } else if (input$index == "Samlet") {
      andel_price <- andel_price %>%
        mutate(per_sqm = total_sqm * per_sqm)
    }
    

    andel_price %>%
      mutate(per_sqm = round(per_sqm, 0)) %>%
      filter(year %in% seq(input$years_to_show[1], input$years_to_show[2]))
  })
  
  output$main_plot <- renderHighchart({
    
    highchart() %>%
      hc_add_series(
        data = andel_final() %>% filter(type == "valuering"),
        type = 'line',
        name = 'valuering',
        hcaes(x = year, y = per_sqm)
      ) %>%
      hc_add_series(
        data = andel_final() %>% filter(type == "andelskrone"),
        type = 'line',
        name = 'maxpris',
        hcaes(x = year, y = per_sqm)
      ) %>%
      hc_add_series(
        data = ejer_final() %>% filter(postnummer == 2100),
        type = 'line',
        name = '2100 København Ø',
        hcaes(x = year, y = per_sqm)
      ) %>%
      hc_add_series(
        data = ejer_final() %>% filter(postnummer == 2150),
        type = 'line',
        name = '2150 Nordhavn',
        hcaes(x = year, y = per_sqm)
      ) %>%
      hc_add_series(
        data = andel_final() %>% filter(type == "reserver"),
        type = 'column',
        name = 'Reserver',
        hcaes(x = year, y = per_sqm),
        visible = input$index != "Index"
      ) %>%
      hc_yAxis(
        min = 0
      ) %>%
      hc_caption(
        text = "Sammenligning mellem prisudviklingen for andelsboligforeningen A/B Gl. Kalkbrænderi Vej 44-46-48 og ejerlejligheder i Kbh Ø og Nordhavn.<br>Inflation baseret på 2022 priser. Valuering dækker over ejendomsvurdering eller valuarvurdering<br><b>Kilder:</b> Boligpriser: <a href=https://rkr.statistikbank.dk>Statistikbank.dk</a>; Inflation: <a href=https://www.dst.dk/>Danmarks statistik</a>; Andelspriser: Årsrapporter/referater fra <a href=https://www.cobblestone.dk>Cobblestone</a>."
      ) %>% 
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "andelspris"
      )
    
    
  })
}

