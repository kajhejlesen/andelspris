
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Using data from December or last in year to calculate inflation
  inflation_dk <- read_delim("input/inflation.csv", delim = ";", col_names = c("month", "index_month")) |> 
    mutate(
      year       = as.numeric(stringr::str_sub(month, 1, 4))
    ) |> 
    filter(year >= min_year) |> 
    group_by(year) |> 
    summarize(index_year = max(index_month)) |> 
    ungroup() |> 
    mutate(index = .data$index_year[.data$year == max(.data$year)] / index_year) |> 
    select(-index_year)
  
  andel_price <- read_delim("input/andel_price.csv", delim = ";", col_types = "dddd") |>
    tidyr::pivot_longer(cols = -year, names_to = c("type")) |>
    mutate(per_sqm = value / total_sqm) |>
    filter(year %in% seq(min_year, max_year))
  
  ejer_price <- read_delim("input/ejer_price.csv", delim = ";", skip = 3, na = c(" ", "", "..")) |>
    slice(c(-1, -2)) |>
    select(-c(1, 2)) |>
    rename(postnummer = 1) |>
    mutate(postnummer = word(postnummer,1, sep = " "))

  ejer_price_2 <- ejer_price |>
    tidyr::pivot_longer(cols = -postnummer) |>
    mutate(year = as.numeric(str_sub(name, 1, 4))) |> 
    filter(year %in% seq(min_year, max_year)) |>
    group_by(postnummer, year) |> 
    summarize(
      per_sqm  = mean(value)
    )
  
  ejer_final <- reactive({
    if (input$inflation) {
      ejer_price_2 <- ejer_price_2 |>
        left_join(inflation_dk, by = "year") |>
        mutate(per_sqm  = per_sqm  * index)
    }
    
    if (input$index == "Index") {
      ejer_price_2 <- ejer_price_2 |>
        inner_join(ejer_price_2 |> filter(year == input$index_years), by = c("postnummer")) |>
        transmute(
          postnummer,
          year = year.x,
          per_sqm = per_sqm.x / per_sqm.y * 100
        )
    } else if (input$index == "2-værelses (56 m2)") {
      ejer_price_2 <- ejer_price_2 |>
        mutate(per_sqm = 56 * per_sqm)
    } else if (input$index == "Samlet") {
      ejer_price_2 <- ejer_price_2 |>
        mutate(per_sqm = total_sqm * per_sqm)
    }
    
    ejer_price_2 |>
      mutate(per_sqm  = round(per_sqm, 0)) |>
      filter(year %in% seq(input$years_to_show[1], input$years_to_show[2]))
  })
  
  andel_final <- reactive({
    if (input$inflation) {
      andel_price <- andel_price |>
        left_join(inflation_dk, by = "year") |>
        mutate(per_sqm = per_sqm * index,
               value = value * index)
    }
    
    if (input$index == "Index") {
      andel_price <- andel_price |>
        inner_join(andel_price |> filter(year == input$index_years), by = c("type")) |>
        transmute(
          type,
          year = year.x,
          per_sqm = per_sqm.x / per_sqm.y * 100
        )
    } else if (input$index == "2-værelses (56 m2)") {
      
      # needlessly complex way to calculate the price of a single (56 m2) apartment.
      # total 'area' is 10000 units divided with two different 'fordelingstal'
      # this was done as a common area was absorbed by an apartment in 2011
      locked_value <- 23846002
      
      
      # First number locked by the value in 2011 when common area was absorbed      
      fordelingstal1 <- 9831

      # Second number for the price increase since
      fordelingstal2 <- 9943.42

      # Fraction out of the total area of 10000 units, listed in the old yearly reports
      # should be equal to 10000 * 56 / 1994, but not quite, some decimals are off
      part_56m2 <- 280.89     
      
      # Price is calculated based on andelskronen (max price)
      price_per_year <- andel_price |>
        select(-per_sqm) |> 
        pivot_wider(names_from = type, values_from = value) |> 
        select(year, andelskrone) |> 
        filter(!is.na(andelskrone))
      
      # Finding the fraction a 56m2 apartment share of the total price and then calculating value
      andel_price <- andel_price |>
        left_join(price_per_year, by = "year") |> 
        mutate(
          formue1 = pmin(locked_value, andelskrone, na.rm = T),
          formue2 = pmax(andelskrone - formue1, 0, na.rm = T),
          total_fordelingstal1 = (part_56m2 / fordelingstal1) * formue1,
          total_fordelingstal2 = (part_56m2 / fordelingstal2) * formue2,
          fraction_of_price = (total_fordelingstal1 + total_fordelingstal2) / andelskrone,
          per_sqm = value * fraction_of_price
        )
      
      # total are within a few kr. from the numbers in the report
      
    } else if (input$index == "Samlet") {
      andel_price <- andel_price |>
        mutate(per_sqm = total_sqm * per_sqm)
    }
    
    andel_price |>
      mutate(per_sqm = round(per_sqm, 0)) |>
      filter(year %in% seq(input$years_to_show[1], input$years_to_show[2]))
  })
  
  output$main_plot <- renderHighchart({
    
    highchart() |>
      hc_add_series(
        data = andel_final() |> filter(type == "valuering"),
        type = 'line',
        name = 'valuering',
        hcaes(x = year, y = per_sqm)
      ) |>
      hc_add_series(
        data = andel_final() |> filter(type == "andelskrone"),
        type = 'line',
        name = 'maxpris',
        hcaes(x = year, y = per_sqm)
      ) |>
      hc_add_series(
        data = ejer_final() |> filter(postnummer == 2100),
        type = 'line',
        name = '2100 København Ø',
        hcaes(x = year, y = per_sqm)
      ) |>
      hc_add_series(
        data = ejer_final() |> filter(postnummer == 2150),
        type = 'line',
        name = '2150 Nordhavn',
        hcaes(x = year, y = per_sqm)
      ) |>
      hc_add_series(
        data = andel_final() |> filter(type == "reserver"),
        type = 'column',
        name = 'Reserver',
        hcaes(x = year, y = per_sqm),
        visible = input$index != "Index"
      ) |>
      hc_yAxis(
        min = 0
      ) |>
      hc_caption(
        text = "Sammenligning mellem prisudviklingen for andelsboligforeningen A/B Gl. Kalkbrænderi Vej 44-46-48 og ejerlejligheder i Kbh Ø og Nordhavn.<br>Inflation baseret på 2022 priser. Valuering dækker over ejendomsvurdering eller valuarvurdering.<br><b>Kilder:</b> Boligpriser: <a href=https://rkr.statistikbank.dk>Statistikbank.dk</a>; Inflation: <a href=https://www.dst.dk/>Danmarks statistik</a>; Andelspriser: Årsrapporter/referater fra <a href=https://www.cobblestone.dk>Cobblestone</a>.<br><a href=https://github.com/kajhejlesen/andelspris>https://github.com/kajhejlesen/andelspris</a>"
      ) |> 
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "andelspris"
      )
    
    
  })
}

