

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Udvikling i pris for andel"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years_to_show",
                        "Vis år",
                        min = min_year,
                        max = max_year,
                        value = c(min_year, max_year),
                        sep = "",
                        ),
            checkboxInput("inflation",
                          "Justér for inflation",
                          value = FALSE
                          ),
            radioButtons("index",
                         "",
                         c("Pris pr. m2", "2-værelses (56 m2)", "Samlet", "Index"),
                         "Pris pr. m2"
                        ),
            conditionalPanel(condition = "input.index == 'Index'",
              sliderInput("index_years",
                        "År, index = 100",
                        min = min_year,
                        max = max_year,
                        value = 2010,
                        sep = "",
                        )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           highchartOutput("main_plot")
        )
    )
)
