# R-Shiny-app

[Demand Forecast in Movie](https://bominkim.shinyapps.io/movie/)
[Calculate Premium](https://bominkim.shinyapps.io/insurance/)
[Optimal Portfolio](https://bominkim.shinyapps.io/optimalportfolio/)


## How to make R shiny application
You can make R shiny app like so:
``` r
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)
```

You can deploy R shiny app like so:
``` r
rsconnect::deployApp()
```
