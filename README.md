# R-Shiny-app

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
