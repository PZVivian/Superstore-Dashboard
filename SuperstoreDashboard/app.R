library(shiny)
library(bslib)
library(tidyverse)

### Data pre-processing & Functions###
superstoreData <- read.csv("/Users/Vivian/Documents/GitHub/DataProjects/SuperstoreDashboard/Superstore.csv")
superstoreData <- superstoreData %>% mutate("Order.Year" = format(as.Date(`Order.Date`, format="%m/%d/%Y"),"%Y"))
orderYears <- sort(unique(superstoreData$`Order.Year`), decreasing = TRUE)
states <- sort(unique(superstoreData$`State`))

renderTable_salesByCategory <- function(category_name, input) {
  renderTable(
    {superstoreData %>% 
        filter(`Category` == category_name, Order.Year == input$orderYear, State == input$state) %>% 
        group_by(`Sub.Category`) %>% 
        summarize("Order Count" = n_distinct(Order.ID),
                  "Total Profit" = sum(Profit)
        ) %>% 
        rename("Subcategory" = `Sub.Category`) %>% 
        arrange(desc(`Total Profit`))
    },
    hover = TRUE,
    spacing = 'xs'
  )
}


### Application UI ###
ui <- fluidPage(

  theme = bs_theme(version = 4, bootswatch = "lux"),
  
  navbarPage(
    "Superstore Sales Dashboard",
    
    inverse = TRUE,
    
    ## Overview Page ##
    tabPanel(
      "Overview",
      "Oops! Looks like this page is still being developed..."
    ),
    
    ## State View Page ##
    tabPanel(
      "State View",
      sidebarLayout(
        # Input
        sidebarPanel(
          selectInput("orderYear", "Order Year:", orderYears),
          selectInput("state", "State:", states)
          ,width = 3
        ),
        # Output
        mainPanel(
          tabsetPanel(
            
            # Sales by Category Tab #
            tabPanel(
              "Sales by Category",
              h5("Furniture"),
              tableOutput("furnSales"),
              h5("Office Supplies"),
              tableOutput("officeSales"),
              h5("Technology"),
              tableOutput("techSales")
            )
            
          )
        )
      )
    ),
  ),
)


### Server Logic ###
server <- function(input, output) {
  
  ## State View Page ##
  # Sales by Category Tab #
  # Show details on subcategory sales for each category
  output$furnSales <- renderTable_salesByCategory("Furniture", input)
  output$officeSales <- renderTable_salesByCategory("Office Supplies", input)
  output$techSales <- renderTable_salesByCategory("Technology", input)

}


### Run the application ###
shinyApp(ui = ui, server = server)
