library(shiny)
library(bslib)
library(tidyverse)

### Data pre-processing ###
superstoreData <- read.csv("/Users/Vivian/Documents/GitHub/DataProjects/SuperstoreDashboard/Superstore.csv")
superstoreData <- superstoreData %>% mutate("Order.Year" = format(as.Date(`Order.Date`, format="%m/%d/%Y"),"%Y"))
orderYears <- sort(unique(superstoreData$`Order.Year`), decreasing = TRUE)
states <- sort(unique(superstoreData$`State`))


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
  
  # Furniture sales table for Sales by Category tab
  output$furnSales <- renderTable(
    {superstoreData %>% 
        filter(`Category` == "Furniture", Order.Year == input$orderYear, State == input$state) %>% 
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
  
  # Office Supplies sales table for Sales by Category tab
  output$officeSales <- renderTable(
    {superstoreData %>% 
        filter(`Category` == "Office Supplies", Order.Year == input$orderYear, State == input$state) %>% 
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
  
  # Technology sales table for Sales by Category tab
  output$techSales <- renderTable(
    {superstoreData %>% 
        filter(`Category` == "Technology", Order.Year == input$orderYear, State == input$state) %>% 
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

### Run the application ###
shinyApp(ui = ui, server = server)
