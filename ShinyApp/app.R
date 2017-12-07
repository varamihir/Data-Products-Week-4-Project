# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Load all packages and the diamond data set that we are going to use to makea shinyApp.
library(ggplot2)
library(dplyr)
library(shiny)

# A dataset containing the prices and other attributes of almost 54,000 diamonds. 
data(diamonds)
data <- as.data.frame(diamonds)

# The variables are as follows:
 names(diamonds)
# price - price in US dollars ($326--$18,823)
# carat - weight of the diamond (0.2--5.01)
# cut - quality of the cut (Fair, Good, Very Good, Premium, Ideal)
# color- diamond colour, from J (worst) to D (best)
# clarity - a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))
# x - length in mm (0--10.74)
# y - width in mm (0--58.9)
# z - depth in mm (0--31.8)
# depth - total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)
# table - width of top of diamond relative to widest point (43--95)


# Define UI for application that draws a plot

ui <- fluidPage(
  # Application title
  titlePanel("How many diamonds are in the specific data? "),
  # Sidebar with a slider input for price, carat and cut, that includes a navigation bar.
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "price", min = 300, max = 20000, c(500, 15000), pre = "$"),
      sliderInput("caratInput", "carat", min = .19, max = 2, c(.25, 1.5)),
      radioButtons("cutInput","cut", choices = c("Ideal","Premium","Good","Very Good", "Fair"), 
                  selected = "Ideal"),
      submitButton("Submit")
    ),
    # Show a plot for number of diamonds  price vs carat.
    # Show a table with all the information based on carat, cut, clarity , color and prices.
    mainPanel(
      plotOutput("plot1"),
      br(),
      tableOutput("results")
    
    )
  )
)
library(dplyr)
server <- function(input, output){
  filtered <- reactive({
    data %>%filter(price >=  input$priceInput[1],
                   price <= input$priceInput[2],
                   carat == input$caratInput,
                   cut== input$cutInput)
  })
  # Rendering the plot using renderPlot function
  output$plot1 <- renderPlot({
   
      ggplot(filtered(), aes(price, carat))+ geom_jitter(col = "blue")
  })
  
    output$results <- renderTable({
      filtered() 
      
    })
    
  }
  shinyApp(ui = ui, server= server)
