#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("tidyverse")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Diabetes Data"),

    tabsetPanel(
      tabPanel(
        "About",
        p("This dataset shows many different data points of people with Diabetes."),
        p(""),
        p("The diabetes column is either 0, 1, or 2. 0 for no diabetes, 1 for type 1, and 2 for type 2 diabetes."),
        p("Here are 6 random samples from the data: "),
        tableOutput("sample")
      ),
      tabPanel(
        "Plot",
        sidebarLayout(
          sidebarPanel(
            sliderInput(
              "bins",
              "Number of Bins: ",
              min = 1,
              max = 50,
              value = 25
            )
          ),
          mainPanel(
            plotOutput("distPlot")
          )
        )   
      ),
      tabPanel("Table")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      diab <- read_csv("diabetes.csv")
      ggplot(data = diab) +
        geom_histogram(mapping = aes(x = income)) +
        facet_wrap(~Diabetes_012)
    })
    
    output$sample <- renderTable({
      diab <- read_csv("diabetes.csv")
      sample_n(diab, 6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
