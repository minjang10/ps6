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
        p("This dataset shows many different data points of people with", strong("Diabetes.")),
        p(""),
        p("The diabetes column is either 0, 1, or 2.", em("0"), "for no diabetes,",
          em("1"), "for type 1, and", em("2"), "for type 2 diabetes."),
        p("Here are", em("6 random samples"), "from the data: "),
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
              max = 15,
              value = 10
            ),
            radioButtons(
              "color",
              "Choose a bin color",
              c(
                "red",
                "blue",
                "green",
                "orange",
                "yellow",
                "purple",
                "black"
              )
            )
          ),
          mainPanel(
            plotOutput("distPlot"),
            textOutput("text")
          )
        )   
      ),
      tabPanel("Table")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    diab <- read_csv("diabetes.csv")

    output$distPlot <- renderPlot({
      ggplot(data = diab) +
        geom_histogram(mapping = aes(x = Income), bins = input$bins, fill = input$color) +
        facet_wrap(~Diabetes_012) +
        xlab("income (0-8)") +
        ylab("number of people") +
        ggtitle("Income versus number of diabetes patients")
    })
    
    output$text <- renderText({
      n0 <- nrow(diab[diab$Diabetes_012 == 0 && !is.na(diab$Income),])
      n1 <- nrow(diab[diab$Diabetes_012 == 1 && !is.na(diab$Income),])
      n2 <- nrow(diab[diab$Diabetes_012 == 2 && !is.na(diab$Income),])
      avg0 <- sum(diab[!is.na(diab$Income),])
      paste("The average income")
    })
    
    output$sample <- renderTable({
      sample_n(diab, 6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
