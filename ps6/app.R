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
            textOutput("textplot")
          )
        )   
      ),
      tabPanel(
        "Table",
        sidebarLayout(
          sidebarPanel(
            radioButtons(
              "category",
              "Category",
              c(
                "Sex",
                "Age",
                "Education",
                "Income"
              )
            )
          ),
          mainPanel(
            tableOutput("table")
          )
        )
      )
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
    
    output$textplot <- renderText({
      paste("There are", input$bins, "bins in each graph, and they are", input$color)
    })
    
    output$sample <- renderTable({
      sample_n(diab, 6)
    })
    
    output$table <- renderTable({
      group_by(diab, Age) %>% 
        summarize(
          diabetes_patients = n()
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
