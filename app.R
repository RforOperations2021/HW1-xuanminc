#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(shinythemes)
library(readr)
# load data
df <- read_csv('WHO_COVID-19.csv')

# Define UI for application
ui <- fluidPage(
    
    # set the them
    theme = shinytheme("sandstone"),

    # Application title
    titlePanel("COVID 19 Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            "---Global Situation Tab---", br(),
            
            # select metrics to display on the data table  ----------------
            checkboxGroupInput(inputId = "selected_metrics",
                               label = "Select Metrics:",
                               choices = c("WHO Region" = "WHO_Region", 
                                           "Cases - cumulative total" = "Cases_cumulative_total", 
                                           "Cases - newly reported in last 7 days" = "Cases_newly_reported_in_last_7_days",
                                           "Cases - newly reported in last 24 hours" = "Cases_newly_reported_in_last_24_hours",
                                           "Deaths - cumulative total" = "Deaths_cumulative_total",
                                           "Deaths - newly reported in last 7 days" = "Deaths_newly_reported_in_last_7_days",
                                           "Deaths - newly reported in last 24 hours" = "Deaths_newly_reported_in_last_24_hours",
                                           "Transmission Classification" = "Transmission_Classification"),
                               selected = c("WHO_Region", 
                                            "Cases_cumulative_total",
                                            "Deaths_cumulative_total")),
    
            
            "---Scatter Plot Tab---", br(),
            
            # Set alpha level ---------------------------------------------
            sliderInput(inputId = "alpha", 
                        label = "Alpha:", 
                        min = 0, max = 1, 
                        value = 0.5),
            
            "--- Histogram Tab---", br(),
            
            # Select regions
            radioButtons(inputId ="region",
                         label = "Select region:", 
                         choices = c("Americas","Africa","Eastern Mediterranean","Europe","South-East Asia","Western Pacific"), 
                         selected = "Americas")
        ),

        
        mainPanel(
            tabsetPanel(type = "tab",
                        tabPanel("Global Situation", br(), br(), DT::dataTableOutput(outputId = "globalTable")),
                        tabPanel("Scatter Plot", br(), br(), plotOutput("scatterplot")),
                        tabPanel("Histogram", br(), br(), plotOutput("histogram1")),
                        tabPanel("Boxplot", br(), br(), plotOutput("boxplot")),
                        tabPanel("Download Data", br(),br(),"Click", downloadLink('downloadData', 'here'), "to download original dataset.")
                        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    # Create data table
    output$globalTable <- DT::renderDataTable({
            DT::datatable(data = df[, c('Name',input$selected_metrics)], 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
    })
    
    # create scatter plot
    output$scatterplot <- renderPlot({
        ggplot(data = df[2:nrow(data),], aes(x = Deaths_cumulative_total, y = Cases_cumulative_total, color = WHO_Region)) +
            geom_point(alpha = input$alpha, size = 3) +
            labs(x = "Deaths cumulative total",
                 y = "Cases cumulative total",
                 title = "Global countries' death and confirmed cases"
                 )
    })
    
    # create histogram
    output$histogram1 <- renderPlot({
        ggplot(data = df[df$WHO_Region == input$region,], aes(x = Cases_cumulative_total, fill = input$region)) +
            geom_histogram() +
            labs(x = "Cases cumulative total",
                 title = "Global countries' total cumulative cases, by region")
    })
    
    # create box plot
    output$boxplot <- renderPlot({
        ggplot(data[data$Transmission_Classification %in% c("Community transmission","Clusters of cases","No cases","Sporadic cases"),], 
               aes(x = as.factor(Transmission_Classification), y = Cases_newly_reported_in_last_7_days, color = Transmission_Classification)) +
            xlab("Community Transmission Type") +
            ylab("Cases newly reported in last 7 days") +
            geom_boxplot() +
            labs(title = "Newly reported cases in last 7 days, by community transmission type")
    })
    
    # download button
    output$downloadData <- downloadHandler(
        filename = function() {
             paste('data-', Sys.Date(), '.csv', sep='')
           },
           content = function(con) {
             write.csv(df, con)
           }
         )
}

# Run the application 
shinyApp(ui = ui, server = server)
