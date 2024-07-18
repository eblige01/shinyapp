#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Elijah's LM Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            # Horizontal line ----
            tags$hr(),
            
            # Button to click that will show the plot
            actionButton( "go", "Plot LM")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("origPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents"),
           textOutput("sumStats")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    lmdata <- reactiveValues()
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    update_lm <- function(){
        lmdata$model <- lm(y~x, data = dataInput())
        lmdata$rsq <- summary(lmdata$model)$r.squared
        lmdata$int <- coef(lmdata$model)[1]
        lmdata$slope <- coef(lmdata$model)[2]
        lmdata$coef <- sqrt(lmdata$rsq)
    
    }
    
    observeEvent(input$go,{
    update_lm()
    })
    
    
    output$origPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y,xlab = "X values", ylab = "Y values")
    })
    
    output$lmPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y,xlab = "X values", ylab = "Y values")
        abline(lmdata$model)
    })
    
    
    output$contents <- renderTable({
        
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
    
    output$sumStats <- renderText({
        paste("R squared:",lmdata$rsq, "Slope:", lmdata$slope,"Intercept:",lmdata$int, "Correlation coefficient",lmdata$coef)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)