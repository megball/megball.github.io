
library(shiny)
library(ggplot2)

plots <- c("boxplot", "histogram")
yes <- c("yes", "no")

ui <- shinyUI(fluidPage(
  titlePanel("Exploring Beer Data"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Histogram",
             pageWithSidebar(
               headerPanel('Distribution'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 radioButtons("plot", "What Type of Plot?", plots)
                 
               ),
               mainPanel(
                 plotOutput('Hist')
               )
             )
    ),
    tabPanel("Scatter Plot",
             pageWithSidebar(
               headerPanel('Scatter Plot'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol2', 'X Variable', ""),
                 #selectInput('ycol', 'Y Variable', ""),
                 radioButtons("add.line", "Add Line?", yes),
                 
               ),
               mainPanel(
                 plotOutput('Scatter')
               )
             )
    )
    
  )
)
)

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, header = input$header)
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'xcol2', label = 'X Variable',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[3])
    
    return(df)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  output$Hist <- renderPlot({
    plot1    <- data()[, input$xcol]
    dist <- switch(input$plot, histogram = hist(plot1), boxplot = boxplot(plot1))
  })
  
  output$Scatter <- renderPlot({
    #plot3 <- data()[, c(input$xcol2, input$ycol)]
    scatter <- ggplot(data(), aes(x = abv, y=ibu)) + geom_point() + 
    withline <- scatter + stat_smooth(method = "lm")
    switch(input$add.line, yes = withline , no = scatter)
    
  })
})

shinyApp(ui, server)