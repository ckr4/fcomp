# load packages
library(shiny)

# load helper script
source("graphFcomp.R")

# Define UI
ui <- fluidPage(
  
  # Title
  titlePanel(HTML("F-distributions of the null and alternative hypotheses")
             ),
  h4(HTML(paste0("H",tags$sub("0"), ": ", "&mu;",tags$sub("1"), " = ",
      "&mu;",tags$sub("2"), " = ... = ", "&mu;",tags$sub("i"), "  vs  ",
      paste0("H",tags$sub("a"), ": ", "At least one ", "&mu;",tags$sub("i"),
             " is different")))),
  
  sidebarLayout(
    sidebarPanel(
      
      
      width=3,
      
      numericInput("no_trtmnts", "# of treatments", value=4),
      textInput("meanList", "Means (comma-separated list)",
                             value="5.2, 6.2, 7.2, 8.2"),
      numericInput("no_reps", "# of reps per treatment", 
                               value=5),
      numericInput("var", "Variance", value=2),
      selectInput("alpha", "Alpha:", c(.01, .05, .1), selected=.05),
      checkboxInput("show_a", "Fill critical region", value=TRUE),
      textOutput("fco"),
      textOutput("ncp")
    ),
    
    # create plot
    mainPanel(plotOutput("f_plot", height=600))
  
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  values <- reactiveValues()
  
  observe({
    values$means <- as.numeric(unlist(strsplit(input$meanList, ",")))
  })
  
  output$ncp <- renderText({
    
    # ensure # of means matches # of treatments
    validate(
      need(length(values$means)==input$no_trtmnts, 
           "")
    )
    
    # calculate and display critical F-value
    output$fco <- renderText({
      fco <- fcutoff(as.numeric(input$alpha), as.numeric(input$no_trtmnts), 
                     as.numeric(input$no_reps))
      op_fco <- paste("Critical F-Value:", fco)
      op_fco
    })
    
    # calculate and display non-centrality parameter
    ncp <- (sum(values$means^2 * input$no_reps) - 
      (sum(values$means * input$no_reps)^2) / 
      (input$no_trtmnts * input$no_reps)) / input$var
    op_ncp <- paste("Non-Centrality Parameter:", round(ncp, 2))
    op_ncp
    })
  
  # create plot
  output$f_plot <- 

    renderPlot({
      
      # ensure # of means matches # of treatments
      validate(
        need(length(values$means)==input$no_trtmnts, 
             "Enter the correct # of means")
      )
      
      # if # of means matches # of treatments, create plot
      if (length(values$means)==input$no_trtmnts) {
        fplots(input$no_trtmnts, input$no_reps, values$means, input$var, 
               as.numeric(input$alpha), input$show_a)  
      }
      
    })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
