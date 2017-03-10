#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

source("freq.r")


# Define UI for application that draws a histogram
ui <- navbarPage( "Collar Placer",
  theme=shinytheme('darkly'),
  
  # Application title
  #titlePanel("Frequency Finder"),
  tabPanel("All Collars Table",
           fluidPage(
             h4("All Animal Collars:"),
             dataTableOutput("allCollars")
           )
           ),
  
  tabPanel("Interference Network",
           fluidPage(
                 h4("Population VHF Conflict Network:"),
                 plotOutput("networkPlot"),
                 h4("Populations In Network:"),
                 dataTableOutput("popAttributes")
                 
           )
           
           # Network plot
           # h4("Population VHF Conflict Network:"),
           # plotOutput("networkPlot")
           #plotOutput("netCounties"),
           #plotOutput("netMap"),
           #plotPNG(func=netMap(baseMapPlot, pop_graph)),
           ),
  tabPanel(
    "Find Populations for Collars",
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId='freqMargin',
                     label='Margin between collars (MHz):',
                     value=0.005,
                     min=0.001,
                     max=0.050,
                     step=0.0001),
        selectInput('inputPops', 'Select Populations:',
                    pop_attributes$population,
                    multiple=T, selectize=F),
        tableOutput("selectedPops")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
      )
    )
           

           ),
  tabPanel(
    "Find Populations for Collars",
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId='frequency',
                     label='Frequency of New Collar (MHz)',
                     value=160.0,
                     min=159.0,
                     max=161.0,
                     step=0.001),
        
        selectInput("species", "Specify target species:",
                    choices = c(
                      "bighorn",
                      "deer",
                      "bobcat",
                      "elk",
                      "pronghorn",
                      "bear",
                      "none"
                    )),
        
        numericInput(inputId='freqMargin',
                     label='Margin between collar frequencies (MHz):',
                     value=0.005,
                     min=0.001,
                     max=0.050,
                     step=0.0001)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        h4("Available Target Populations:"),
        tableOutput("availablePops"),
        
        h4("Conflicting Collars"),
        tableOutput("conflictCollars")
      )
    )
  )
  # navbarMenu("Help",
  #            tabPanel("Supporting Files"),
  #            tabPanel("Code"),
  #            tabPanel("About")),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   output$networkPlot <- renderPlot({
     network_plot
   })
   # output$networkCounties <- renderPlot({
   #   ggnetworkmap( county_base, net = pop_graph,
   #                 great.circles=T,
   #                 node.group="species",
   #                 segment.color="red", palette ="Set3"
   #   )
   # })
   # output$netMap <- renderPlot({
   #   ggnetworkmap( baseMapPlot, net = pop_graph,
   #                 great.circles=T,
   #                 node.group="species",
   #                 segment.color="red", palette ="Set3"
   #   )
   # })
   
   
   output$availablePops <- renderTable({
     df_from_nodes(
       find_available_pops(pop_graph, collars, input_freq=input$frequency, input_species=input$species, freq_margin=input$freqMargin)
     )
   },
   hover=T, border=T)
   
   output$occupiedPops <- renderTable({
     find_conflict_pops(pop_graph, collars, input_freq=input$frequency, freq_margin=input$freqMargin)
   })
   output$popAttributes <- renderDataTable({
     pop_attributes
   })
   
   output$conflictCollars <- renderTable({
     find_conflict_collars(pop_graph, collars, input_freq=input$frequency, freq_margin=input$freqMargin)
   },
   striped=T, bordered=T,hover=T,spacing="xs")
   
   output$selectedPops <- renderTable({
     pop_attributes[pop_attributes$population %in% input$inputPops, c('population', 'species', 'location')]
   })
   output$allCollars <- renderDataTable({
     collars
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

