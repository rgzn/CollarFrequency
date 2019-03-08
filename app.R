#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#  added in library load routine to automatically load libraries if not installed  -- load RODBC only if 32 bit system
#  changed the rendering for the Find Populations for Collars  tab for readability on smaller monitors

# rm(list=ls())							#clears the workspace
# if (Sys.getenv("R_ARCH")== "/i386" ) {    # install RODBC only if using 32 bit system
# packages<-c("RODBC","shiny","shinythemes","igraph","network","sna", "GGally", "intergraph","dplyr")	# libraries to install
# } else {
# packages<-c("shiny","shinythemes","igraph","network","sna","GGally", "intergraph","dplyr")	# libraries to install
# }
# # Load libraries
# n.lib<- length(packages)
# for (l in 1:n.lib) {
#   if (!require(packages[l], character.only=T, quietly=T)) {
#     # install.packages(packages[l])
#     library(packages[l], character.only=T)
#   }
# }
# 
# rm(list=ls(all=TRUE))



library(shiny)
library(shinythemes)
library(igraph)
library(network)
library(sna)
library(GGally)
library(intergraph)
library(dplyr)
library(ggplot2)

source("freq.r")


# Define UI for application that draws a histogram
ui <- navbarPage( "collard",
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
  tabPanel("Find Available Frequencies",
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId='freqFinderMargin',
                     label='Margin between collars (MHz):',
                     value=DEFAULT_FREQ_MARGIN,
                     min=0.001,
                     max=0.050,
                     step=0.0001),
        selectInput('inputPops', 'Select Populations:',
                    pop_attributes$population,
                    multiple=T, selectize=F),
        h4("Interfering Populations:"),
        tableOutput("interferencePops")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h4("Frequencies Available in All Selected Populations:"),
        plotOutput("freqsAvailPlot", 
                    hover = hoverOpts(id="plot_hover"),
                    brush = brushOpts(id="freq_brush")
                   ),
        verbatimTextOutput("hover_info"),
        h4("Collars in selected range and interfering populations:"),
        tableOutput("brush_table")
      )
    )
           

           ), 
   tabPanel(
    ("Find Populations for Collars"),
           fluidRow(
             column(width = 6,
               
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
                               "lion",
                               "none"
                             )),
                 
                 numericInput(inputId='popFinderMargin',
                              label='Margin between collar frequencies (MHz):',
                              value=DEFAULT_FREQ_MARGIN,
                              min=0.001,
                              max=0.050,
                              step=0.0001)
          ),column(6,
                   h4("Available Target Populations:"),
                   tableOutput("availablePops")
          )
           ),
    fluidRow(column(12,h4("Conflicting Collars"),
                    tableOutput("conflictCollars")))
    
  )
  # navbarMenu("Help",
  #            tabPanel("Supporting Files"),
  #            tabPanel("Code"),
  #            tabPanel("About")),
)

# Define server logic required to draw output plots and tables
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
   output$selectedPops <- renderTable({
     pop_attributes[pop_attributes$population %in% input$inputPops, c('population', 'species', 'location')]
   }, spacing="xs")
   
   output$interferencePops <- renderTable({
     find_neighborhood_df(pop_graph, pop_attributes, input$inputPops)
   })
   
   output$freqsAvailPlot <- renderPlot({
     plot_available(pop_graph, collars, input$inputPops, freq_margin=input$freqFinderMargin)
   })
   
   output$hover_info <- renderPrint({
     cat("Frequency at mouse:\n", 
         as.character(input$plot_hover$x) )
   })
   
   output$brush_table <- renderTable({
     neighbors = find_neighborhood(pop_graph, input$inputPops)$name
     find_collars_in_range(pop_graph, 
                           collars[collars$population %in% neighbors, ], 
                           lo_f = input$freq_brush$xmin, 
                           hi_f = input$freq_brush$xmax)
   },
   digits=3,striped=T, bordered=T,hover=T,spacing="xs")
   
   
   output$availablePops <- renderTable({
     df_from_nodes(
       find_available_pops(pop_graph, collars, input_freq=input$frequency, input_species=input$species, freq_margin=input$popFinderMargin)
     )
   },
   hover=T, border=T)
   
   output$occupiedPops <- renderTable({
     find_conflict_pops(pop_graph, collars, input_freq=input$frequency, freq_margin=input$popFinderMargin)
   })
   
   output$popAttributes <- renderDataTable({
     pop_attributes
   })
   
   output$conflictCollars <- renderTable({
     find_conflict_collars(pop_graph, collars, input_freq=input$frequency, freq_margin=input$popFinderMargin)
   },
   digits=3,striped=T, bordered=T,hover=T,spacing="xs")
   
   output$allCollars <- renderDataTable({
     collars
   }, options=list(autoWidth=T)
   )  
}

# Run the application 
shinyApp(ui = ui, server = server)

