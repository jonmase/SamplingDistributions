# Shiny Server for Robustness Model
# Authors: Jon Mason, Medical Sciences Division Learning Technologies, University of Oxford,
#		   Martin J. Hadley, Academic IT Research Support, University of Oxford
# License: GPL v3, https://www.gnu.org/licenses/gpl.html

library("shiny")
library("sn")

source("visualisation-function.R")

function(input, output, session) {
  output$resetable_input <- renderUI({
    times <- input$reset
    fluidRow(column(width=12,
      selectInput(
        "distribution_type",
        h4("What distribution should be used?"),
        choices = c("normal", "skewed"),
        selected = "norm"
      ),
      conditionalPanel(
        condition = "input.distribution_type == 'skewed'",
        sliderInput(
          "alpha",
          label = h4("Alpha (skewness)"),
          min = -20,
          max = 20,
          value = 10
        )
      ),
      #numericInput(
      #  "datapoints",
      #  label = h4("Number of data-points in each sample"),
      #  value = 100
      #),
      sliderInput(
        "datapoints",
        label = h4("Number of data-points in each sample"),
        min = 5,
        max = 100,
        value = 100
      ),
      sliderInput(
        "reps",
        label = h4("Number of samples"),
        min = 5,
        max = 500,
        value = 250
      ),
      selectInput(
        "what",
        h4("What should be plotted?"),
        choices = c("mean", "median", "sd", "var", "range"),
        selected = "means"
      )
    ))
  })
  output$firstPlot <- renderPlot({
    f(
      distribution = req(input$distribution_type),
      alpha =  req(input$alpha),
      N = req(input$datapoints),
      reps = req(input$reps),
      what = req(input$what),
      plot.all = TRUE
    )
    
  }, height = 700)
  
  observeEvent(input$rerun, {
    output$firstPlot <- renderPlot({
      f(
        distribution = input$distribution_type,
        alpha =  input$alpha,
        N = input$datapoints,
        reps = input$reps,
        what = input$what,
        plot.all = TRUE
      )
      
    }, height = 700)
  })
}