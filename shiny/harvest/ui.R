## this is the user-interface
## a slider with time "i"

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Malthus simulation with harvesting"),
h4("Choose levels and slide time"),
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("N0",
                  "Starting population N",
                  min = 1.0,
                  max = 10.0,
                  value = 7.7,
                  step = .1),
      sliderInput("w.alpha",
                                         "w'(N) level",
                                         min = .7,
                                         max = 1.3,
                                         value = 1),
      sliderInput("b.alpha",
"b'(w) level",

                  min = -.03,
                  max = .05,
                  value = .00),
      sliderInput("d.alpha",
"d'(w) level",

                  min = .05,
                  max = .15,
                  value = .1),
      sliderInput("h",
"harvest h rate",

                  min = .0,
                  max = .15,
                  value = .0),
      sliderInput("i",
                  "Time i",
                  min = 1,
                  ##                  max = 1000,
                                    max = 200,
                  value = 1,
                  animate = animationOptions(interval = 300, loop = F))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("malthusPlot"),
      h6("Note: new  conditions start at time 0")
      #,textOutput("Eq.b")
    )
  )
))
