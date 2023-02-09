## this is the user-interface
## a slider with time "t"

library(shiny)

shinyUI(fluidPage(
  # Application title
                                        #  titlePanel("Malthus simulation"),
    titlePanel("Solow simulation"),
    h6("1. Choose parameters (e.g., slide k' to 12)"),
    h6("2. Run simulation by advancing Time t (either with play button or by clicking slider and using keyboard arrows)"),
    h6("Refresh browser to reset parameters to default values"),
    h6("Zoom in (using browser) to enlarge fonts and diagrams"),

    fluidRow(
        column(8,
               plotOutput("solowPlot"),
               h6("Note: new  conditions start at time 0"),

               sliderInput("t",
                           "Time t",
                           min = -9,
                           max = 90,
                           value = -9,
                           animate = animationOptions(interval = 100,
                                                      loop = F))
               ),
        column(4,
               h4("Parameters"),
               sliderInput("k0",
                           "Capital/Labor Ratio k' at time t=0",
                           min = 3,
                           max = 17,
                           value = 10),
               sliderInput("n",
                           "population growth rate n' after time t=0",
                           min = -.01,
                           max = .05,
                           value = .01),
               sliderInput("s",
                           "savings level s' after time t=0",
                           min = 0.01,
                           max = 0.30,
                           value = .2),
               sliderInput("A",
                           "Output level y' after time t=0",
                           min = .7,
                           max = 2,
                           value = 1),
               sliderInput("delta",
                           "depreciation rate d' after time t=0",
                           min = 0,
                           max = .10,
                           value = .04),
               sliderInput("alpha",
                           "alpha level after time t=0",
                           min = .1,
                           max = .6,
                           value = 2/5)
               )
    )
))
