## this is the user-interface
## a slider with time "t"

library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Malthus simulation"),
h4("Choose levels and slide time"),
  p("The simulation starts in equilibrium.
To see the effect of exogenous changes in the starting population N, the wage function w(N), the birth function b(w), or the death function d(w), move the slider and hit the 'play' button the time slider."),
  p("For example, move the N slider to '8', hit 'play',  and watch the economy and population return to the steady state wage level with population '5'"),
  p("To replay more slowly, click on circular slider itself and use right arrow to advance (or left arrow to go back in time)."),
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("N0",
                  "Population N at time t=0",
                  min = 1,
                  max = 10,
                  value = 5),
      sliderInput("w.alpha",
                  "wage level w'(N) after time t=0",
                  min = .7,
                  max = 1.3,
                  value = 1),
      sliderInput("b.alpha",
"birth level b'(w) after time t=0",

                  min = -.03,
                  max = .05,
                  value = .00),
      sliderInput("d.alpha",
"death level d'(w) after time t=0",

                  min = .05,
                  max = .15,
                  value = .1),
      sliderInput("t",
                  "Time t",
                  min = -9,
                  max = 30,
                  value = -9,
                  animate = animationOptions(interval = 300, loop = F))
    ),

    ## Show a plot of the generated distribution
    mainPanel(
      plotOutput("malthusPlot"),
      h6("Note: new  conditions start at time 0")
      #,textOutput("Eq.b")
    )
  )
))
