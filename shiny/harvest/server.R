library(shiny)


## let's do straight lines for everything
w.fun <- function(N, w.alpha = 1, w.beta = 1/10)
{
    w <- w.alpha + w.beta * N
    w[w < 0] <- 0
    return(w)
}
b.fun <- function(w, b.alpha = .01, b.beta = 1/10)
{

## change so it has upper asyptotr

    b <- b.alpha -exp(-w*5)*b.beta/2 + b.beta/2
    b[b < 0] <- 0
    b
}
d.fun <- function(w, d.alpha = .05, d.beta = 1/10, h = 0)
{
    ## range from .05 to .01
##     d <- .05 - w*beta

    d <- d.alpha + exp(-w*5)*(-d.beta)/2 +d.beta * .8  + h
##    d <- d.alpha + d.beta * w
    d[d < 0] <- 0
    d
}

malthus.step <- function(j,
                         N0 = 5,
                         ##                         time.max = 1000,
                         time.max = 200,
                         w.alpha, w.beta,
                         b.alpha, b.beta,
                         d.alpha, d.beta,
                         h = 0)

{

    ## hard wire eqilibrium

    w.a = 1
    w.b = -.1
    b.a = 0
    b.b = .1
    d.a = .1
    d.b = -.1

    ##hard wired
    N.vec.tmp <- 0:10
    w.vec.tmp <- w.fun(N.vec.tmp, w.alpha=w.a, w.beta=w.b)
    b.vec.tmp <- b.fun(w.vec.tmp, b.alpha=b.a, b.beta=b.b)
    d.vec.tmp <- d.fun(w.vec.tmp, d.alpha=d.a, d.beta=d.b, h = 0)

    ## reactive
    N.vec.prime <- 0:10
        w.vec.prime <- w.fun(N.vec.prime, w.alpha=w.alpha, w.beta=w.b)
    b.vec.prime <- b.fun(w.vec.prime, b.alpha=b.alpha, b.beta=b.b)
    d.vec.prime <- d.fun(w.vec.prime, d.alpha=d.alpha, d.beta=d.b, h = h)


    ## par(mfcol = c(2,2))
    ## plot(w.vec.tmp, b.vec.tmp, type = "l", col = "green",
    ##      ylim = c(0, .2))
    ## lines(w.vec.tmp, d.vec.tmp, type = "l", col = "red")

    ## plot(w.vec.tmp, N.vec.tmp, type = "l", lwd = 2)



    w.vec <- N.vec <- rep(NA, time.max)
    N.vec[1] <- N0

    ## do whole simulation, storing output
    ## note N.vec is sufficient to get other values
prime = 10
    for (i in 2:prime)
    {
                N <- N.vec[i-1]
   w <- w.fun(N, w.a, w.b)
        b <- b.fun(w, b.a, b.b)
        d <- d.fun(w, d.a, d.b, h = 0)
   N.vec[i] <- N * (1 + b - d)
   w.vec[i] = w
}
##

    for (i in (prime+1):time.max)
    {
        N <- N.vec[i-1]
        ## par(mfcol = c(2,2))
        ## plot(w.vec.tmp, b.vec.tmp, type = "l", col = "green",
        ##      ylim = c(0, .2))
        ## lines(w.vec.tmp, d.vec.tmp, type = "l", col = "red")
        ## points(w.fun(N), b.fun(w.fun(N)), col = "green")
        ## points(w.fun(N), d.fun(w.fun(N)), col = "red")

        ## plot(w.vec.tmp, N.vec.tmp, type = "l", lwd = 2)
        ## points(w.fun(N), N, col = "black")

        w <- w.fun(N, w.alpha, w.beta)
        b <- b.fun(w, b.alpha, b.beta)
        d <- d.fun(w, d.alpha, d.beta, h = h)
        N.vec[i] <- N * (1 + b - d)
##         print(paste(b, d, i,N))
w.vec[i] = w
    }

    ## now plot process up to time j
    i <- j
    N <- N.vec[i]
    par(mfcol = c(2,2))
    plot(w.vec.tmp, b.vec.tmp, type = "l", col = "green",
         xlab = "Wages w",
         ylab = "Vital rates",
         ylim = c(0, .1))
    lines(w.vec.tmp, d.vec.tmp, type = "l", col = "red")
    lines(w.vec.prime, d.vec.prime, type = "l", lty = 2, col = "red")
    lines(w.vec.prime, b.vec.prime, type = "l", lty = 2, col = "green")

if(i  <= prime)
  points(w.fun(N, w.a, w.b),
         b.fun(w.fun(N, w.a, w.b), b.a, b.b),
         col = "green")
  if (i > prime)
    points(w.fun(N, w.alpha, w.beta),
           b.fun(w.fun(N, w.alpha, w.beta), b.alpha, b.beta),
           col = "green")
if(i  <= prime)
  points(w.fun(N, w.a, w.b),
         d.fun(w.fun(N, w.a, w.b), d.a, d.b, h = 0), col = "red")


  if (i > prime)
    points(w.fun(N, w.alpha, w.beta),
           d.fun(w.fun(N, w.alpha, w.beta), d.alpha, d.beta, h = h), col = "red")
title("Demography")

    plot(w.vec.tmp, N.vec.tmp, type = "l", lwd = 2,
         ylab = "Population N", xlab = "Wages w")

if(i  <= prime)
    points(w.fun(N, w.a, w.b), N, col = "black")
if (i > prime)
  points(w.fun(N, w.alpha, w.beta), N, col = "black")
    lines(w.vec.prime, N.vec.prime, type = "l", lwd = 2, lty = 2)
title("Economy")

    plot(x = c(0, time.max), c(0, 10), type = "n", axes = F,
##         axis(1, at = seq(0, 100, 10), labels = seq(-10, 90, 10),
              xlab = "Time", ylab = "Population N")
    ##     axis(1, at = seq(0, 1000, 100), labels = seq(-10, 990, 100))
        axis(1, at = seq(0, time.max, 10), labels = seq(-10, time.max-1, 10))
axis(2)
    abline(v = prime, lty = 2)
    lines(1:i, N.vec[1:i])
    points(i, N, col = "red")
title("Population over time")

## wages
    if(0) {
plot(x = c(0, time.max), c(.3, .7), type = "n", axes = F,
     xlab = "Time", ylab = "Wages w")
## axis(1, at = seq(0, 1000, 100), labels = seq(-10, 990, 100))
        axis(1, at = seq(0, time.max, 10), labels = seq(-10, time.max-1, 10))
axis(2)
abline(v = prime, lty = 2)
lines(1:i, w.vec[1:i])
points(i, w.vec[i], col = "red")
title("Wages over time")
}
    ## harvest
h.vec <- c(rep(0, prime), rep(h, length(N.vec) - prime))
    plot(x = c(0, time.max), c(0, .25), type = "n", axes = F,
     xlab = "Time", ylab = "Harvest h * N")
    ## axis(1, at = seq(0, 1000, 100), labels = seq(-10, 990, 100))
        axis(1, at = seq(0, time.max, 10), labels = seq(-10, time.max-1, 10))
axis(2)
abline(v = prime, lty = 2)
lines(1:i, h.vec[1:i]*N.vec[1:i])
    points(i, h.vec[i]*N.vec[i], col = "red")
    text(.6* time.max, .2, paste("Sustainable yield",
                                 round(h*N.vec[time.max], 4)), font = 2)
    text(.6*time.max, .15, "(Note: inaccurate if not sustainable)") 
title("Harvest over time")



    return(NULL)
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

    output$malthusPlot <- renderPlot({
        malthus.step(j = input$i,
                     N0 = input$N0,
                     w.alpha = input$w.alpha,
                     b.alpha = input$b.alpha,
                     d.alpha = input$d.alpha,
                     h = input$h,
                     w.beta = -.1,
                     b.beta = 1/10,
                     d.beta = -1/10)

        output$Eq.b<-renderText({
                 paste0("b\'(w)= ",round(input$b.alpha,3)," + ", 1/10,"*w")
        #paste0("hello world ", input$b.alpha,   "  ", b.beta)
          })

  })
})
