library(shiny)

point.cex = 1.5

## let's do straight lines for everything
w.fun <- function(N, w.alpha = 1, w.beta = 1/10)
{
    w <- w.alpha + w.beta * N
    w[w < 0] <- 0
    return(w)
}
b.fun <- function(w, b.alpha = .01, b.beta = 1/10)
{
    b <- b.alpha + b.beta * w
    b[b < 0] <- 0
    b
}
d.fun <- function(w, d.alpha = .05, d.beta = 1/10)
{
    ## range from .05 to .01
    ##     d <- .05 - w*beta
    d <- d.alpha + d.beta * w
    d[d < 0] <- 0
    d
}



malthus.step <- function(j, ## j in time index in simulation
                         N0 = 5,
                         time.max = 40,
                         w.alpha, w.beta, # intercept and slope of w
                         b.alpha, b.beta,
                         d.alpha, d.beta)

{

    ## hard wire eqilibrium
    w.a = 1
    w.b = -.1
    b.a = 0
    b.b = .1
    d.a = .1
    d.b = -.1
    N0.init = 5

    ##hard wired (these are used for time before conditions change)
    N.vec.tmp <- 0:10
    w.vec.tmp <- w.fun(N.vec.tmp, w.alpha=w.a, w.beta=w.b)
    b.vec.tmp <- b.fun(w.vec.tmp, b.alpha=b.a, b.beta=b.b)
    d.vec.tmp <- d.fun(w.vec.tmp, d.alpha=d.a, d.beta=d.b)

    ## reactive
    N.vec.prime <- 0:10
    w.vec.prime <- w.fun(N.vec.prime, w.alpha=w.alpha, w.beta=w.b)
    b.vec.prime <- b.fun(w.vec.prime, b.alpha=b.alpha, b.beta=b.b)
    d.vec.prime <- d.fun(w.vec.prime, d.alpha=d.alpha, d.beta=d.b)

    ## initialize
    w.vec <- N.vec <- rep(NA, time.max)
    b.vec <- d.vec <- rep(NA, time.max)
    N.vec[1] <- N0.init
    w.vec[1] <- w.fun(N0.init, w.alpha = w.a, w.beta = w.b)
    b.vec[1] <- b.fun(w.vec[1], b.alpha = b.a, b.beta = b.b)
    d.vec[1] <- d.fun(w.vec[1], d.alpha = d.a, d.beta = d.b)


    ## do whole simulation, storing output
    ## note N.vec is sufficient to get other values
    prime = 10                       # this is time when things change
    for (i in 2:prime)
    {
        ## before change
        N <- N.vec[i-1]
        w <- w.fun(N, w.a, w.b)
        b <- b.fun(w, b.a, b.b)
        d <- d.fun(w, d.a, d.b)
        N.vec[i] <- N * (1 + b - d)
        w.vec[i] = w
        b.vec[i] <- b
        d.vec[i] <- d
    }
    ##
    if (i == prime)
    {
                                        #        print(c("bingo", N0))
        N.vec[i] <- N0      # we change N to user set value at t = prime
        ## note that next loop begins with prime+1
                                        #        print(N.vec[i])
    }
    for (i in (prime+1):time.max)
    {

        ## after change
        N <- N.vec[i-1]
        ## par(mfcol = c(2,2))
        ## plot(w.vec.tmp, b.vec.tmp, type = "l", col = "forestgreen",
        ##      ylim = c(0, .2))
        ## lines(w.vec.tmp, d.vec.tmp, type = "l", col = "red")
        ## points(w.fun(N), b.fun(w.fun(N)), col = "forestgreen")
        ## points(w.fun(N), d.fun(w.fun(N)), col = "red")

        ## plot(w.vec.tmp, N.vec.tmp, type = "l", lwd = 2)
        ## points(w.fun(N), N, col = "black")

        w <- w.fun(N, w.alpha, w.beta)
        b <- b.fun(w, b.alpha, b.beta)
        d <- d.fun(w, d.alpha, d.beta)
        N.vec[i] <- N * (1 + b - d)
        ##         print(paste(b, d, i,N))
        w.vec[i] = w
        d.vec[i] <- d
        b.vec[i] <- b
    }
    ## ------------ end of full run thru of simu -----------
##    print(N.vec)
##    print("j")
##    print(j)
    ## now plot process up to time j
    i <- j
    N <- N.vec[i]
##    print(i)

    par(mfcol = c(2,2))
    ## Demography
    plot(w.vec.tmp, b.vec.tmp, type = "l", col = "forestgreen",
         xlab = "Wages w",
         ylab = "Vital rates",
         lwd = 2,
         ylim = c(0, .1))
    text(x = .1, y = .03,
         "birth rate", col = "forestgreen")
    text(x = .1, y = .07,
         "death rate", col = "red")
    lines(w.vec.tmp, d.vec.tmp,
          lwd = 2,
          type = "l", col = "red")
    lines(w.vec.prime, d.vec.prime,
          lwd = 2, type = "l", lty = 2, col = "red")
    lines(w.vec.prime, b.vec.prime,
          lwd = 2, type = "l", lty = 2, col = "forestgreen")
    ## trace of points
    points(w.vec[1:i], b.vec[1:i], col = "grey", pch = 19)
    points(w.vec[1:i], d.vec[1:i], col = "grey", pch = 19)
    ## print("w.vec")
    ## print(w.vec)
    ## print("b.vec")
    ## print(b.vec)
    ## print("d.vec")
    ## print(d.vec)


    if(i  <= prime)                     # b, hardwired
        points(w.fun(N, w.a, w.b),
               b.fun(w.fun(N, w.a, w.b), b.a, b.b),
               cex = point.cex,
               col = "forestgreen")
    if (i > prime)                      # b, dynamic
        points(w.fun(N, w.alpha, w.beta),
               b.fun(w.fun(N, w.alpha, w.beta), b.alpha, b.beta),
               cex = point.cex,
               col = "forestgreen")
    if(i  <= prime)                     # d, hardwired
        points(w.fun(N, w.a, w.b),
               d.fun(w.fun(N, w.a, w.b), d.a, d.b),
               cex = point.cex,
               col = "red")
    if (i > prime)                      #d, dynamic
        points(w.fun(N, w.alpha, w.beta),
               d.fun(w.fun(N, w.alpha, w.beta), d.alpha, d.beta),
               cex = point.cex,
               col = "red")
    title("Demography")
    ##
    ## Economy
    plot(w.vec.tmp, N.vec.tmp, type = "l", lwd = 2,
         ylab = "Population N", xlab = "Wages w")
    ## Plot trace of economy
    ## Since w(t) a function of N(t-1), we re-align
    if (i < time.max)
        points(w.vec[2:(i+1)], N.vec[1:i], col = "grey", pch = 19)
    if (i == time.max)       # so plot won't disappear at end
        points(w.vec[2:(i)], N.vec[1:(i-1)], col = "grey", pch = 19)
    ## plot latest value as a point
    if(i  <= prime)
        points(w.fun(N, w.a, w.b), N,
               cex = point.cex,
               col = "black")
    if (i > prime)
        points(w.fun(N, w.alpha, w.beta), N,
               cex = point.cex,
               col = "black")
    lines(w.vec.prime, N.vec.prime, type = "l", lwd = 2, lty = 2)
    title("Economy")
    ## Population over time
    ##     plot(x = c(0, time.max), c(0, 10), type = "n", axes = F,
    plot(x = seq(N.vec), N.vec, type = "n", axes = F,
         xlab = "Time", ylab = "Population N",
         ylim = range(N.vec) * c(.8, 1.2)) # ylim > range
    axis(1, at = seq(0, 100, 10), labels = seq(-10, 90, 10))
    axis(2)
    abline(v = prime, lty = 2)
    lines(1:i, N.vec[1:i])
    points(i, N, cex = point.cex,
           col = "black")
    title("Population over time")
    ## wages over time
    ##    plot(x = c(0, time.max), c(.3, .7), type = "n", axes = F,
    plot(x = seq(w.vec), w.vec, type = "n", axes = F,
         xlab = "Time", ylab = "Wages w",
         ylim = range(w.vec, na.rm = T) * c(.8, 1.2)) # ylim > range
    axis(1, at = seq(0, 100, 10), labels = seq(-10, 90, 10))
    axis(2)
    abline(v = prime, lty = 2)
    lines(1:i, w.vec[1:i])
    points(i, w.vec[i],    cex = point.cex,
           col = "black")
    title("Wages over time")

    return(NULL)
}


## Define server logic required to draw plot
shinyServer(function(input, output) {

    ## Expression that generates a histogram. The expression is
    ## wrapped in a call to renderPlot to indicate that:
    ##
    ##  1) It is "reactive" and therefore should re-execute automatically
    ##    when inputs change
    ##  2) Its output type is a plot

    output$malthusPlot <- renderPlot({
        malthus.step(j = input$t + 10, # t starts at -9, so t = 0 <-> j = 10
                     N0 = input$N0,
                     w.alpha = input$w.alpha,
                     b.alpha = input$b.alpha,
                     d.alpha = input$d.alpha,
                     w.beta = -.1,
                     b.beta = 1/10,
                     d.beta = -1/10)

    })
})
