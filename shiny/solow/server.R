## server.R for Solow_2017

##

library(shiny)



point.cex = 1.5

## y.fun replaces w.fun
y.fun <- function(A = 1, k, alpha)
{
    y <- A * k^alpha
    y[y < 0] <- 0
    return(y)
}

## b.fun is now the new capital. it is a function of k and s
b.fun <- function(k, alpha, s, A)
{
    b <- s * y.fun(A, k, alpha)
    b[b < 0] <- 0
    b
}

d.fun <- function(k, n, delta)
{
    death <- (n + delta) * k
    death[death < 0] <- 0
    death
}


## this big function, starts by running the whole simulation through,
## and then it displays everything up to time j.  (it's a bit messy in
## that the variable names aren't very clear; there are vectors that
## keep track of the whole timepath, scalars that we iterate over,
## etc.)

solow.step <- function(j, ## j in time index in simulation
                       k0,
                       time.max = 100,
                       A,
                       alpha,
                       n,
                       delta,
                       s)

{
    ## hard wire equilibrium
    A.default = 1
    alpha.default = 2/5
    k0.default = 10
    n.default = .01
    delta.default = .04
    s.default = .2
    k0.init = k0.default

    ## hard wired (these are used for time before conditions change)
    k.vec.tmp <- seq(0, 20, .1)
    y.vec.tmp <- y.fun(A = 1, k.vec.tmp, alpha=alpha.default)
    b.vec.tmp <- b.fun(k.vec.tmp, alpha = alpha.default, s=s.default, A = 1)
    d.vec.tmp <- d.fun(k.vec.tmp, n = n.default, delta = delta.default)

    ## reactive (used only for plotting, i think)
    k.vec.prime <- k.vec.tmp            #  a broad range of values
    ## will recompute actual k.vec below.
    y.vec.prime <- y.fun(A = A, k.vec.prime, alpha=alpha)
    b.vec.prime <- b.fun(k.vec.prime, alpha = alpha, s=s, A=A)
    d.vec.prime <- d.fun(k.vec.prime, n = n, delta = delta)


    ## initialize the dynamic values to be calculated
    k.vec <- y.vec <- rep(NA, time.max)
    b.vec <- d.vec <- rep(NA, time.max)
    ##
    k.vec[1] <- k0.init
    y.vec[1] <- y.fun(A = 1, k0.init, alpha = alpha.default)
    b.vec[1] <- b.fun(k.vec[1], alpha = alpha.default, s = s.default, A = 1)
    d.vec[1] <- d.fun(k.vec[1], n = n.default, delta = delta.default)


    ## do whole simulation, storing output
    ## note k.vec is sufficient to get other values
    prime = 10                       # time when things change
    for (i in 2:prime)
    {
        ## before change
        k <- k.vec[i-1]
        y <- y.fun(A = A.default, k, alpha.default)
        b <- b.fun(k, alpha.default, s.default, A = A.default)
        d <- d.fun(k, n.default , delta.default)
        k.vec[i] <- k + b - d ## k * (1 + b - d)
        b.vec[i] <- b
        d.vec[i] <- d
        y.vec[i] <- y
    }
    ##
    if (i == prime)
    {
        k.vec[i] <- k0      # we change N to user set value at t = prime
        ## note that next loop begins with prime+1
    }
    for (i in (prime+1):time.max)
    {
        ## after change
        k <- k.vec[i-1]
        y <- y.fun(A, k, alpha)
        b <- b.fun(k, alpha, s, A = A)
        d <- d.fun(k, n , delta)
        k.vec[i] <- k + b - d ## k * (1 + b - d)
        b.vec[i] <- b
        d.vec[i] <- d
        y.vec[i] <- y
    }
    ## ------------ end of full run thru of simu -----------
    ## now plot process up to time j
    i <- j
    k <- k.vec[i]
    par(mfcol = c(2,2))

    ## Demography
    plot(k.vec.tmp, b.vec.tmp, type = "l", col = "forestgreen",
         xlab = "Capital/labor = k = K/L",
         ylab = "Changes in capital",
         lwd = 2,
         ylim = c(0, max(d.vec.tmp)*1.2))
    text(x = 2, y = b.vec.tmp[k.vec.tmp == 2]*1.5,
         "s y", col = "forestgreen")
    text(x = 7, y = d.vec.tmp[k.vec.tmp == 7]*.5,
         "(d+n) k", col = "red")
    lines(k.vec.tmp, d.vec.tmp,
          lwd = 2,
          type = "l", col = "red")
    lines(k.vec.prime, d.vec.prime,
          lwd = 2, type = "l", lty = 2, col = "red")
    lines(k.vec.prime, b.vec.prime,
          lwd = 2, type = "l", lty = 2, col = "forestgreen")
    ## trace of points
    if(i < time.max) {
        points(k.vec[1:i], b.vec[2:(i+1)], col = "grey", pch = 19)
        points(k.vec[1:i], d.vec[2:(i+1)], col = "grey", pch = 19)
    }
    if(i == time.max) {
        points(k.vec[1:(i-1)], b.vec[2:i], col = "grey", pch = 19)
        points(k.vec[1:(i-1)], d.vec[2:i], col = "grey", pch = 19)
    }

    if(i  <= prime)                     # b, hardwired
        points( k0.default,
               b.fun(k0.default, alpha.default, s.default, A = 1),
               cex = point.cex,
               col = "forestgreen")

    if (i > prime)                      # b, dynamic
        points(k,
               b.fun(k, alpha, s, A = A),
               cex = point.cex,
               col = "forestgreen", pch = 19)
    if(i  <= prime)                     # d, hardwired
        points( k0.default,
               d.fun(k0.default, n = n.default, delta = delta.default),
               cex = point.cex,
               col = "red")
    if (i > prime)                      #d, dynamic
        points(k,
               d.fun(k, n, delta),
               cex = point.cex,
               col = "red")
    title("Investment, depreciation, and population growth")
    ##

    ## Economy
    plot(k.vec.tmp, y.vec.tmp, type = "l", lwd = 2,
         ylab = "Output y",
         xlab = "Capital/labor = k = K/L",
         ylim = c(0, 5))
    ## Plot trace of economy
    ## Since w(t) a function of N(t-1), we re-align
    if (i < time.max)
        points(k.vec[1:i], y.vec[2:(i+1)], col = "grey", pch = 19)
    if (i == time.max)       # so plot won't disappear at end
        points(k.vec[1:(i-1)], y.vec[2:i], col = "grey", pch = 19)
    if(i == 10)
        print(cbind(k.vec, y.vec))
    ## plot latest value as a point
    if(i  < prime)
        points(k0.default, y.fun(A = 1, k, alpha.default),
               cex = point.cex,
               col = "black")
    if (i >= prime)
        points(k, y.fun(A = A, k, alpha),
               cex = point.cex,
               col = "black")
    lines(k.vec.prime, y.vec.prime, type = "l", lwd = 2, lty = 2)
    title("Production function")
    ## Capital over time

    plot(x = seq(k.vec), k.vec, type = "n", axes = F,
         xlab = "Time", ylab = "Capital/Labor k",
         ylim = range(k.vec) * c(.8, 1.2)) # ylim > range
    axis(1, at = seq(0, 100, 10), labels = seq(-10, 90, 10))
    axis(2)
    abline(v = prime, lty = 2)
    lines(1:i, k.vec[1:i], lwd = 2)
    points(i, k, cex = point.cex,
           col = "black")
    title("Capital per person k, over time")
    ## income over time
    plot(x = seq(y.vec), y.vec, type = "n", axes = F,
         xlab = "Time", ylab = "Output y",
         ylim = range(y.vec, na.rm = T) * c(.8, 1.2)) # ylim > range
    axis(1, at = seq(0, 100, 10), labels = seq(-10, 90, 10))
    axis(2)
    abline(v = prime, lty = 2)
    lines(1:i, y.vec[1:i], lwd = 2)
    points(i, y.vec[i],    cex = point.cex,
           col = "black")
    title("Output per person y, over time")

    return(NULL)
}


## Define server logic required to draw plot
shinyServer(function(input, output) {

    ## Expression that generates our plot
    ## wrapped in a call to renderPlot to indicate that:
    ##
    ##  1) It is "reactive" and therefore should re-execute automatically
    ##    when inputs change
    ##  2) Its output type is a plot

    output$solowPlot <- renderPlot({
        solow.step(j = input$t + 10, # t starts at -9, so t = 0 <-> j = 10
                   k0 = input$k0,
                   A = input$A,
                   alpha = input$alpha,
                   n = input$n,
                   delta = input$delta,
                   s = input$s)
    })
})
