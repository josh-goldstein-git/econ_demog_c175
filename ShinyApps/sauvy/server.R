## R functions called by server

xx <- c(1, 250, 500, 750, 1000)

sauvy_opt_fun <- function(mp1, ##  = 0,
                          mp25, ##  = 10,
                          mp50, ##  = 10,
                          mp75, ## = 5,
                          mp100, ## = 0,
                          s_constant)##  = 7)
{
n <- 1:1000
mp <- approx(x = xx,
             y = c(mp1, mp25, mp50, mp75, mp100),
             xout = n)$y
( ap <- cumsum(mp)/n )
s <- rep(s_constant, length(n))
( pow <- cumsum(mp -s) )
if (sum(ap >=s) > 0)
    {
        max.pop <- max(n[ap >= s])
        min.pop <- min(n[ap >= s])
        }
else
    min.pop <- max.pop <- NA

econ.opt <- n[which.max(ap)] + 1
power.opt <- n[which.max(cumsum(mp - s))] + 1

par(mfrow = c(1,1))
plot(n, mp, col = "orange", type = 'l', pch = paste(1:5),
     axes = F, lwd = 3,
     ylab = "Units of production/consumption",
     xlab = "Population")
axis(2)
points(x = xx,
       y = mp[xx],
       pch = paste(1:5),
       cex = 1.3,
       col = "black")
abline(v = c(min.pop, max.pop),
       col = "grey", lty = 2)
abline(v = econ.opt,
       col = "red", lty = 2)
abline(v = power.opt,
       col = "orange", lty = 2)
axis(3)

lines(n, ap, col = "red", type = 'l', pch = 1, lwd = 3)
lines(n, s, col = "black", type = 'l', pch = 1, lwd = 3)
## title("Marginal Values")
text(0 + 400, s[400], "subsistence", pos = 1)
text(0 + 700, ap[700], "average product",  pos = 4, col = 'red')
text(0 + 250, mp[250], "marginal product", pos = 3, col = 'orange')
## abline(v = n[which.max(ap)], col = "red")
## abline(v = econ.opt, col = "red")
## text(y = max(mp)/2, x = econ.opt, "economic optimum", srt = 90,
##      pos = 3)
## abline(v = power.opt, col = "orange")
## text(y = max(mp)/2, x = power.opt, "power optimum", srt = 90,
##      pos = 1)
## abline(v = max.pop)
## abline(v = min.pop)
axis(side = 1,
     tick = F,
     labels = F)
axis(side = 1, at = c(econ.opt), ## , power.opt), ## , min.pop, max.pop),
     labels = c("Economic\n Optimum"), ## , "Power\n Optimum"),## , "Min Pop", "Max Pop"),
     line = -2,
     tick = F,
     col.axis = c("blue"))
axis(side = 1, at = c(power.opt), ## , min.pop, max.pop),
     labels = c("Power\n Optimum"),## , "Min Pop", "Max Pop"),
     line = -0,
     tick = F,
     col.axis = c("blue"))
axis(side = 1, at = # c(econ.opt, power.opt,
                   c(min.pop, max.pop),
     labels = ## c("Economic\n Optimum", "Power\n Optimum",
         c("Min Pop", "Max Pop"),
     line = +1,
     tick = F,
     col.axis = c("red"))

##
## plot(n, cumsum(mp), col = "orange", type = 'l', pch = 19,
##      lwd = 3,
##      ylim = c(0, pmax(max(cumsum(mp)), max(cumsum(s)))),
##      axes = F)
## ## lines(n, cumsum(ap), col = "red", type = 'b', pch = 19)
## lines(n, cumsum(s), col = "black", type = 'l', pch = 19, lwd = 3)
## lines(n, cumsum(mp), col = "orange", type = 'l', pch = 19, 
##      lwd = 3)
## title("Total Product")
## text(0 + 20, cumsum(mp)[20], "total product",  col = 'orange')
## text(0 + 20, cumsum(s)[20], "total subsistence", col = 'black')
}

## sauvy_opt_fun(mp1 = 10, mp25 = 4, mp50 = 10, s_constant = 5)


## Define server logic required to draw plot
library(shiny)
shinyServer(function(input, output) {
    ## Expression that generates a histogram. The expression is
    ## wrapped in a call to renderPlot to indicate that:
    ##
    ##  1) It is "reactive" and therefore should re-execute automatically
    ##    when inputs change
    ##  2) Its output type is a plot
    output$peakPlot <- renderPlot({
        sauvy_opt_fun(mp1 = input$mp1,
                      mp25 = input$mp25,
                      mp50 = input$mp50,
                      mp75 = input$mp75,
                      mp100 = input$mp100,
                      s_constant = input$s_constant)

    })
})
