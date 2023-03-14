## one-file shiny app for cost-of-time
## we show only the "prime" values as sliders

n.max = 8

############# functions
x.fun <- function(U, N, a)
{
    ##    print("calculating x as a function of U and N, and a")
    ##    U           = X^a * N^(1-a)
    X = (U / (N^(1-a)))^(1/a)
    return(X)
}



draw.isoquants <- function(levels, a, xmax = NULL, ymax = NULL)
{
    ##    print("drawing isoquants")
    N = seq(.1, n.max, by = .01)
    if (is.null(xmax ))
        xmax = ymax = max(N)
    plot(N, N, type = "n",
         xlab = "N children",
         ylab = "X goods ($ per week)",
         ylim = c(0, ymax),
         xlim = c(0, xmax))
    title("Utility maximization based on the optimal consumption choice of N children and X goods")
    for (i in 1:length(levels))
    {
        lines(N, x.fun(U = levels[i], N, a),
              col = "grey", lty = 3)
        text(max(N), x.fun(U = levels[i], max(N), a),
             pos = 4,
             cex = .6,
             col = "grey",
             round(levels[i], 1))
    }
    text(xmax * .9, ymax * .9,
         "Utility: U = X^a N^(1-a)")
    text(xmax * .9, ymax * .8,
         "Budget constraint: I + wT = (p + wc) N + X")
    text(xmax * .9, ymax * .7,
         "Fertility choice: N* = (1-a) * (I + w T) / (p + c w)")


}


draw.budget.constraint <- function(p,
                                   c,
                                   w,
                                   T,
                                   I, ...)
{

    ##    print("drawing budget constraints")
    segments(x0 = 0, y0 = w*T + I,
             x1 = (w*T + I)/(p + w*c), y1 = 0, ...)
}

draw.solution <- function(w, p, c, I, T, a, draw = TRUE, ...)
{
    ##    print("drawing solution")
    ## zhou yi has 1-alpha:    N.star = (1-a)*(I + w*T)/(p + c*w)
    N.star = (1-a)*(I + w*T)/(p + c*w)
    ##     print(N.star)
    tot.income = I + T*w
    kid.expenses = N.star * (p + c*w)
    p.x = 1
    X.star = (tot.income - kid.expenses)/p.x
    if (draw == TRUE)
    {
        points(N.star, X.star, col = "red", pch = 19)
        text(N.star, X.star, paste("w = ", w),
             col = "red", pos = 2, cex = 1)
        text(N.star, X.star, paste0("(", round(N.star,1), ", ",
                                    round(X.star), ")"),
             col = "black", pos = 4, cex = 1)

    }
    ## draw the tangent isoquant
    U = N.star^(1-a) * X.star^(a)
    N.vec <- seq(0, n.max, .1)
    if (draw == TRUE)
        lines(N.vec, x.fun(U = U, N.vec, a), lwd = 1, ...)
    return(c("N.star"=N.star, "X.star" =X.star))
}

consumer.choice.fun <- function(w.vec,
                                p, pp,
                                c, cp,
                                I, Ip,
                                T,
                                a)
{

    ## hardwire in p, c, I, T, and a
                                        #    p = 100
                                        #    c = 10
                                        #    I = 1000
                                        #    T = 40
                                        #    a = .6
    xmax = n.max
    ymax = max(w.vec)*T + I
    sol = draw.solution(w = max(w.vec),
                        p=p, c=c, I=I, T=T, a = a)


    N.star <- sol["N.star"]
    X.star <- sol["X.star"]
    U.max <- X.star^a * N.star^(1-a)

    my.levels = seq(0, round(U.max*.15)*10,
                    by = 10)
    print(my.levels)
    draw.isoquants(levels = my.levels,
                   a = a,
                   xmax = xmax, ymax = ymax)

    print("T/c")
    print(T/c)
    print("I/p")
    print(I/p)
    if ((T/c) < (I/p))
        print("fertility declines")
    if ((T/c) > (I/p))
        print("fertility increases")

    ## draw according to initial conditions
    w = w.vec[1]
    draw.budget.constraint(w = w, p = p, c = c, I = I, T = T,
                           lwd = 4)
    draw.solution(w = w,
                  p=p, c=c, I=I, T=T, a = a, draw = TRUE)

    ## draw according to perturbed values
    ##    for (i in 1:2)
    ##    {
    wp = w.vec[2]
    ## initial draw
    draw.budget.constraint(w = wp, p = pp, c = cp, I = Ip, T = T,
                           lwd = 4, col = "grey")
    draw.solution(w = wp,
                  p=pp, c=cp, I=Ip, T=T, a = a, draw = TRUE,
                  col = "grey")
    ## redraw prime
    ##        draw.budget.constraint(w = w, p = p, c = c, I = I, T = T,
    ##                               col = "black", lwd = 2)
    ##        draw.solution(w = w,
    ##                      p=p, c=c, I=I, T=T, a = a, draw = TRUE,
    ##                      col = "black")
    ## redraw initial
    draw.budget.constraint(w = w, p = p, c = c, I = I, T = T,
                           lwd = 2)
    draw.solution(w = w,
                  p=p, c=c, I=I, T=T, a = a, draw = TRUE)

    ##    }
}

############## end of functions

server <- function(input, output) {
    output$choicePlot <- renderPlot({
        w.vec <- c(input$w, input$wp)
        consumer.choice.fun(w.vec = w.vec,
                            p = input$p,
                            pp = input$pp,
                            c = input$c,
                            cp = input$cp,
                            I = input$I,
                            Ip = input$Ip,
                            T = input$T,
                            a = input$a)
    })
    output$wp.slider <- renderUI({
        sliderInput("wp", "wage rate per hour w'",
                    min = 0, max = 50, value = input$w,
                    step = 5,
                    width = my.width, ticks = my.ticks)
    })
    output$pp.slider <- renderUI({
        sliderInput("pp", "per child fixed costs p'",
                    min = 0, max = 500,
                    value = input$p,
                    step = 50,
                    width = my.width, ticks = my.ticks)
    })
    output$cp.slider <- renderUI({
        sliderInput("cp", "per child time costs c'",
                    min = 0, max = 40,
                    value = input$c,
                    step = 1,
                    width = my.width, ticks = my.ticks)
    })
    output$Ip.slider <- renderUI({
        sliderInput("Ip", "per week 'outside' income I'",
                    min = 0, max = 2000,
                    value = input$I,
                    step = 200,
                    width = my.width, ticks = my.ticks)
    })
}

my.width = "100%"
my.ticks = FALSE
ui <- fluidPage(
    titlePanel("Cost-of-Time Fertility Model"),
    plotOutput("choicePlot"),
    h4("Instructions: (1) Change 'Parameters to perturb' for comparative statics. (e.g., shift w' from 15 to 25 dollars per hour) (2) Change 'Initial parameter values' _and_ 'Parameters to perturb' for comparative statics with different initial conditions. (e.g., change c from 10 hours per child to 1 hour per child, and then look at the effect of increased wages by changing w' from 15 to 20.) Note: if you change 'initial parameter values', the 'parameters to perturb' shift automatically to match the initial values; but if you change the 'parameters to perturb', it will have no effect on the initial values."),
    hr(),
    fluidRow(
        column(4,
               h4("Initial parameter values"),
               h4("The consumer choice based on the initial parameter values (w, p, c, I) will be graphed black."),
               ## initial parameters
               sliderInput("w", "base wage rate per hour w",
                           min = 0, max = 50, value = 15,
                           step = 5,
                           width = my.width, ticks = my.ticks),
               sliderInput("p", "per child fixed costs p",
                           min = 0, max = 500,
                           value = 100,
                           step = 50,
                           width = my.width, ticks = my.ticks),
               sliderInput("c", "per child time costs c",
                           min = 0, max = 40,
                           value = 10,
                           step = 1,
                           width = my.width, ticks = my.ticks),
               sliderInput("I", "per week 'outside' income I",
                           min = 0, max = 2000,
                           value = 1000,
                           step = 200,
                           width = my.width, ticks = my.ticks)
               ),
        column(4,
               h4("Parameters to perturb"),
               h4("The consumer choice based on the perturbed parameter values (w', p', c', I') will be graphed grey."),
               ## perturbation parameters
               uiOutput("wp.slider"),
               uiOutput("pp.slider"),
               uiOutput("cp.slider"),
               uiOutput("Ip.slider")
        ## sliderInput("wp", "(new) hourly wage w'",
        ##             min = 0, max = 50, value = 15,
        ##             step = 5,
        ##             width = my.width, ticks = my.ticks),
        ## sliderInput("pp", "(new) per child fixed costs p'",
        ##             min = 0, max = 500,
        ##             value = 100,
        ##             step = 50,
        ##             width = my.width, ticks = my.ticks),
        ## sliderInput("cp", "(new) per child time costs c'",
        ##             min = 0, max = 40,
        ##             value = 10,
        ##             step = 1,
        ##             width = my.width, ticks = my.ticks),
        ## sliderInput("Ip", "(new) per week 'outside' income I'",
        ##             min = 0, max = 2000,
        ##             value = 1000,
        ##             step = 200,
        ##             width = my.width, ticks = my.ticks)
    ),
    column(4,
           h4("Defaults"),
           h4("You don't need to change these values. But you can if you want."),
           sliderInput("T", "hours in the week T",
                       min = 0, max = 60,
                       value = 40, step = 5,
                       width = my.width, ticks = my.ticks),
           sliderInput("a", "income elasticity of goods a",
                       min = 0, max = 1,
                       value = .6, step = .1,
                       width = my.width, ticks = my.ticks)
           )
)
)

shinyApp(ui = ui, server = server)
