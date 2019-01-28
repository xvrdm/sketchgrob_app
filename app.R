library(shiny)
library(grid)

ui <- fluidPage(

   titlePanel("Sketchy splines"),
   h6(a("Link to Github", href="https://github.com/xvrdm/sketchgrob_app")),
   sidebarLayout(
      sidebarPanel(
         sliderInput("lines",
                     "Number of lines:",
                     min = 1,
                     max = 50,
                     value = 3),
         sliderInput("lwd",
                     "Line width:",
                     min = 0.1,
                     max = 3,
                     value = 1),
         sliderInput("alpha",
                     "Line opacity:",
                     min = 0,
                     max = 1,
                     value = 0.5),
         sliderInput("spline_points",
                     "Number of spline points:",
                     min = 1,
                     max = 10,
                     value = 3),
         checkboxInput("spline_show",
                       "Show spline points",
                       value = FALSE),
         sliderInput("spline_noise",
                     "Spline points noise:",
                     min = 0,
                     max = 0.5,
                     value = c(.05,0.1))
      ),

      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
  x <- c(0.5,5)
  y <- c(0.5,4)

  slope <- function(x1,y1,x2,y2) (y2-y1)/(x2-x1)

  intercept <- function(x1,y1,x2,y2,a) ((y1/y2)*a*x2 - a*x1)/(1-(y1/y2))


  rand_x <- function(x1, x2, spline_points) {
    mid_x_pool <- seq(x1, x2, length=100)
    sort(sample(mid_x_pool, size=spline_points))
  }

  rand_y <- function(x, a, b, y_noise_pool){
    x*a + b + sample(c(1,-1), 1)*sample(y_noise_pool, 1)
  }

  a <- slope(x[1], y[1], x[2], y[2])
  b <- intercept(x[1], y[1], x[2], y[2], a)

  output$distPlot <- renderPlot({
    pushViewport(plotViewport(c(4, 4, 4, 4)))
    pushViewport(dataViewport(xData=0:5, yData=0:5,
                               xscale = c(0,5), yscale = c(0,5)))
    grid.xaxis()
    grid.yaxis()

    grid.points(x, y, pch = 1)

    y_noise_pool <- seq(input$spline_noise[1], input$spline_noise[2], length=100)

    get_spline_coords <- function(x1, y1, x2, y2, a, b, y_noise_pool, spline_points) {
      randx <- rand_x(x1, x2, spline_points)
      randy <- unlist(lapply(randx, rand_y, a, b, y_noise_pool))
      list(
        x=c(x1, randx, x2),
        y=c(y1, randy, y2)
      )
    }

    splines <- replicate(input$lines,
                         get_spline_coords(x[1], y[1], x[2], y[2],
                                           a, b, y_noise_pool,
                                           input$spline_points))

    xs <- unlist(splines[1,])
    ys <- unlist(splines[2,])

    if(input$spline_show) grid.points(xs, ys, pch = 16)

    grid.xspline(x = xs, y = ys, open=TRUE, default.units = "native",
                 id.lengths = rep(input$spline_points + 2, input$lines),
                 gp = gpar(lwd=input$lwd, alpha=input$alpha),
                 shape = sample(seq(0.5, 1, length=100), length(xs)),
                 repEnds = T)
   })
}

shinyApp(ui = ui, server = server)

