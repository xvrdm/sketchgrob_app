library(shiny)
library(grid)

ui <- fluidPage(

   titlePanel("Sketchy splines"),

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
  a <- (y[2]-y[1])/(x[2]-x[1])
  b <- ((y[1]/y[2])*a*x[2] - a*x[1])/(1-(y[1]/y[2]))
  mid_x_pool <- seq(x[1], x[2], length=100)

   output$distPlot <- renderPlot({
     pushViewport(plotViewport(c(4, 4, 4, 4)))
     pushViewport(dataViewport(xData=0:5, yData=0:5,
                               xscale = c(0,5), yscale = c(0,5)))
     grid.xaxis()
     grid.yaxis()

     grid.points(x, y, pch = 1)

     y_noise_threshold <-
     y_noise_pool <- seq(input$spline_noise[1], input$spline_noise[2], length=100)

     for(i in seq_along(1:input$lines)) {
       randx <- sort(sample(mid_x_pool, input$spline_points))
       randy <- (randx*a) + b + sample(c(1,-1), length(randx), replace=TRUE)*sample(y_noise_pool, length(randx))
       new_x <- c(x[1], randx, x[2])
       new_y <- c(y[1], randy, y[2])
       if(input$spline_show) grid.points(randx, randy, pch = 16)
       grid.xspline(x = new_x, y = new_y, open=TRUE, default.units = "native",
                    gp = gpar(lwd=input$lwd, alpha=input$alpha),
                    shape = sample(seq(0.5, 1, length=100), length(new_x)))
     }
   })
}

shinyApp(ui = ui, server = server)

