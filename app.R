library(shiny)
library(cookies)
library(shinyjs)

source("./nzTides.R")

userQuery <- NULL
tc <- NULL
tac <- NULL

ui <-
  add_cookie_handlers(
    fluidPage(
      useShinyjs(),

      tags$script("
        function getCurrentDateTime() {
          var now = new Date();

          var year = now.getFullYear();
          var month = (now.getMonth() + 1).toString().padStart(2, '0');
          var day = now.getDate().toString().padStart(2, '0');
          var hours = now.getHours().toString().padStart(2, '0');
          var minutes = now.getMinutes().toString().padStart(2, '0');

          var dateTime = year + month + day + hours + ':' + minutes;
          return dateTime;
        }
      "),

      titlePanel("New Zealand tide forecast."),

      br(),

      tags$p("This app is a simple demonstration of R, and Shiny.  It plots tide charts for New Zealand ports, and the sunrise and sunset times with an approximation of the golden hours."),

      hr(),

      plotOutput("plotChart", height = 600, click = "click"),

      br(),

      tags$p(
          "The author accepts no responsibility for any loss or damage resulting from the use of this chart.",
          tags$br(),
          "All values shown are approximate.",
          tags$br(),
          "The data presented in this chart is intended solely for informational purposes.",
          tags$br(),
          "No decision should be made based on the information shown.",
          style = "color:red"),

      hr(),

      fluidRow(
        column(
          width = 4,
          wellPanel(
            selectInput(inputId  = "location",
                        label    = "Port:",
                        choices  = portsList,
                        selected = "Auckland"),

            sliderInput(inputId  = "periods",
                        label    = "Tide segments:",
                        min      = 5,
                        max      = 26,
                        value    = 7),

            fluidRow(
              column(
                width = 2,
                actionButton(inputId = "refresh",
                            label   = "Refresh chart"),
              ),
              column(
                width = 2,
                offset = 1,
                actionButton(inputId = "clear",
                            label   = "Clear queries")
              )
            )
          )
        ),

        column(
          width = 4,
          offset = 4,
          verbatimTextOutput("userDateTime")
        )
      )
    )
  )


server <-
  function(input, output, session) {

    output$plotChart <-
      renderPlot({
        runjs("var currentDateTime = getCurrentDateTime(); Shiny.onInputChange('currentDateTime', currentDateTime);")
        latDec <- mainPorts$latitude_dec[mainPorts$port == input$location]
        longDec <- mainPorts$longitude_dec[mainPorts$port == input$location]

        newPort <- input$location != tc$port
        tc <<- tideChart(input$location, longDec, latDec, input$currentDateTime, periods = input$periods, dylt = dylt)

        if ((!is.null(input$click$x) | !is.null(tac)) && !newPort) {
          if (!is.null(input$click$x)) {
            x <- input$click$x
          } else {
            x <- tac$decimalTime
          }
          tac <<- tideAtMouseX(tc$tideHeights, x, tc$decimalTimeAdj)
          abline(v = tac$decimalTime, col = "red")
        }
      })

    observeEvent(
      input$refresh,
      {
        output$plotChart <- renderPlot({
          runjs("var currentDateTime = getCurrentDateTime(); Shiny.onInputChange('currentDateTime', currentDateTime);")
          latDec <- mainPorts$latitude_dec[mainPorts$port == input$location]
          longDec <- mainPorts$longitude_dec[mainPorts$port == input$location]

          tc <<- tideChart(input$location, longDec, latDec, input$currentDateTime, periods = input$periods, dylt = dylt)
        })
      }
    )

    observeEvent(
      input$clear,
      {
        output$userDateTime <- renderText({
          userQuery <<- NULL
          tac <<- NULL

          "Click on the chart to get the tide height at that time."
        })
      }
    )

    observeEvent(
      input$click,
      {
        output$userDateTime <-
          renderText({
            d_ <- format(as.Date(substr(tac$date, 1, 8), "%Y%m%d"), "%a %d %b %Y")
            t_ <- tac$properTime

            if (is.null(userQuery)) {
              userQuery <<- paste0(tc$port, ": ", d_, " ", t_, " >> ", round(tac$height_m, digits = 2), "m")
            } else {
              userQuery <<- paste0(userQuery, "\n", tc$port, ": ", d_, " ", t_, " >> ", round(tac$height_m, digits = 2), "m")
            }

            userQuery
          })
      }
    )

    output$userDateTime <- renderText({
      paste("Click on the chart to get the tide height at that time.")
    })

    ## Cookie handlers
    ## setting cookies
    observeEvent(
      input$location,
      {
        cookies::set_cookie_response(
          cookie_name  = "location",
          cookie_value = input$location
        )
      }
    )

    observeEvent(
      input$periods,
      {
        cookies::set_cookie_response(
          cookie_name  = "periods",
          cookie_value = as.character(input$periods)
        )
      }
    )

    ## getting cookies
    observeEvent(
      cookies::get_cookie("location"),
      {
        updateSelectInput(
          inputId = "location",
          selected = cookies::get_cookie("location", "Auckland")
        )
      },
      once = TRUE
    )

    observeEvent(
      cookies::get_cookie("periods"),
      {
        updateSliderInput(inputId = "periods",
                          value = as.integer(cookies::get_cookie("periods", 7)))
      },
      once = TRUE
    )

}

shinyApp(ui = ui,
         server = server,
         options = list(launch.browser = TRUE))
