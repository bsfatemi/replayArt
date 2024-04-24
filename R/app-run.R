#' Run App
#'
#' Functions to run the package shiny app
#'
#' @import shiny
#' @importFrom magick image_read image_info image_resize image_ggplot image_write_gif
#' @importFrom rdstools log_inf
#' @importFrom shinyWidgets panel noUiSliderInput wNumbFormat dropMenu actionBttn
#' @importFrom waiter useWaiter
#' @importFrom bslib bs_theme
#' @importFrom shinycssloaders withSpinner
#' @importFrom ggtext geom_textbox
#' @importFrom ggplot2 aes geom_rect
#'
#' @name app-run
NULL


#' @describeIn app-run returns app object for subsequent execution
#' @export
runReplayApp <- function() {
  shiny::shinyApp(
    ui = app_ui(),
    server = app_server()
  )
}

#' @describeIn app-run UI function for app
app_ui <- function() {
  .colors <- get_app_colors()
  shinydashboardPlus::dashboardPage(
    header = dashboardHeader(disable = TRUE),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      waiter::useWaiter(),

      shinydashboardPlus::box(
        width = 12,
        title = dashboardLabel(
          "Click on Image to Select Region",
          status = "warning"
        ),
        collapsible = FALSE,
        closable = FALSE,
        headerBorder = TRUE,
        status = "warning",
        shiny::plotOutput(
          outputId = "plot_portrait",
          width = "auto",
          height = "400px",
          click = "plot_click"
        ),
        footer = tagList(
          shiny::sliderInput(
            inputId = "fps_slider",
            post = " fps",
            min = 1,
            max = 50,
            value = 25,
            step = 1,
            width = "100%",
            label = NULL,
            ticks = FALSE
          ),
          shiny::actionButton(
            inputId = "btn_submit",
            label = "Generate Replay",
            width = "100%"
          )
        )
      ),
      # shiny::br(),

      shinydashboardPlus::box(
        id = "box_replay",
        width = 12,
        title = "Replay",
        closable = TRUE,
        status = "warning",
        solidHeader = FALSE,
        collapsible = FALSE,
        dropdownMenu = shinydashboardPlus::boxDropdown(
          shinydashboardPlus::boxDropdownItem(
            id = "btn_download",
            icon = icon("download")
          )
        ),
        # shinycssloaders::withSpinner(
          shiny::imageOutput(
            outputId = "gif_replay",
            width = "auto",
            height = "auto",
            fill = TRUE
          )
        # )
      )
    )
  )
}


#' @describeIn app-run server function for app
app_server <- function() {
  function(input, output, session) {
    w <- new_waiter()

    base_plot <- get_base_plot()
    replay_gif <- get_replay_gif()

    ## Initial coordinates for plot click on app load
    r_click_x <- reactiveVal(300)
    r_click_y <- reactiveVal(425)

    ## On plot click, get x and y coordinates in pixels and shift by 100
    ## to get the start coordinates of the box
    observeEvent(input$plot_click, {
      r_click_x(max(ceiling(input$plot_click$x * 1000) - 100, 0))
      r_click_y(max(ceiling(input$plot_click$y * 1000) - 100, 0))
    })

    ## Plot the base image with the box corresponding to the
    ## click or initial location
    output$plot_portrait <- renderPlot({
      dt <- data.table(
        x = as.numeric(r_click_x()),
        x2 = as.numeric(r_click_x()) + 200,
        y = as.numeric(r_click_y()),
        y2 = as.numeric(r_click_y()) + 200
      )
      p <- base_plot +
        ggplot2::geom_rect(
          ggplot2::aes(xmin = x, xmax = x2, ymin = y, ymax = y2),
          fill = NA,
          color = "black",
          data = dt
        )
      print(p)
    })

    ## On submit, get the replay gif of the selected region in the base image
    ##
    r_tmpfile <- eventReactive(input$btn_submit, {
      w$show()
      w$update(html = "Generating Replay")

      shinydashboardPlus::updateBox(
        id = "box_replay",
        action = "restore"
      )

      ## Scale the coords because the replay is Ratiox larger than base image
      ratio <- 3
      x <- r_click_x()
      y <- r_click_y()
      xmin <- x * ratio
      xmax <- min(xmin + 200*ratio, 1000*ratio)
      ymin <- y * ratio
      ymax <- min(ymin + 200*ratio, 1000*ratio)

      ## Read and crop Gif based on user click position
      gif <- image_crop2(
        replay_gif,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      )

      ## Drop frames that haven't changed since prior, set speed and save
      tmpfile <- gif[which(get_comp_vec(gif))] |>
        magick::image_write_gif(
          delay = 1 / input$fps_slider,
          path = tempfile(fileext='.gif')
        )

      w$hide()
      tmpfile
    })

    ## On app init before the submit button is pressed, load a presaved
    ## replay corresponding to the init position of the box on the base image
    output$gif_replay <- renderImage({
      if (input$btn_submit == 0) {
        appdir <- fs::path_package("replayArt") |>
          fs::path("app")

        src_path <- fs::file_copy(
          fs::path(appdir, "images/init.gif"),
          fs::path(appdir, "temp/init.gif")
        )
        list(src = src_path, contentType = "image/gif")
      } else {
        list(src = r_tmpfile(), contentType = "image/gif")
      }
    }, deleteFile = TRUE)

  }
}






