#' Run App
#'
#' Functions to run the package shiny app
#'
#' @import shiny
#' @importFrom magick image_read image_info image_resize image_ggplot image_write_gif
#' @importFrom shinyWidgets panel noUiSliderInput wNumbFormat dropMenu actionBttn
#' @importFrom waiter useWaiter waiter_preloader
#' @importFrom bslib bs_theme
#' @importFrom ggplot2 aes geom_rect
#' @importFrom base64enc dataURI
#'
#' @name app-run
NULL


globalVariables(c("x2", "y2"))

#' @describeIn app-run returns app object for subsequent execution
#' @export
runReplayApp <- function() {
  shiny::shinyApp(
    ui = app_ui(),
    server = app_server(),
    onStart = function() {
      create_session_dir()

      fs::file_copy(
        get_app_dir("images/init.gif"),
        get_temp_dir("temp/init.gif")
      )
      fs::file_copy(
        get_app_dir("images/init.jpeg"),
        get_temp_dir("temp/init.jpeg")
      )
      fs::file_copy(
        get_app_dir("images/init.png"),
        get_temp_dir("temp/init.png")
      )
    }
  )
}


#' @import shinydashboardPlus
#' @importFrom shinydashboard dashboardBody
#'
#' @describeIn app-run UI function for app
app_ui <- function() {


  .colors <- get_app_colors()

  shinydashboardPlus::dashboardPage(
    header = shinydashboardPlus::dashboardHeader(
      controlbarIcon = icon("palette"),
      leftUi = tagList(),
      userOutput("user")
    ),
    sidebar = shinydashboardPlus::dashboardSidebar(disable = TRUE),
    footer = dashboardFooter(
      left = "By Bobby Fatemi",
      right = paste0("Last updated on ", lubridate::today())
    ),
    body = shinydashboard::dashboardBody(

      # use a bit of shinyEffects
      setShadow(class = "dropdown-menu"),
      setShadow(class = "box"),

      # some styling
      get_page_head(),

      waiter::useWaiter(),
      waiter::waiter_preloader(waiter_html("Initializing Session"), .colors$bg),


      # shinydashboardPlus::box(
      #   title = "Artwork Progress",
      #   width = 12,
      #   status = "danger",
      #   headerBorder = FALSE,
        column(
          width = 12,
          shinydashboardPlus::boxPad(
            color = "gray",

            fluidRow(
              column(
                width = 4,
                shinydashboardPlus::descriptionBlock(
                  numberIcon = icon("paintbrush"),
                  header = "423,111",
                  text = "Brush Strokes",
                  rightBorder = FALSE,
                  marginBottom = FALSE
                )
              ),
              column(
                width = 4,
                shinydashboardPlus::descriptionBlock(
                  numberIcon = icon("clock"),
                  header = "246",
                  text = "Hours Spent",
                  rightBorder = FALSE,
                  marginBottom = FALSE
                )
              ),
              column(
                width = 4,
                shinydashboardPlus::descriptionBlock(
                  numberIcon = icon("hourglass-half"),
                  header = "60%",
                  text = "Est. Complete",
                  rightBorder = FALSE,
                  marginBottom = FALSE
                )
              )
            )
          ),
          br()
      ),

      # fluidRow(
      shinydashboardPlus::box(
        title = boxLabel("Signature", status = "primary"),
        label = "Click Image to Select Region",
        collapsible = TRUE,
        collapsed = FALSE,
        headerBorder = FALSE,
        sidebar = shinydashboardPlus::boxSidebar(
          id = "box_sidebar",
          column(
            width = 12,
            shiny::sliderInput(
              inputId = "view_slider",
              pre = "Hour",
              min = 50,
              max = 250,
              value = 250,
              step = 50,
              width = "100%",
              label = "Placeholder Input",
              ticks = FALSE,
              animate = animationOptions(interval = 300, loop = TRUE)
            )
          ),
          column(
            width = 12,
            shiny::sliderInput(
              inputId = "fps_slider",
              post = " fps",
              min = 1,
              max = 50,
              value = 25,
              step = 1,
              width = "100%",
              label = "Timelapse Speed",
              ticks = FALSE
            )
          )
        ),
        width = 12,

        column(
          width = 12,
          shiny::plotOutput(
            outputId = "base_portrait",
            fill = TRUE,
            width = "100%",
            height = "500px",
            click = clickOpts("plot_click", clip = TRUE)
          )
        ),
        footer = tagList(
          shiny::actionButton(
            inputId = "btn_submit",
            label = "Generate Output",
            width = "100%"
          )
        ),
        status = "warning"
      ),

      shinydashboardPlus::box(
        height = "500px",
        id = "box_zoom",
        width = 6,
        icon = icon("magnifying-glass-plus"),
        title = "",
        closable = FALSE,
        solidHeader = FALSE,
        collapsible = FALSE,
        dropdownMenu = shinydashboardPlus::boxDropdown(
          icon = icon("download"),
          shinydashboardPlus::boxDropdownItem(
            shinyWidgets::radioGroupButtons(
              inputId = "dl_zoom",
              individual = TRUE,
              label = "Download Format",
              choices = c("PNG", "JPEG"),
              selected = character(0),
              width = "100%",
              status = "warning"
            )
          )
        ),
        shiny::imageOutput("img_zoom", fill = TRUE, inline = TRUE)
      ),

      shinydashboardPlus::box(
        height = "500px",
        id = "box_replay",
        width = 6,
        icon = icon("video"),
        # label = "Drawing Timelapse",
        title = "",
        closable = FALSE,
        solidHeader = FALSE,
        collapsible = FALSE,
        dropdownMenu = shinydashboardPlus::boxDropdown(
          icon = shiny::icon("download"),
          shinydashboardPlus::boxDropdownItem(
            shinyWidgets::radioGroupButtons(
              inputId = "dl_replay",
              individual = TRUE,
              label = "Download as",
              choices = c("GIF", "MP4"),
              selected = character(0),
              width = "100%",
              status = "warning"
            )
          )
        ),
        shiny::imageOutput("gif_replay", fill = TRUE, inline = TRUE)
      )
    )
  )
}


#' @describeIn app-run server function for app
app_server <- function() {
  function(input, output, session) {

    output$user <- renderUser(get_user_box())

    shiny::onSessionEnded(function() {
      get_temp_dir() |>
        fs::dir_ls(type = "file", recurse = TRUE) |>
        fs::file_delete()
    })

    w <- new_waiter()

    base_plot <- get_base_plot()
    replay_gif <- get_replay_gif() # Make this faster
    full_img <- get_full_img() # Make this faster

    ## Initial coordinates for plot click on app load
    r_click_x <- reactiveVal(300)
    r_click_y <- reactiveVal(425)

    ## On plot click, get x and y coordinates in pixels and shift by 100
    ## to get the start coordinates of the box
    observeEvent(input$plot_click, {
      x <- round(input$plot_click$x * 1000, 0)
      y <- round(input$plot_click$y * 1000, 0)

      r_click_x(max(x - 100, 0))
      r_click_y(max(y - 100, 0))
    })

    ## Plot the base image with the box corresponding to the
    ## click or initial location
    output$base_portrait <- renderPlot({
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
    r_tmp_gif <- reactiveVal()
    r_tmp_jpg <- reactiveVal()
    r_tmp_png <- reactiveVal()

    ## Initialize the reactive vars with the init paths
    appdir <- fs::path_package("replayArt", "app")

    get_temp_dir("init.gif") |> r_tmp_gif()
    get_temp_dir("init.jpeg") |> r_tmp_jpg()
    get_temp_dir("init.png") |> r_tmp_png()
    # fs::path(appdir, "temp/init.gif") |> r_tmp_gif()
    # fs::path(appdir, "temp/init.jpeg") |> r_tmp_jpg()
    # fs::path(appdir, "temp/init.png") |> r_tmp_png()

    observeEvent(input$btn_submit, {
      w$show()
      w$update(html = waiter_html("Generating Replay..."))


      clear_frames_dir() ## Clear previous frames on last submit

      ## Crop based on user click position
      xmin <- r_click_x()
      xmax <- min(r_click_x() + 200, 1000)
      ymin <- r_click_y()
      ymax <- min(r_click_y() + 200, 1000)

      ## Note
      ## Scale the coords because replay and full image are X times
      ## larger than base image user has clicked on
      gif <- image_crop2(replay_gif, xmin * 3, xmax * 3, ymin * 3, ymax * 3)
      img <- image_crop2(full_img,   xmin * 8, xmax * 8, ymin * 8, ymax * 8)

      ## For the GIF:
      ##  1. Drop frames that haven't changed since prior
      ##  2. Set speed and write out
      ##  3. Save path in reactive object
      gif <- gif[which(get_comp_vec(gif))]

      gif |>
        magick::image_write_gif(
          delay = 1 / input$fps_slider,
          path = tempfile(fileext='.gif')
        ) |> r_tmp_gif()


      w$update(html = waiter_html("Saving Timelapse Frames"))

      for (i in 1:length(gif)) {
        w$update(html = waiter_html(
          paste0("Saving Timelapse Frames...",
                 scales::percent(i/length(gif), accuracy = 1))
        ))
        outpath <- fs::path(appdir, "temp/frames", paste0(i, ".jpeg"))
        magick::image_write(gif[i], outpath, format = "jpeg")
      }


      w$update(html = waiter_html("Finalizing Output..."))

      ## For the PNG and JPEG:
      ##  1. Write out as JPEG at highest quality and PNG
      ##  2. Save path in reactive object
      img |>
        magick::image_write(
          path = tempfile(fileext='.jpeg'),
          format = "jpeg",
          quality = 100
        ) |> r_tmp_jpg()
      img |>
        magick::image_write(
          path = tempfile(fileext='.png'),
          format = "png"
        ) |> r_tmp_png()

      w$hide()
    })

    ## Render gif timeseries
    output$gif_replay <- renderImage({
      list(src = r_tmp_gif(), contentType = "image/gif")
    }, deleteFile = FALSE)

    ## Render zoom image
    output$img_zoom <- renderImage({
      list(src = r_tmp_jpg(), contentType = "image/jpeg")
    }, deleteFile = FALSE)

    ## Download Zoomed image as png or jpeg
    observeEvent(input$dl_zoom, {

      w$show()
      w$update(html = waiter_html("Download Image"))

      if (input$dl_zoom == "PNG") {
        tmpfile <- fs::file_copy(r_tmp_png(), tempfile(fileext = ".png"))
        b64 <- base64enc::dataURI(file = tmpfile, mime = "image/png")
        session$sendCustomMessage("download_png", b64)

      } else {
        tmpfile <- fs::file_copy(r_tmp_jpg(), tempfile(fileext = ".jpeg"))
        b64 <- base64enc::dataURI(file = tmpfile, mime = "image/jpeg")
        session$sendCustomMessage("download_jpg", b64)
      }

      w$hide()
    }, ignoreNULL = TRUE)

    ## Download timelapse as video or gif
    observeEvent(input$dl_replay, {
      w$show()
      w$update(html = waiter_html("Download Timelapse"))

      if (input$dl_replay == "GIF") {
        tmpfile <- fs::file_copy(r_tmp_gif(), tempfile(fileext = ".gif"))
        b64 <- base64enc::dataURI(file = tmpfile, mime = "image/gif")
        session$sendCustomMessage("download_gif", b64)

      } else {

        w$update(html = waiter_html("Converting to MP4"))

        tmpfile <- tempfile(fileext = ".mp4")

        r_tmp_gif() |>
          magick::image_read() |>
          magick::image_resize("500") |>
          magick::image_write_video(tmpfile, framerate = input$fps_slider)

        b64 <- base64enc::dataURI(file = tmpfile, mime = "video/mp4")
        session$sendCustomMessage("download_mp4", b64)
      }
      w$hide()
    })
  }
}






