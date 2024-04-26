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
        get_temp_dir("init.gif")
      )
      fs::file_copy(
        get_app_dir("images/init.jpeg"),
        get_temp_dir("init.jpeg")
      )
      fs::file_copy(
        get_app_dir("images/init.png"),
        get_temp_dir("init.png")
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
      waiter::waiter_preloader(
        waiter_html("Initializing Session"),
        .colors$bg,
        fadeout = 2000
      ),

      get_progress_stats(),
      get_input_box(),
      get_zoom_box(),
      get_replay_box(),
      get_frames_box()
    )
  )
}


#' @describeIn app-run server function for app
app_server <- function() {
  function(input, output, session) {

    updateBox("box_zoom_viewer", action = "remove")
    updateBox("box_replay_viewer", action = "remove")
    updateBox("box_frame_viewer", action = "remove")

    output$user <- renderUser(get_user_box())

    shiny::onSessionEnded(fun = clear_temp_dir)

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
      r_click_x(max(round(input$plot_click$x * 1000, 0) - 100, 0))
      r_click_y(max(round(input$plot_click$y * 1000, 0) - 100, 0))
    })

    ## Plot the base image with the box corresponding to the
    ## click or initial location
    output$base_portrait <- renderPlot({
      get_click_plot(base_plot, r_click_x(), r_click_y())
    })

    ## On submit, get the replay gif of the selected region in the base image
    ##
    r_tmp_gif <- reactiveVal()
    r_tmp_jpg <- reactiveVal()
    r_tmp_png <- reactiveVal()

    observeEvent(input$btn_submit, {
      w$show()
      w$update(html = waiter_html("Generating Replay"))

      clear_frames_dir() ## Clear previous frames on last submit
      clear_graphics_dir() ## Clear previous graphics on last submit

      ## Get and adjust click position so that click is in the middle of region
      xmin <- r_click_x()
      xmax <- min(r_click_x() + 200, 1000)
      ymin <- r_click_y()
      ymax <- min(r_click_y() + 200, 1000)

      ## Scale coords to map selected region to res of files and crop
      gif <- image_crop2(replay_gif, xmin * 3, xmax * 3, ymin * 3, ymax * 3)
      img <- image_crop2(full_img,   xmin * 8, xmax * 8, ymin * 8, ymax * 8)

      ## Drop frames that haven't changed since prior
      gif <- gif[which(get_comp_vec(gif))]

      ## Write and save path to reactive object
      write_gif(gif, input$fps_slider) |> r_tmp_gif()

      ## Temporary data, replace with real info later
      DT <- data.table(x = 0:100)
      DT[, strokes := x*100]
      DT[, hours := (x + 5)*(x > 0)]

      w$update(html = waiter_html("Saving Timelapse Frames"))

      for (i in 1:length(gif)) {
        if (i %% 5 == 0) {
          # Update Progress Screen
          w$update(html = waiter_html(
            paste0("Saving Timelapse Frames...",
                   scales::percent(i/length(gif), accuracy = 1))
          ))
        }
        write_frame(gif, i) # Save Frame
        write_graphic(DT, i) # Save Graphic
      }

      updateSliderInput(
        inputId = "frame_slider",
        label = NULL,
        value = 1,
        min = 1,
        max = length(gif),
        step = 1
      )

      w$update(html = waiter_html("Finalizing Output"))

      ## Write out and save paths in reactive obj
      r_tmp_jpg(write_jpg(img))
      r_tmp_png(write_png(img))

      w$hide()
    })

    observe({
      req(r_tmp_gif(), r_tmp_jpg(), r_tmp_png())
      updateBox("box_frame_viewer", action = "restore")
      updateBox("box_frame_viewer", action = "update", options = list(closable = FALSE))
      updateBox("box_replay_viewer", action = "restore")
      updateBox("box_replay_viewer", action = "update", options = list(closable = FALSE))
      updateBox("box_zoom_viewer", action = "restore")
      updateBox("box_zoom_viewer", action = "update", options = list(closable = FALSE))
    })

    ## Render image and gif
    output$gif_replay <- renderImage({
      list(src = req(r_tmp_gif()), contentType = "image/gif")
    }, deleteFile = FALSE)
    output$img_zoom <- renderImage({
      list(src = req(r_tmp_jpg()), contentType = "image/jpeg")
    }, deleteFile = FALSE)

    ## Download Zoomed image as png or jpeg
    observeEvent(input$dl_zoom, {
      w$show()
      w$update(html = waiter_html("Download Image"))
      if (input$dl_zoom == "PNG") {
        trigger_image_dl(req(r_tmp_png()), session)
      } else {
        trigger_image_dl(req(r_tmp_jpg()), session)
      }
      w$hide()
    }, ignoreNULL = TRUE)

    ## Download timelapse as video or gif
    observeEvent(input$dl_replay, {
      w$show()
      w$update(html = waiter_html("Download Timelapse"))
      if (input$dl_replay == "GIF") {
        trigger_gif_dl(req(r_tmp_gif()), session)
      } else {
        w$update(html = waiter_html("Converting to MP4"))
        trigger_vid_dl(req(r_tmp_gif()), input$fps_slider, session)
      }
      w$hide()
    })

    output$frame_portrait <- renderImage({
      list(
        src = get_temp_dir("frames", paste0(input$frame_slider, ".jpeg")),
        width = "100%", height = "100%"
      )
    }, deleteFile = FALSE)

    output$frame_graphic <- renderImage({
      list(
        src = get_temp_dir("graphics", paste0(input$frame_slider, ".jpeg")),
        width = "100%", height = "100%"
      )
    }, deleteFile = FALSE)
  }
}






