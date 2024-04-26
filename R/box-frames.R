#' Get Frames Box
#'
get_frames_box <- function() {
  shinydashboardPlus::box(
    id = "box_frame_viewer",
    title = "",
    width = 12,
    icon = icon("chart-line"),
    closable = TRUE,
    collapsible = TRUE,
    headerBorder = FALSE,

    column(
      width = 12,
      sliderInput(
        width = "100%",
        inputId = "frame_slider",
        min = 0,
        max = 10,
        value = 0,
        label = NULL,
        animate = animationOptions(interval = 300, loop = TRUE),
        ticks = FALSE
      )
    ),

    footer = tagList(
      column(5, imageOutput("frame_graphic", fill = TRUE, inline = TRUE)),
      column(7, imageOutput("frame_portrait", fill = TRUE, inline = TRUE))
    )
  )
}
