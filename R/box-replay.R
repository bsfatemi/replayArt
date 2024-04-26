#' Get Replay Box
#'
#' @importFrom shinyWidgets radioGroupButtons
#'
get_replay_box <- function() {
  shinydashboardPlus::box(
    height = "500px",
    id = "box_replay_viewer",
    width = 6,
    icon = icon("video"),
    headerBorder = FALSE,
    title = "",
    closable = TRUE,
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
}
