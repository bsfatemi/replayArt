#' Get Zoom Box
#'
get_zoom_box <- function() {
  shinydashboardPlus::box(
    height = "500px",
    id = "box_zoom_viewer",
    width = 6,
    icon = icon("magnifying-glass-plus"),
    title = "",
    closable = TRUE,
    solidHeader = FALSE,
    collapsible = FALSE,
    headerBorder = FALSE,
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
    imageOutput("img_zoom", fill = TRUE, inline = TRUE)
  )
}
