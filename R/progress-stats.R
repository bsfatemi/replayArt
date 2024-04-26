#' Get Progress Stats
#'
get_progress_stats <- function() {
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
  )
}
