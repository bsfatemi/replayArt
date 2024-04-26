#' Get User Box
#'
get_user_box <- function() {
  dashboardUser(
    name = "Bobby Fatemi",
    image = "https://avatars.githubusercontent.com/u/15960931?v=4",
    title = "2024",
    subtitle = "Artist, Data Scientist, Developer",
    footer = tagList(
      p("Drawing Portraits Since 2019", class = "text-center"),

      column(
        12,
      fluidRow(
        socialButton(
          href = "",
          icon = icon("tiktok")
        ),
        socialButton(
          href = "",
          icon = icon("instagram")
        ),
        socialButton(
          href = "",
          icon = icon("youtube")
        ),
        socialButton(
          href = "",
          icon = icon("github")
        ),
        socialButton(
          href = "",
          icon = icon("linkedin")
        )
      )
      )
    ),
    fluidRow(
      dashboardUserItem(
        width = 6,
        descriptionBlock(
          number = "3M",
          numberColor = "green",
          numberIcon = icon("caret-up"),
          header = "Total Brush Strokes",
          rightBorder = TRUE,
          marginBottom = FALSE
        )
      ),
      dashboardUserItem(
        width = 6,
        descriptionBlock(
          number = "10K",
          numberColor = "green",
          numberIcon = icon("caret-up"),
          header = "Total Hours Drawing",
          rightBorder = FALSE,
          marginBottom = FALSE
        )
      )
    )
  )
}
