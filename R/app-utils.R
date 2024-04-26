#' App Utils
#'
#' Functions required to execute and facililate an application user session.
#'
#' @import fs
#'
#' @name app-utils
NULL


#' @param session shiny session object
#'
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom waiter Waiter
#'
#' @describeIn app-utils create a new waiter object
new_waiter <- function(session = NULL) {
  if (is.null(session))
    shiny::getDefaultReactiveDomain()
  waiter::Waiter$new(
    html = waiter_html("Initializing..."),
    color = get_app_colors()$bg
  )
}

#' @param msg message for waiter screen
#'
#' @importFrom shiny tagList br
#' @importFrom waiter spin_pulsar
#'
#' @describeIn app-utils get html for waiter progress page
waiter_html <- function(msg) {
  shiny::tagList(waiter::spin_pulsar(), shiny::br(), msg)
}

#' @describeIn app-utils returns TRUE if called on CI
is_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI", "false")))
}


#' @describeIn app-utils returns TRUE if called while testing
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}


#' @describeIn app-utils Set Plot colors
get_app_colors <- function() {
  list(
    bg = "#06325f",
    fg = "#E0ECF9",
    primary    = "#187dd4",
    secondary  = "#ED9100",
    success    = "#00A651",
    info       = "#fff573",
    warning    = "#7d3be8",
    danger     = "#DB14BF"
  )
}



#' @importFrom magick image_crop image_info geometry_area
#'
#' @param im image
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#'
#' @describeIn app-utils crop image based on plot brush
image_crop2 <- function(im, xmin, xmax, ymin, ymax) {
  new_width <- xmax -  xmin
  new_height <- ymax -  ymin
  adj_y <- magick::image_info(im)$height - ymax
  magick::image_crop(
    image = im,
    geometry = magick::geometry_area(new_width, new_height, xmin, adj_y),
    repage = TRUE
  )
}

#' @importFrom data.table data.table
#' @importFrom magick image_compare_dist
#'
#' @param gif gif image
#'
#' @describeIn app-utils function to get frames of a gif only if there are changes from prior frame
get_comp_vec <- function(gif) {
  frames <- seq_along(gif)
  MAT <- data.table(
    a = frames[-length(frames)],
    b = frames[-1]
  )
  apply(MAT, 1, function(x) {
    magick::image_compare_dist(
      gif[x[1]],
      gif[x[2]],
      metric = "RMSE"
    )$distortion > .001
  })
}


#' @importFrom magick image_read
#' @importFrom fs path path_package
#'
#' @describeIn app-utils get base image for app
get_full_img <- function() {
  fs::path_package("replayArt") |>
    fs::path("app/images/base8k.png") |>
    magick::image_read() |>
    magick::image_resize("8000")
}

#' @importFrom magick image_read
#' @importFrom fs path path_package
#'
#' @describeIn app-utils get replay gif for app
get_replay_gif <- function() {
  fs::path_package("replayArt") |>
    fs::path("app/images/frames.gif") |>
    magick::image_read()
}

#' @importFrom magick image_read image_ggplot
#' @importFrom fs path path_package
#'
#' @describeIn app-utils get base image plot for app
get_base_plot <- function() {
  fs::path_package("replayArt") |>
    fs::path("app/images/base1000.png") |>
    magick::image_read() |>
    magick::image_ggplot()
}


get_app_dir <- function(...) {
  fs::path_package("replayArt", "app", ...)
}
get_temp_dir <- function(...) {
  fs::path(tempdir(), "temp", ...)
}

create_session_dir <- function() {
  fs::dir_create(get_temp_dir("temp",  "frames"))
}

clear_frames_dir <- function() {
  get_temp_dir("frames") |>
    fs::dir_ls() |>
    fs::file_delete()
}
