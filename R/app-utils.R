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
  get_app_dir("images/base8000.jpeg") |>
    magick::image_read()
}

#' @importFrom magick image_read
#' @importFrom fs path path_package
#'
#' @describeIn app-utils get replay gif for app
get_replay_gif <- function() {
  get_app_dir("images/frames.gif") |>
    magick::image_read()
}

#' @importFrom magick image_read image_ggplot
#' @importFrom fs path path_package
#'
#' @describeIn app-utils get base image plot for app
get_base_plot <- function() {
  get_app_dir("images/base1000.jpeg") |>
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
  fs::dir_create(get_temp_dir("frames"))
  fs::dir_create(get_temp_dir("graphics"))
}

clear_frames_dir <- function() {
  get_temp_dir("frames") |>
    fs::dir_ls() |>
    fs::file_delete()
}

clear_temp_dir <- function() {
  get_temp_dir() |>
    fs::dir_ls(type = "file", recurse = TRUE) |>
    fs::file_delete()
}

clear_graphics_dir <- function() {
  get_temp_dir("graphics") |>
    fs::dir_ls() |>
    fs::file_delete()
}

write_jpg <- function(img) {
  img |>
    magick::image_write(
      tempfile(fileext='.jpeg'),
      format = "jpeg",
      quality = 100
    )
}

write_png <- function(img) {
  img |>
    magick::image_write(
      path = tempfile(fileext='.png'),
      format = "png"
    )
}

write_gif <- function(gif, fps) {
  gif |>
    magick::image_write_gif(
      delay = 1 / fps,
      path = tempfile(fileext='.gif')
    )
}

write_frame <- function(gif, i) {
  outpath <- get_temp_dir("frames", paste0(i, ".jpeg"))
  magick::image_write(gif[i], outpath, format = "jpeg")
}

write_graphic <- function(DT, i) {
  p <- ggplot(data = DT)

  outpath <- get_temp_dir("graphics", paste0(i, ".jpeg"))
  jpeg(outpath, width = 858, height = 1200, res = 175)

  graph1 <- p +
    geom_point(data = DT[1:(i+1)], aes(x, strokes), color = "red") +
    geom_line(data = DT[1:(i+1)], aes(x, strokes), color = "red") +
    coord_cartesian(
      xlim = c(0, nrow(DT)),
      ylim = c(min(DT$strokes), max(DT$strokes))
    ) +
    ggtitle(label = "Strokes") +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_blank()
    ) +
    scale_y_sqrt()
  graph2 <- p +
    geom_point(data = DT[1:(i+1)], aes(x, hours), color = "blue") +
    geom_line(data = DT[1:(i+1)], aes(x, hours), color = "blue") +
    coord_cartesian(
      xlim = c(0, nrow(DT)),
      ylim = c(min(DT$hours), max(DT$hours))
    ) +
    ggtitle(label = "Hours") +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_blank()
    )
  graph <- ggpubr::ggarrange(
    graph1, graph2,
    ncol = 1,
    align = "hv"
  ) +
    theme(plot.margin = margin(0,0,0,0, "cm"))
  print(graph)

  dev.off()
  invisible(NULL)
}

trigger_image_dl <- function(img_path, session) {
  ext <- fs::path_ext(img_path)
  tmpfile <- fs::file_copy(
    img_path,
    tempfile(fileext = paste0(".", ext))
  )
  b64 <- base64enc::dataURI(file = tmpfile, mime = paste0("image/", ext))
  session$sendCustomMessage(paste0("download_", ext), b64)
}

trigger_gif_dl <- function(gif_path, session) {
  tmpfile <- fs::file_copy(gif_path, tempfile(fileext = ".gif"))
  b64 <- base64enc::dataURI(file = tmpfile, mime = "image/gif")
  session$sendCustomMessage("download_gif", b64)
}

trigger_vid_dl <- function(gif_path, fps, session) {
  tmpfile <- tempfile(fileext = ".mp4")

  magick::image_read(gif_path) |>
    magick::image_resize("500") |>
    magick::image_write_video(tmpfile, framerate = fps)

  b64 <- base64enc::dataURI(file = tmpfile, mime = "video/mp4")
  session$sendCustomMessage("download_mp4", b64)
}

get_click_plot <- function(base_plot, x, y) {
  base_plot +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = x, xmax = x2, ymin = y, ymax = y2),
      fill = NA,
      color = "black",
      data = data.table(
        x = as.numeric(x),
        x2 = as.numeric(x) + 200,
        y = as.numeric(y),
        y2 = as.numeric(y) + 200
      )
    )
}
