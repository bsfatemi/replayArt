#' App Utils
#'
#' Functions required to execute and facililate an application user session.
#'
#' @import fs
#' @importFrom magick image_read image_ggplot image_crop image_info geometry_area image_compare_dist image_write_gif image_write_video
#' @importFrom data.table data.table
#' @importFrom base64enc dataURI
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

#' @param gif gif image
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

#' @describeIn app-utils get base image for app
get_full_img <- function() {
  get_app_dir("images/base8000.jpeg") |>
    magick::image_read()
}

#' @describeIn app-utils get replay gif for app
get_replay_gif <- function() {
  get_app_dir("images/frames.gif") |>
    magick::image_read()
}

#' @describeIn app-utils get base image plot for app
get_base_plot <- function() {
  get_app_dir("images/base1000.jpeg") |>
    magick::image_read() |>
    magick::image_ggplot()
}

#' @param ... optional
#' @describeIn app-utils get path to internal app files
get_app_dir <- function(...) {
  fs::path_package("replayArt", "app", ...)
}

#' @param ... optional
#' @describeIn app-utils get path to session temp dir
get_temp_dir <- function(...) {
  fs::path(tempdir(), "temp", ...)
}

#' @describeIn app-utils create dirs on session start
create_session_dir <- function() {
  fs::dir_create(get_temp_dir("frames"))
  fs::dir_create(get_temp_dir("graphics"))
}

#' @describeIn app-utils clear generated frames
clear_frames_dir <- function() {
  get_temp_dir("frames") |>
    fs::dir_ls() |>
    fs::file_delete()
}

#' @describeIn app-utils clear generated graphics
clear_graphics_dir <- function() {
  get_temp_dir("graphics") |>
    fs::dir_ls() |>
    fs::file_delete()
}

#' @describeIn app-utils clear all in temp dir
clear_temp_dir <- function() {
  get_temp_dir() |>
    fs::dir_ls(type = "file", recurse = TRUE) |>
    fs::file_delete()
}


#' @param img image object
#' @describeIn app-utils write jpg
write_jpg <- function(img) {
  img |>
    magick::image_write(
      tempfile(fileext='.jpeg'),
      format = "jpeg",
      quality = 100
    )
}

#' @param img image object
#' @describeIn app-utils write png
write_png <- function(img) {
  img |>
    magick::image_write(
      path = tempfile(fileext='.png'),
      format = "png"
    )
}

#' @param gif gif object
#' @describeIn app-utils write gif
write_gif <- function(gif, fps) {
  gif |>
    magick::image_write_gif(
      delay = 1 / fps,
      path = tempfile(fileext='.gif')
    )
}

#' @param gif gif object
#' @param i frame number
#' @describeIn app-utils write frame in gif
write_frame <- function(gif, i) {
  outpath <- get_temp_dir("frames", paste0(i, ".jpeg"))
  magick::image_write(gif[i], outpath, format = "jpeg")
}

#' @param DT data for plot
#' @param i frame number
#' @importFrom ggplot2 ggplot aes geom_point geom_line coord_cartesian ggtitle theme element_blank scale_y_sqrt
#' @importFrom ggpubr ggarrange
#' @importFrom grDevices jpeg dev.off
#' @describeIn app-utils write graph for given frame's data
write_graphic <- function(DT, i) {
  p <- ggplot2::ggplot(data = DT)

  outpath <- get_temp_dir("graphics", paste0(i, ".jpeg"))
  grDevices::jpeg(outpath, width = 858, height = 1200, res = 175)

  graph1 <- p +
    ggplot2::geom_point(data = DT[1:(i+1)], ggplot2::aes(x, strokes), color = "red") +
    ggplot2::geom_line(data = DT[1:(i+1)], ggplot2::aes(x, strokes), color = "red") +
    ggplot2::coord_cartesian(
      xlim = c(0, nrow(DT)),
      ylim = c(min(DT$strokes), max(DT$strokes))
    ) +
    ggplot2::ggtitle(label = "Strokes") +
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_sqrt()
  graph2 <- p +
    ggplot2::geom_point(data = DT[1:(i+1)], ggplot2::aes(x, hours), color = "blue") +
    ggplot2::geom_line(data = DT[1:(i+1)], ggplot2::aes(x, hours), color = "blue") +
    ggplot2::coord_cartesian(
      xlim = c(0, nrow(DT)),
      ylim = c(min(DT$hours), max(DT$hours))
    ) +
    ggplot2::ggtitle(label = "Hours") +
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
  graph <- ggpubr::ggarrange(
    graph1, graph2,
    ncol = 1,
    align = "hv"
  ) +
    ggplot2::theme(plot.margin = ggplot2::margin(0,0,0,0, "cm"))
  print(graph)

  grDevices::dev.off()
  invisible(NULL)
}


#' @param img_path image path
#' @param session shiny session
#' @describeIn app-utils trigger download
trigger_image_dl <- function(img_path, session) {
  ext <- fs::path_ext(img_path)
  tmpfile <- fs::file_copy(
    img_path,
    tempfile(fileext = paste0(".", ext))
  )
  b64 <- base64enc::dataURI(file = tmpfile, mime = paste0("image/", ext))
  session$sendCustomMessage(paste0("download_", ext), b64)
}

#' @param gif_path gif path
#' @param session shiny session
#' @describeIn app-utils trigger download
trigger_gif_dl <- function(gif_path, session) {
  tmpfile <- fs::file_copy(gif_path, tempfile(fileext = ".gif"))
  b64 <- base64enc::dataURI(file = tmpfile, mime = "image/gif")
  session$sendCustomMessage("download_gif", b64)
}

#' @param gif_path gif path
#' @param fps frames per second
#' @param session shiny session
#' @describeIn app-utils trigger download
trigger_vid_dl <- function(gif_path, fps, session) {
  tmpfile <- tempfile(fileext = ".mp4")

  magick::image_read(gif_path) |>
    magick::image_resize("500") |>
    magick::image_write_video(tmpfile, framerate = fps)

  b64 <- base64enc::dataURI(file = tmpfile, mime = "video/mp4")
  session$sendCustomMessage("download_mp4", b64)
}

#' @param base_plot base plot
#' @param x position of click
#' @param y position of click
#' @importFrom ggplot2 aes geom_rect
#' @describeIn app-utils get plot based on click position
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
