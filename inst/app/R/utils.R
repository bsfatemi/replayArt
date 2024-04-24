image_crop2 <- function(im, xmin, xmax, ymin, ymax) {
  ratio <- 1 # gif size relative to base image
  new_width <- (xmax -  xmin) * ratio
  new_height <- (ymax -  ymin) * ratio
  adj_y <- (image_info(im)$height - ymax) * ratio
  image_crop(im, geometry_area(new_width, new_height, xmin, adj_y), repage = TRUE)
}


get_comp_vec <- function(gif) {
  frames <- seq_along(gif)
  MAT <- data.table(
    a = frames[-length(frames)],
    b = frames[-1]
  )
  apply(MAT, 1, function(x) {
    image_compare_dist(
      gif[x[1]],
      gif[x[2]],
      metric = "RMSE"
    )$distortion > .001
  })
}

get_base_plot <- function() {
  fs::path("images", "base1000.png") |>
    magick::image_read() |>
    magick::image_ggplot()
}


get_replay_gif <- function() {
  fs::path("images", "frames.gif") |>
    magick::image_read()
}


