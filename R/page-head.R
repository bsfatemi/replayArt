get_page_head <- function() {
  js_jpg <- '
Shiny.addCustomMessageHandler("download_jpg", function(b64){
      const a = document.createElement("a");
      document.body.append(a);
      a.download = "steph-artwork-zoom.jpeg";
      a.href = b64;
      a.click();
      a.remove();
    })
'

  js_png <- '
Shiny.addCustomMessageHandler("download_png", function(b64){
      const a = document.createElement("a");
      document.body.append(a);
      a.download = "steph-artwork-zoom.png";
      a.href = b64;
      a.click();
      a.remove();
    })
'

  js_gif <- '
Shiny.addCustomMessageHandler("download_gif", function(b64){
      const a = document.createElement("a");
      document.body.append(a);
      a.download = "steph-artwork-timelapse.gif";
      a.href = b64;
      a.click();
      a.remove();
    })
'

  js_mp4 <- '
Shiny.addCustomMessageHandler("download_mp4", function(b64){
      const a = document.createElement("a");
      document.body.append(a);
      a.download = "steph-artwork-timelapse.mp4";
      a.href = b64;
      a.click();
      a.remove();
    })
'

  tags$head(
    tags$style(
      rel = "stylesheet",
      type = "text/css",
      href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/qtcreator_dark.min.css"
    ),
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
    ),
    tags$script(
      "$(function() {
            $('.sidebar-toggle').on('click', function() {
              $('.skinSelector-widget').toggle();
            });
          });
          "
    ),
    tags$style(
      type="text/css",
      "#gif_replay img {max-width: 100%; width: 100%; height: auto}",
      "#img_zoom img {max-width: 100%; width: 100%; height: auto}"
    ),
    tags$script(HTML(js_png)),
    tags$script(HTML(js_jpg)),
    tags$script(HTML(js_gif)),
    tags$script(HTML(js_mp4))
  )
}
