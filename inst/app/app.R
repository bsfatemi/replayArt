library(shiny)
library(magick)
library(fs)
library(data.table)
library(ggplot2)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel = NULL,
        mainPanel = mainPanel(
            plotOutput(
                outputId = "plot_portrait",
                width = "100%",
                height = "500px",
                click = "plot_click"
            ),
            actionButton(
                inputId = "btn_submit",
                label = "Go",
                width = "100%"
            ),
            imageOutput(
                outputId = "gif_replay",
                width = "100%",
                height = "500px",
                dblclick = "replay_dbclick"
            )
        )
    )
)


server <- function(input, output) {

    base_plot <- get_base_plot()
    replay_gif <- get_replay_gif()

    ## Initial coordinates for plot click on app load
    r_click_x <- reactiveVal(300)
    r_click_y <- reactiveVal(425)

    ## On plot click, get x and y coordinates in pixels and shift by 100
    ## to get the start coordinates of the box
    observeEvent(input$plot_click, {
        r_click_x(max(ceiling(input$plot_click$x * 1000) - 100, 0))
        r_click_y(max(ceiling(input$plot_click$y * 1000) - 100, 0))
    })

    ## Plot the base image with the box corresponding to the
    ## click or initial location
    output$plot_portrait <- renderPlot({
        dt <- data.table(
            x = as.numeric(r_click_x()),
            x2 = as.numeric(r_click_x()) + 200,
            y = as.numeric(r_click_y()),
            y2 = as.numeric(r_click_y()) + 200
        )
        p <- base_plot +
            geom_rect(
                aes(xmin = x, xmax = x2, ymin = y, ymax = y2),
                fill = NA,
                color = "black",
                data = dt
            )
        print(p)
    })

    ## On submit, get the replay gif of the selected region in the base image
    ##
    r_gif_replay <- eventReactive(input$btn_submit, {

        ## Scale the coords because the replay is Ratiox larger than base image
        ratio <- 3
        x <- r_click_x()
        y <- r_click_y()
        xmin <- x * ratio
        xmax <- min(xmin + 200*ratio, 1000*ratio)
        ymin <- y * ratio
        ymax <- min(ymin + 200*ratio, 1000*ratio)

        ## Read and crop Gif based on user click position
        gif <- image_crop2(img_gif, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

        ## Drop frames that haven't changed since prior and write out
        tmpfile <- gif[which(get_comp_vec(gif))] |>
            magick::image_write_gif(tempfile(tmpdir = "temp", fileext='.gif'))

        ## Return path for shiny app to display
        list(src = tmpfile, contentType = "image/gif")
    })

    ## On app init before the submit button is pressed, load a presaved
    ## replay corresponding to the init position of the box on the base image
    output$gif_replay <- renderImage({
        if (input$btn_submit == 0) {
            fs::file_copy("images/init.gif", "temp/init.gif")
            list(src = "temp/init.gif", contentType = "image/gif")
        } else {
            r_gif_replay()
        }
    }, deleteFile = TRUE)
}

shinyApp(ui = ui, server = server)
