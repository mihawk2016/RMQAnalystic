library(shinyjs)

runApp(shinyApp(
  ui = fluidPage(
    # need to make a call to useShinyjs() in order to use its functions in server
    shinyjs::useShinyjs(),  
    actionButton("start_proc", "Click to start processing data"),
    downloadButton("data_file")
  ),
  server = function(input, output) {
    observe({
      if (input$start_proc > 0) {
        Sys.sleep(1)
        # enable the download button
        shinyjs::enable("data_file")
        # change the text of the download button
        shinyjs::html("data_file",
                      sprintf("<i class='fa fa-download'></i>
                              Download (file size: %s)",
                              round(runif(1, 1, 10000))
                      )
        )
      }
    })
    
    output$data_file <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        write.csv(data.frame(x=runif(5), y=rnorm(5)), file)
      }
    )
    
    # disable the downdload button on page load
    shinyjs::disable("data_file")
  }
))