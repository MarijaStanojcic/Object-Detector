
require(imager)
require(shiny)
require(jpeg)
require(png)
library(magick)
library(tidyverse)
library(grDevices)
library(magick)
library(httr)



ui <- fluidPage(
  titlePanel("Object Detector"),
  
  fluidRow(column(width = 4, 
                  wellPanel(fileInput("upload", "Upload new image", accept = c('image/png', 'image/jpeg')),
                            textInput("size", "Size", value = "500x500"),
                            sliderInput("threshold", "Probability threshold:", min = 0, max = 1, value = 0.7))),
           # Probability threshold for including a detected object in the response in the range [0, 1] (default: 0.7). Lowering the threshold includes objects the model is less certain about.
          column(width = 5, 
                  tableOutput("data_pred"))),
  
  fluidRow(imageOutput("img"))

  
)

server <- function(input, output, session) {
  

  # Start with placeholder image
  image <- image_read("https://raw.githubusercontent.com/IBM/MAX-Object-Detector/master/assets/jockey.jpg")

  
  
  prob_threshold <- reactive({
    prob <- input$threshold
    return(prob)
  })
  
 # thresh <- prob_threshold()
  
  
  model_endpoint_object_detector <- reactive({
    paste0('http://max-object-detector.max.us-south.containers.appdomain.cloud/model/predict?threshold=', input$threshold)
  })
  
  observeEvent(input$upload, {
    if (length(input$upload$datapath))
      image <<- image_read(input$upload$datapath)
    info <- image_info(image)
    updateTextInput(session, "size", value = paste(info$width, info$height, sep = "x"))
    
  })
  

  
      
  my_data <- data.frame()
  det_box <- data.frame()
  
  n <- 0
  
  data_table <- reactive({
    
    if (is.null(input$upload)) return(NULL)

    response_object <- httr::POST(model_endpoint_object_detector(),
                                  body = list(image = upload_file(paste0(input$upload$datapath)), type = "image/jpeg"), 
                                  encode = c("multipart")
    ) %>% content()
    
    row <- character()
    det_box_row <- vector()
    
    n <- length(response_object$predictions)
    
    for (i in 1:n){
      row <- cbind(response_object$predictions[[i]]$label_id, response_object$predictions[[i]]$label, response_object$predictions[[i]]$probability)
      my_data <- rbind(row, my_data) # predictions
      det_box_row <- rbind(t(matrix(unlist(response_object$predictions[[i]]$detection_box), nrow=4)))
      det_box <- rbind(det_box_row, det_box) # coordinates 
    }
    #     
    colnames(my_data) <- c("LabelID", "Label", "Probability")
    
    
    colnames(det_box) <- c("ymin", "xmin", "ymax", "xmax")
    
    return(list(my_data, det_box))
    
  })
  
  #Getting predictions
  output$data_pred <- renderTable({
    predictions <- data_table()[[1]]
    predictions
  }) 
  
  # Getting image
  output$img <- renderImage({
    
    req(input$upload)
    req(input$size)
    
    det_box <- data_table()[[2]] # this is table of cordinates define in data_table()
    
    info <- image_info(image)
    
    grDevices::jpeg("moja_slika.jpeg", width = info$width, height = info$height) +
    plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE) +
    rasterImage(image,0,0,1,1) +
    for (i in 1:nrow(det_box)){
      rect(det_box$xmin[i], 1- det_box$ymax[i], det_box$xmax[i], 1- det_box$ymin[i], border = "green", lty = "solid", lwd = 3)} 
    dev.off()
    
    image <- image_read("moja_slika.jpeg")
    
    # Writing an iimage
    tmpfile <- image %>%
      image_resize(input$size) %>%
      image_write(tempfile(fileext='jpeg'), format = 'jpeg')
    
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })
  
  
}

shinyApp(ui, server)