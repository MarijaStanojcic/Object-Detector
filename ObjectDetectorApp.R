
require(imager)
require(shiny)
require(jpeg)
require(png)
library(magick)
library(tidyverse)
library(grDevices)
library(magick)
library(httr)
library(plotrix)
library(shinyBS)


ui <- fluidPage(
  titlePanel("Object Detector"),
  
  fluidRow(column(width = 3, 
                  wellPanel(fileInput("upload", "Upload new image", accept = c('image/png', 'image/jpeg')), 
                            hr(),
                            textInput("size", "Image size (width x height)", value = "500x500"),
                            p("Aspect ratio from the original picture will be kept the same."), 
                            hr(),
                            sliderInput("threshold", "Probability threshold", min = 0, max = 1, value = 0.7),
                            p("Lowering the threshold includes objects the model is less certain about."),
                            hr(),
                            tags$b("Labels found"),
                            uiOutput("images"))),
           # Probability threshold for including a detected object in the response in the range [0, 1] (default: 0.7). Lowering the threshold includes objects the model is less certain about.
           column(width = 9,
                  imageOutput("img")
           ))
  
  #  fluidRow(column(width = 1),
  #           column(width = 11, 
  #                  imageOutput("img")))
  #  
  
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
    
    row <- data.frame()
    det_box_row <- vector()
    
    n <- length(response_object$predictions)
    
    for (i in 1:n){
      row <- cbind.data.frame(response_object$predictions[[i]]$label_id, response_object$predictions[[i]]$label, response_object$predictions[[i]]$probability)
      my_data <- rbind.data.frame(row, my_data, stringsAsFactors = FALSE) # predictions
      det_box_row <- rbind(t(matrix(unlist(response_object$predictions[[i]]$detection_box), nrow=4)))
      det_box <- rbind(det_box_row, det_box) # coordinates 
    }
    #     
    colnames(my_data) <- c("Label_ID", "Label", "Probability")
    
    
    colnames(det_box) <- c("ymin", "xmin", "ymax", "xmax")
    
    return(list(my_data, det_box))
    
  })
  
  #Getting predictions
  # output$data_pred <- renderTable({
  #    predictions <- data_table()[[1]]
  #    predictions[!duplicated(predictions$Label),1:2]
  #  }) 
  
  # Getting image
  output$img <- renderImage({
    
    req(input$upload)
    req(input$size)
    
    my_data <- data_table()[[1]]
    det_box <- data_table()[[2]] # this is table of cordinates define in data_table()
    
    info <- image_info(image)
    
    w <- info$width
    h <- info$height
    
    {
      moja_slika <- image_draw(image) 
      
      for (i in 1:nrow(det_box)){
        rect(w*det_box$xmin[i], h*det_box$ymin[i], 
             w*det_box$xmax[i], h*det_box$ymax[i], border = "green", lty = "solid", lwd = 5)
        plotrix::textbox(x = c(w*det_box$xmin[i], w*(det_box$xmin[i] + 0.1)), 
                         y = h*(det_box$ymin[i]), 
                         textlist = paste0(my_data$Label[i], " ", round(100*my_data$Probability[i],2), "%"), 
                         justify = 'l', cex = 1.2, 
                         col="Black", border="green", fill="green")
        
      } 
      dev.off()
    }
    #image <- image_read("moja_slika.jpeg")
    
    # Writing an iimage
    tmpfile <- moja_slika %>%
      image_resize(input$size) %>%
      image_write(tempfile(fileext='jpeg'), format = 'jpeg')
    
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })

    
    
  output$images <- renderUI({
    
    labels_id <- unique(data_table()[[1]]$Label_ID) 
    #unique_n <- length(labels_id)
    labels <- unique(data_table()[[1]]$Label)  
    
    for (i in labels_id){
      local({
        my_i <- i
        image_name <- paste("my_image", my_i, sep = "")
        imagename <- image_read(paste0("https://raw.githubusercontent.com/IBM/MAX-Object-Detector-Web-App/master/static/img/cocoicons/", my_i, ".jpg"))
        
        
        output[[image_name]] <- renderImage({
          # Writing an iimage
          tmpfile <- imagename %>%
            image_resize("80x80") %>% 
            image_write(tempfile(fileext='jpg'), format = 'jpg')
          
          
          
          # Return a list
          list(src = tmpfile, contentType = "image/jpg", width = 80, height = 80)  
        })
        
#        bsTooltip(image_name, title ="M")
#        addTooltip(session, image_name, paste0(labels[i]))
        
      })
    }
    
    
    image_output_list <- lapply(labels_id, function(i){
      
      image_name <- paste("my_image", i, sep ="")
      
      imageOutput(image_name, inline = TRUE)

    })
    
    do.call(tagList,image_output_list)
    
  
    
  })
  
  

}

shinyApp(ui, server)