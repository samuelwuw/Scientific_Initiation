library(shiny)

shinyServer(function(input,output){
  
  output$image <- renderPlot({
    library(kohonen)
    require(kohonen)
    
    df <- read.csv('uscitiesCsv.csv', header = TRUE, sep = ",")
    
    # DATA FRAME de Delaware com 2 Vari?veis (latitude e longitude), 77 exemplares
    df_del <- df[621:697, c(9,10)] 
    
    # Normalizacao dos Dados
    data_train_matrix <- as.matrix(scale(df_del)) 
    
    som_grid <- somgrid(xdim = 4, ydim = 4, topo="hexagonal") # SOM 6x6, hexagonal
    
    
    som_model <- som(data_train_matrix, 
                     grid=som_grid,  
                     rlen=100, 
                     alpha=c(0.05,0.01), 
                     keep.data = TRUE,
                     radius = 5)
    
    #centroides de cada estado (16)
    centroides <- as.data.frame(som_model$codes)
    
    library(ggplot2)
    require(ggplot2)
    grid_size <- 2.3
    
    #Xn?mero de clientes 
    n <- 77
    customer_locations <- data.frame(
      id = 1:n,
      x = data_train_matrix[,1],
      y = data_train_matrix[,2]
    )
    
    #-----warehouses are also randomly placed on the grid
    m <- 16
    warehouse_locations <- data.frame(
      id = 1:m,
      x = centroides[,1],
      y = centroides[,2]
    )
    
    
    #PLOT principal
    p <- ggplot(customer_locations, aes(x, y)) + 
      geom_point() + 
      geom_point(data = warehouse_locations, color = "red", alpha = 0.5, shape = 17) +
      scale_x_continuous(limits = c(-1.6, grid_size)) +
      scale_y_continuous(limits = c(-1.6, grid_size)) +
      theme(axis.title = element_blank(), 
            axis.ticks = element_blank(), 
            axis.text = element_blank(), panel.grid = element_blank())
    p
  })
})