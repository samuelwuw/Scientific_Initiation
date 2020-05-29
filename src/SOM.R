getwd()
setwd("C:/Users/samue/Documents/www/IC/Scientific_Initiation/src")

library(kohonen)
require(kohonen)

df <- read.csv('database/uscitiesCsv.csv', header = TRUE, sep = ",")

# Dellaware DATA FRAME with 2 Var (latitude and longitude), 77 cities
df_del <- df[621:697, c(9,10)] 

# data normalization
data_train_matrix <- as.matrix(scale(df_del)) 

som_grid <- somgrid(xdim = 4, ydim = 4, topo="hexagonal") # SOM 6x6, hexagonal


som_model <- som(data_train_matrix, 
                 grid=som_grid,  
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 radius = 5)

#plots of SOM model
plot(som_model, type="changes")

#quantidade de amostras mapeadas em cada node (centroide)
plot(som_model, type="count", main = "node counts")

plot(som_model, type="dist.neighbours", main = "SOM neighbour distances") 

#aaa 
plot(som_model, type="codes", main = "codes") 

som_model$unit.classif 

#heatmaps:
#allows the visualisation of the distribution of a single variable across the map
plot(som_model, type = "property", 
     property = getCodes(som_model)[,1],
     main=colnames(getCodes(som_model))[1]) 
plot(som_model, type = "property",
     property = getCodes(som_model)[,2],
     main=colnames(getCodes(som_model))[2]) 


# Montagem de DF com os Neur?nios do SOM para Washington
mydata <- as.data.frame(som_model$codes)

# cALCULANDO sOMAS DE qUADRADOS (Vari?ncias) para o Mydata - WASHINGTON
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 

######### Clusteriza??o dos N?s do SOM, pelo K-means (variando o K) ###########
# Registra a Varia??o Interna (WSS) de cada Cluster e Soma essas varia??es

for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}

###############################################################################

# Plotagem do Gr?fico de Cotovelo - WSS vs. K
plot(wss, main = "nrow*variances")

pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

#################  Hierarchical Clustering - hclust()  #########################

#################  Distance Matrix Computation dist()  #########################

##### Monta Clusters da Matriz de Distancias entre os Neuronios ###########

#### cuttree()  -  Cut a Tree into Groups of Data
### Cuts a tree, e.g., as resulting from hclust, into several groups either by specifying the desired number(s) of groups or the cut height(s)
#### cutree(tree, k = NULL, h = NULL)

som_cluster <- cutree(hclust(dist(as.data.frame(som_model$codes))), 6)
plot(som_cluster)
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)

som_cluster

colnames(localiz) <- c("node")


#centroides de cada estado (16)
centroides <- as.data.frame(som_model$codes)
plot(centroides)
View(centroides)

#pontos de demanda
plot(data_train_matrix)

#########################################################################
##########################     PLOTS     ################################
#########################################################################


library(ggplot2)
require(ggplot2)

grid_size <- 2.3
#vetor com distâncias entre os customers e warehouses, e centroides para sua media 
customerDistanceVector <- c()
centroidDistanceVector <- c()
customerCostVector <- c()
centroidCostVector <- c()
#indica a qual warehouse cada customer está atrelado
localiz <- as.matrix(som_model$unit.classif)
m <- 16 #usado em warehouse locations, id
n <- 77 #usado em customer locations, id 
D <- 0
x_mean <- mean(centroides[,1]) #media x dos centroides  
y_mean <- mean(centroides[,2]) #media y dos centroides
centroid_id <- 16

#calcula o custo do transporte entre o ponto de demanda e o seu armazÃ©m
distanc <- function(Xc, Yc, Xw, Yw){
  distance <- sqrt((Xw-Xc)**2+(Yw-Yc)**2)
  return(distance)
}

warehouse_locations <- data.frame(
  id = 1:m,
  x = centroides[,1],
  y = centroides[,2]
)
View(warehouse_locations)
summary(localiz)

customer_locations <- data.frame(
  id = 1:n,
  x = data_train_matrix[,1],
  y = data_train_matrix[,2],
  localiz
)
View(customer_locations)

for(val in 1:centroid_id){
  D <- distanc(centroides$lat[[val]], centroides$lng[[val]], 
               x_mean, y_mean)
  
  centroidDistanceVector[val] <- D 
}
View(centroidDistanceVector)

centroid_dist_to_mean <- data.frame(
  id = 1:centroid_id,
  x = centroides[,1],
  y = centroides[,2],
  dist = centroidDistanceVector
  #cost
)
View(centroid_dist_to_mean)

for(val in customer_locations$id){
  D <- distanc(customer_locations$x[[val]], customer_locations$y[[val]],
          warehouse_locations$x[[customer_locations$localiz[[val]]]], 
          warehouse_locations$y[[customer_locations$localiz[[val]]]])
  
  customerDistanceVector[val] <- D 
  customerCostVector[val] <- D * 2.5
  
}
View(customerDistanceVector)


#PLOT principal
p <- ggplot(customer_locations, aes(x, y)) + 
  geom_point() + 
  geom_point(data = warehouse_locations, color = "red", alpha = 0.5, shape = 17) +
  scale_x_continuous(limits = c(-1.6, grid_size)) +
  scale_y_continuous(limits = c(-1.6, grid_size)) +
  theme(axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), panel.grid = element_blank())
p + ggtitle("Warehouse location problem", 
            "Black dots are customers. Light red triangles show potential warehouse locations.")







