getwd()
setwd("C:/Users/samue/Documents/www/IC/Scientific_Initiation/src/USA")

library(kohonen)
require(kohonen)

df <- read.csv('database/uscitiesCsv.csv', header = TRUE, sep = ",")

# Dellaware DATA FRAME with 2 Var (latitude and longitude), 77 cities
df_del <- df[621:697, c(9,10,11)] 

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
plot(som_model, type = "property",
     property = getCodes(som_model)[,3],
     main=colnames(getCodes(som_model))[3]) 


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

#population
df_del_population <- df[621:697, c(1,11)] #population column

grid_size <- 2.3
#vetor com dist�ncias entre os customers e warehouses, e centroides para sua media 
customerDistanceVector <- c()

#vetor com dist�ncias entre a m�dia da posi��o dos centroides, e cada um deles
centroidDistanceVector <- c()

#vector with costs based in distance
centroid_costPerSquareMeter <- c()

customerCostVector <- c()
centroidCostVector <- c()

#indica a qual warehouse cada customer est� atrelado
localiz <- as.matrix(som_model$unit.classif)

m <- 16 #usado em warehouse locations, id
n <- 77 #usado em customer locations, id 
D <- 0
x_mean <- mean(centroides[,1]) #media x dos centroides  
y_mean <- mean(centroides[,2]) #media y dos centroides
centroid_id <- 16

#calcula o custo do transporte entre o ponto de demanda e o seu armazém
distanc <- function(Xc, Yc, Xw, Yw){
  distance <- sqrt((Xw-Xc)**2+(Yw-Yc)**2)
  return(distance)
}

#data frame customer locations
customer_locations <- data.frame(
  id = 1:n,
  x = data_train_matrix[,1],
  y = data_train_matrix[,2],
  localiz,
  population = df_del_population$population
  
)
View(customer_locations)


#calculation of dist of centroid locations mean, and locations
for(val in 1:centroid_id){
  D <- distanc(centroides$lat[[val]], centroides$lng[[val]], 
               x_mean, y_mean)
  
  centroidDistanceVector[val] <- D 
}
View(centroidDistanceVector)

#def of quartiles of distances between centroids mean and its locations
quartile1 <- quantile(centroidDistanceVector, 0.25)
quartile2 <- quantile(centroidDistanceVector, 0.5) 
quartile3 <- quantile(centroidDistanceVector, 0.75) 

for(val in 1:centroid_id){
  if(centroidDistanceVector[val] <= quartile1){
    centroid_costPerSquareMeter[val] <- 2000 #custo por metro quadrado
  } 
  if(centroidDistanceVector[val] > quartile1 && centroidDistanceVector[val] <= quartile2){
    centroid_costPerSquareMeter[val] <- 1500
  } 
  if(centroidDistanceVector[val] > quartile2 && centroidDistanceVector[val] <= quartile3){
    centroid_costPerSquareMeter[val] <- 1000
  } 
  if(centroidDistanceVector[val] > quartile3 ){
    centroid_costPerSquareMeter[val] <- 500
  } 
} 
View(centroid_costPerSquareMeter)

#soma a popula��o de cada centroide
clustPop <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(i in 1:m){
  for(j in 1:n){
    if(customer_locations$localiz[j] == i){
      clustPop[i] <- clustPop[i] + customer_locations$population[j]
    }
  }
}
View(clustPop)


#calc of warehouse size and cost
warehouse_costs <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
warehouse_size <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
meter_per_habitant <- 1
for(i in 1:16){
  warehouse_size[i] <- (clustPop[i] * meter_per_habitant) / 100
  warehouse_costs[i] <- warehouse_size[i] * centroid_costPerSquareMeter[i]
}

warehouse_locations <- data.frame(
  id = 1:centroid_id,
  x = centroides[,1],
  y = centroides[,2],
  dist_to_mean = centroidDistanceVector, #dist of each waarehouse to all warehouse mean
  cost_per_square_meter = centroid_costPerSquareMeter, #cost based on dist_to_mean quartiles (line 162)
  total_population = clustPop,
  warehouse_size = warehouse_size, #size based on population 
  warehouse_costs = warehouse_costs #cost based on warehouse_size and cost_per_square_meter
)
View(warehouse_locations)
summary(localiz)


#calc of dist between customer and respectives warehouses
for(val in customer_locations$id){
  D <- distanc(customer_locations$x[[val]], customer_locations$y[[val]],
          warehouse_locations$x[[customer_locations$localiz[[val]]]],
          warehouse_locations$y[[customer_locations$localiz[[val]]]])
  
  customerDistanceVector[val] <- D
  customerCostVector[val] <- D * 2.5
}
View(customerDistanceVector)

#transport cost calculation
calc_transport_dist <- function(id){
  vec <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  for(centroid_counter in 1:77){
    D <- distanc(customer_locations$x[[centroid_counter]], customer_locations$y[[centroid_counter]],
                 warehouse_locations$x[[id]], warehouse_locations$y[[id]])
    
    vec[centroid_counter] <- D 
  }
  
  return(vec)
}

transport_cost <- data.frame(
  centroide1 = calc_transport_dist(1), 
  centroide2 = calc_transport_dist(2), 
  centroide3 = calc_transport_dist(3), 
  centroide4 = calc_transport_dist(4), 
  centroide5 = calc_transport_dist(5), 
  centroide6 = calc_transport_dist(6), 
  centroide7 = calc_transport_dist(7), 
  centroide8 = calc_transport_dist(8),
  centroide9 = calc_transport_dist(9), 
  centroide10 = calc_transport_dist(10), 
  centroide11 = calc_transport_dist(11), 
  centroide12 = calc_transport_dist(12),
  centroide13 = calc_transport_dist(13),
  centroide14 = calc_transport_dist(14), 
  centroide15 = calc_transport_dist(15), 
  centroide16 = calc_transport_dist(16)
)
View(transport_cost)
summary(transport_cost)

#prove
print(
  distanc(customer_locations$x[[1]], customer_locations$y[[1]],
        warehouse_locations$x[[1]], warehouse_locations$y[[1]])
)

#TRANSPORT COST FUNCTION
transportcost_func <- function(i, j) {
  customer <- customer_locations[i, ]
  warehouse <- warehouse_locations[j, ]
  (sqrt((customer$x - warehouse$x)^2 + (customer$y - warehouse$y)^2)) * 2.50 * 100 #substituir 100 pelo valor correto
}

transportcost_func(1,1)

#principal PLOT
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

#solving model
library(ompr)
library(magrittr)
n <- 77
m <- 16
model_MIP <- MIPModel() %>%
  # 1 iff i gets assigned to warehouse j
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # 1 iff warehouse j is built
  add_variable(y[j], j = 1:m, type = "binary") %>%
  
  # maximize the preferences
  set_objective(sum_expr(transportcost_func(i, j) * x[i, j], i = 1:n, j = 1:m) +    #trocar por transport_cost[i,j]
                  sum_expr(warehouse_costs[j] * y[j], j = 1:m), "min") %>%           #trocar por warehouse_costs[j]
  
  # every customer needs to be assigned to a warehouse
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n) %>% 
  
  # if a customer is assigned to a warehouse, then this warehouse must be built
  add_constraint(x[i,j] <= y[j], i = 1:n, j = 1:m)
model_MIP

library(ompr.roi)
library(ROI.plugin.glpk)
result <- solve_model(model_MIP, with_ROI(solver = "glpk", verbose = TRUE))

suppressPackageStartupMessages(library(dplyr))
matching <- result %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%  
  select(i, j)


#add the assignments to the previous plot
plot_assignment <- matching %>% 
  inner_join(customer_locations, by = c("i" = "id")) %>% 
  inner_join(warehouse_locations, by = c("j" = "id"))
customer_count <- matching %>% group_by(j) %>% summarise(n = n()) %>% rename(id = j)

###### problema com fixed cost (o custo fixo deste c�digo varia)
plot_warehouses <- warehouse_locations %>% 
  mutate(costs = warehouse_costs) %>% 
  inner_join(customer_count, by = "id") %>% 
  filter(id %in% unique(matching$j))
p + 
  geom_segment(data = plot_assignment, aes(x = x.y, y = y.y, xend = x.x, yend = y.x)) + 
  geom_point(data  = plot_warehouses, color = "red", size = 3, shape = 17) +
  ggrepel::geom_label_repel(data  = plot_warehouses, 
                            aes(label = paste0("fixed costs:", costs, "; customers: ", n)), 
                            size = 2, nudge_y = 20) + 
  ggtitle(paste0("Cost optimal warehouse locations and customer assignment"),
          "Big red triangles show warehouses that will be built, light red are unused warehouse locations. 
Dots represent customers served by the respective warehouses.")

#fixed costs for setting up the 4 warehouses:
sum(warehouse_costs[unique(matching$j)])













#################################################################################################################
######################################## OBJETIVOS ##############################################################
#################################################################################################################

#1)
# Somar as popula��es das 77 cidades de delaware
# Dividir a popula��o de cada cidade pelo total somado (dplyr package)
# Pegar o resultado de cada divis�o (77 indices), e multiplica pela popula��o real (google) de delaware (var realPop)
# O resultado ser� a popula��o aproximada de cada cidade

#2)
# Depois vamos estabelecer um valor de m� de armaz�m por habitante (1m� por habitante)
# Multiplica esse valor pela popula��o de cada cidade = tamanho de cada armaz�n na cidade
# multiplicar pelos custos por M� que j� est�o no data frame

#3)
# adicionar 2 colunas ao warehouse locations:
# uma ser� o tamanho du cluster ( a �rea de armaz�n = popula��o * parmetro p)
# a outra coluna custo total ser� o custo do armaz�n, que ser� a �rea do armaz�n * custo por m�

#4)
# melhorar vetor de custo de transporte, adicionando o custo de cada cidade para todos os armaz�ns (16), 
# para assim vermos quais armaz�ns s�o os melhores
# tentar recriar fun��o de "transport cost" do warehouse locations

#5)
#pegar a soma da coluna "i" do vetor de custo de transporte, e a "i" linha do custo fixo do vetor de armaz�ns.

#6)
#Usar o modelo MIP do script warehouse.R, trocando a fun��o "transportcost()" pelo valor i
#no data frame "transport_cost", e trocar
# (Problema no resultado do modelo mip)

#7) 
# calcular a matriz com os dados originais (77 por 77), e outra com os dados normalizados.
# tirar a m�dia geral das duas matrizes, e divide a m�dia dos dados originais pela m�dia dos dados normalizados.
# usar esse valor para montar a matriz "transport_cost", multiplicando a dist por esse valor
# (esperar um valor alto)


##################################### population data normalization (not used) ######################
View(df_del_population)
delaware_real_population <- 973764
pop_sum <- sum(df_del_population$population) #sum the column

library(dplyr)
df_del_population <- df_del_population %>%
  mutate(divis = population/pop_sum)

divis_sum <- sum(df_del_population$divis) #check, the sum of this column must be 1

df_del_population <- df_del_population %>%
  mutate(realPop = round(divis*delaware_real_population))

df_del_population <- df_del_population %>%
  mutate(warehouseSize = realPop*0.5)

View(df_del_population)
#######################################################################################################






calc_dist_cityToCity <- function(id){
  dataFrame <- data.frame{
    
  }
  
  for(city_counter in 1:77){
    for(city2_counter in 1:77){
      dataFrame$ <- distanc(df_del$lat[[city_counter]], df_del$lng[[city_counter]],
                   df_del$lat[[city2_counter]], df_del$lng[[city2_counter]])
      
      vec[city_counter] <- D 
    }
  }
  
  return(vec)
}

transport_cost <- data.frame(
  centroide1 = calc_transport_dist(1), 
  centroide2 = calc_transport_dist(2), 
  centroide3 = calc_transport_dist(3), 
  centroide4 = calc_transport_dist(4), 
  centroide5 = calc_transport_dist(5), 
  centroide6 = calc_transport_dist(6), 
  centroide7 = calc_transport_dist(7), 
  centroide8 = calc_transport_dist(8),
  centroide9 = calc_transport_dist(9), 
  centroide10 = calc_transport_dist(10), 
  centroide11 = calc_transport_dist(11), 
  centroide12 = calc_transport_dist(12),
  centroide13 = calc_transport_dist(13),
  centroide14 = calc_transport_dist(14), 
  centroide15 = calc_transport_dist(15), 
  centroide16 = calc_transport_dist(16)
)
View(transport_cost)
summary(transport_cost)

