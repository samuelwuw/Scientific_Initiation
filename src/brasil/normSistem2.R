#forma alternativa de normalização dos dados
getwd()
setwd("C:/Users/samue/Documents/www/IC/Scientific_Initiation/src/brasil")

library(kohonen)
require(kohonen)
library(RSNNS)
somFunc <- kohonen::som

df <- read.csv('database/brCitiesCsv.csv', header = TRUE, sep = ",")

#Base de dados com cidades proeminentes do nordeste, e sudeste
usedCities <- c(4:9, 30:60, 61:79, 81:85, 104:122, 141:183, 208:210, 231:242, 243:249, 250:259, 260:266, 
                328:374, 375:376)
df_cities <- df[usedCities, c(2,3,8)]
rownames(df_cities) <- NULL

#data_train_matrix <- as.matrix(scale(df_cities)) 
data_train_matrix <- as.matrix(normalizeData(df_cities, type = "norm")) 
colnames(data_train_matrix) <- c("lat", "lng", "population")

som_grid <- somgrid(xdim = 3, ydim = 4, topo="hexagonal") # SOM 3x5, hexagonal

som_model <- somFunc(data_train_matrix, 
                 grid=som_grid,  
                 rlen=300, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 radius = 5)

#plots of SOM model
plot(som_model, type="changes")

#quantidade de amostras mapeadas em cada node (centroide)
plot(som_model, type="count", main = "node counts")

plot(som_model, type="dist.neighbours", main = "SOM neighbour distances") 

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

for (i in 12) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}

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

colnames(localiz) <- c("node")

#centroides de cada estado (16)
centroides <- as.data.frame(som_model$codes)
plot(centroides)

centroides_norm <- as.data.frame(denormalizeData(centroides, getNormParameters(data_train_matrix)))

#pontos de demanda
plot(x = data_train_matrix[,1], y = data_train_matrix[,2], xlab = "X", ylab = "y")

#########################################################################
##########################     PLOTS     ################################
#########################################################################


library(ggplot2)
require(ggplot2)

#population
df_cities_population <- df[usedCities, c(1,8)] #population column

#vetor com distâncias entre os customers e warehouses, e centroides para sua media 
customerDistanceVector <- c()

#vetor com distâncias entre a média da posição dos centroides, e cada um deles
centroidDistanceVector <- c()

#vector with costs based in distance
centroid_costPerSquareMeter <- c()

customerCostVector <- c()
centroidCostVector <- c()

#indica a qual warehouse cada customer está atrelado
localiz <- as.matrix(som_model$unit.classif)

m <- 12 #usado em warehouse locations, id  (15)
n <- 211 #usado em customer locations, id (142)
D <- 0
x_mean <- mean(centroides[,1]) #media x dos centroides  
y_mean <- mean(centroides[,2]) #media y dos centroides
centroid_id <- 12

#data frame customer locations
customer_locations <- data.frame(
  id = 1:n,
  x = df_cities[,1],
  y = df_cities[,2],
  localiz,
  population = df_cities_population$population
  
)
View(customer_locations)

#calcula o custo do transporte entre o ponto de demanda e o seu arma
distanc <- function(Xc, Yc, Xw, Yw){
  distance <- sqrt((Xw-Xc)**2+(Yw-Yc)**2)
  return(distance)
}

#cálculo da distância entre cada centroide e a mádia dos centroides
for(val in 1:m){
  D <- distanc(centroides$lat[[val]], centroides$lng[[val]], 
               x_mean, y_mean)
  
  centroidDistanceVector[val] <- D 
}
View(centroidDistanceVector)

#def of quartiles of distances between centroids mean and its locations
quartile1 <- quantile(centroidDistanceVector, 0.25)
quartile2 <- quantile(centroidDistanceVector, 0.5) 
quartile3 <- quantile(centroidDistanceVector, 0.75) 

for(val in 1:m){
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

#soma a população de cada centroide
clustPop <- vector(length = m)

for(i in 1:m){
  for(j in 1:n){
    if(customer_locations$localiz[j] == i){
      clustPop[i] <- clustPop[i] + customer_locations$population[j]
      
    }
  }
}
View(clustPop)


#calc of warehouse size and cost
warehouse_costs <- vector(length = m)
warehouse_size <- vector(length = m)
meter_per_habitant <- 1
for(i in 1:m){
  warehouse_size[i] <- (clustPop[i] * meter_per_habitant) / 100
  warehouse_costs[i] <- warehouse_size[i] * centroid_costPerSquareMeter[i]
}

warehouse_locations <- data.frame(
  id = 1:centroid_id,
  x = centroides_norm$V1,
  y = centroides_norm$V2,
  dist_to_mean = centroidDistanceVector, #dist of each waarehouse to all warehouse mean
  cost_per_square_meter = centroid_costPerSquareMeter, #cost based on dist_to_mean quartiles (line 162)
  total_population = clustPop,
  warehouse_size = warehouse_size, #size based on population 
  warehouse_costs = warehouse_costs #cost based on warehouse_size and cost_per_square_meter
)
View(warehouse_locations)

#calc of dist between customer and respectives warehouses
#Normalizado
for(val in customer_locations$id){
  D <- distanc(customer_locations$x[[val]], customer_locations$y[[val]],
               warehouse_locations$x[[customer_locations$localiz[[val]]]],
               warehouse_locations$y[[customer_locations$localiz[[val]]]])
  
  customerDistanceVector[val] <- D
  customerCostVector[val] <- D * 2.5
}
View(customerDistanceVector)

#haversine
library(pracma)
require(pracma)

#transport cost calculation
transportcost_func <- function(i, j) {
  customer <- customer_locations[i, ]
  warehouse <- warehouse_locations[j, ]
  # calcula o custo de transporte: 
  return(haversine(c(customer$x, customer$y), c(warehouse$x, warehouse$y)) 
         * (2.5/25) * (warehouse$warehouse_size * 12/0.3))
}
transportcost_func(1,7)


transportCostMatrixFact <- function(){
  transport_cost <- matrix(nrow = n, ncol = m)
  
  for(row in 1:n){
    for(col in 1:m){
      transport_cost[row, col] <- transportcost_func(row, col)
    }
  }
  
  return(transport_cost)
}
transport_cost <- as.data.frame(transportCostMatrixFact())
View(transport_cost)
summary(transport_cost)

grid_size <- 0
#principal PLOT
p <- ggplot(customer_locations, aes(x, y)) +
  geom_point() +
  geom_point(data = warehouse_locations, color = "red", alpha = 0.5, shape = 17) +
  scale_x_continuous(limits = c(-25, -2)) +
  scale_y_continuous(limits = c(-53, -33)) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank())
p + ggtitle("Warehouse location problem",
            "Black dots are customers. Light red triangles show potential warehouse locations.")

#solving model
library(ompr)
library(magrittr)
#masked functions: and, mod, or

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

###### problema com fixed cost (o custo fixo deste código varia)
#armazéns escolhidos
plot_warehouses <- warehouse_locations %>% 
  mutate(costs = warehouse_costs) %>% 
  inner_join(customer_count, by = "id") %>% 
  filter(id %in% unique(matching$j))

p + 
  geom_segment(data = plot_assignment, aes(x = x.y, y = y.y, xend = x.x, yend = y.x)) + 
  geom_point(data  = plot_warehouses, color = "red", size = 3, shape = 17) +
  ggrepel::geom_label_repel(data  = plot_warehouses, 
                            aes(label = paste0("fixed costs:", costs, "; customers: ", n )), 
                            size = 3, nudge_y = 20) + 
  ggtitle(paste0("Cost optimal warehouse locations and customer assignment"),
          "Big red triangles show warehouses that will be built, light red are unused warehouse locations. 
Dots represent customers served by the respective warehouses.")

#fixed costs for setting up the 4 warehouses:
sum(warehouse_costs[unique(matching$j)])













#################################################################################################################
######################################## OBJETIVOS ##############################################################
#################################################################################################################

#1)
# Somar as populações das 77 cidades de delaware
# Dividir a população de cada cidade pelo total somado (dplyr package)
# Pegar o resultado de cada divisão (77 indices), e multiplica pela população real (google) de delaware (var realPop)
# O resultado será a população aproximada de cada cidade

#2)
# Depois vamos estabelecer um valor de m² de armazém por habitante (1m² por habitante)
# Multiplica esse valor pela população de cada cidade = tamanho de cada armazén na cidade
# multiplicar pelos custos por M² que já estão no data frame

#3)
# adicionar 2 colunas ao warehouse locations:
# uma será o tamanho du cluster ( a área de armazén = população * parmetro p)
# a outra coluna custo total será o custo do armazén, que será a área do armazén * custo por m²

#4)
# melhorar vetor de custo de transporte, adicionando o custo de cada cidade para todos os armazéns (16), 
# para assim vermos quais armazéns são os melhores
# tentar recriar função de "transport cost" do warehouse locations

#5)
#pegar a soma da coluna "i" do vetor de custo de transporte, e a "i" linha do custo fixo do vetor de armazéns.

#6)
#Usar o modelo MIP do script warehouse.R, trocando a função "transportcost()" pelo valor i
#no data frame "transport_cost", e trocar
# (Problema no resultado do modelo mip)

# 7) 
# calcular a matriz com os dados originais (77 por 77), e outra com os dados normalizados.
# tirar a média geral das duas matrizes, e divide a média dos dados originais pela média dos dados normalizados.
# usar esse valor para montar a matriz "transport_cost", multiplicando a dist por esse valor
# (esperar um valor alto)

# 8) montar a matriz de transport_cost (237)



fra <- c(df$lat[328], df$lng[328])
ord <- c(df$lat[250], df$lng[250])

dis <- haversine(fra, ord)
fprintf('Flight distance Frankfurt-Chicago is %8.3f km.\n', dis)

