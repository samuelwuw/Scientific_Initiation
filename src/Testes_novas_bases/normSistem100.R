getwd()
setwd("C:/Users/swu/Documents/facul/TCC/Projeto/Scientific_Initiation/src/Testes_novas_bases")

library(kohonen)
require(kohonen)
library(RSNNS)
somFunc <- kohonen::som

database <- file.choose(new = FALSE)
df <- read.csv(database, header = TRUE, sep = ";")

#Base de dados com cidades proeminentes do nordeste, e sudeste

df_cities <- df[, c(2,3,4)]
rownames(df_cities) <- NULL

#data_train_matrix <- as.matrix(scale(df_cities)) 
data_train_matrix <- as.matrix(normalizeData(df_cities, type = "norm")) 
colnames(data_train_matrix) <- c("lat", "lng", "population")

som_grid <- somgrid(xdim = 4, ydim = 3, topo="hexagonal") # SOM 3x4, hexagonal

som_model <- somFunc(data_train_matrix, 
                 grid=som_grid,  
                 rlen=300, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 radius = 5)

#centroides de cada estado (12)
centroides <- as.data.frame(som_model$codes)
View(centroides)

#processo de denormaliza��o.
centroides_norm <- as.data.frame(denormalizeData(centroides, getNormParameters(data_train_matrix)))

#plots of SOM model
plot(som_model, type="changes")

#quantidade de amostras mapeadas em cada node (centroide)
plot(som_model, type="count", main = "node counts")

plot(som_model, type="dist.neighbours", main = "SOM neighbour distances") 

plot(som_model, type="codes", main = "codes") 

som_model$unit.classif 

#########################################################################
##########################     PLOTS     ################################
#########################################################################

library(ggplot2)
require(ggplot2)

m <- 12 #usado em warehouse locations
n <- as.double(NROW(df)) #usado em customer locations
D <- 0
x_mean <- mean(centroides[,1]) #media x dos centroides  
y_mean <- mean(centroides[,2]) #media y dos centroides

#population
df_cities_population <- df[, c(1,4)] #population column
for(val in 1:n){
  df_cities_population[val, 2] <- df_cities_population[val, 2]
}

#vetor com distâncias entre os customers e warehouses, e centroides para sua media 
customerDistanceVector <- c()

#vetor com distâncias entre a media da posit dos centroides, e cada um deles
centroidDistanceVector <- c()

#vector with costs based in distance
centroid_costPerSquareMeter <- c()

customerCostVector <- c()
centroidCostVector <- c()

#indica a qual warehouse cada customer está atrelado
#localiz <- as.matrix(som_model$unit.classif)

#data frame customer locations
customer_locations <- data.frame(
  id = 1:n,
  x = df_cities[,1],
  y = df_cities[,2],
  localiz = as.matrix(som_model$unit.classif),
  population = df_cities_population$quantity
  
)
View(customer_locations)

#calcula o custo do transporte entre o ponto de demanda e o seu arma
distanc <- function(Xc, Yc, Xw, Yw){
  distance <- sqrt((Xw-Xc)**2+(Yw-Yc)**2)
  return(distance)
}

#c�lculo da dist�ncia entre cada centroide e a m�dia dos centroides
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

#soma a popula��o de cada centroide
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
  warehouse_size[i] <- (clustPop[i] * meter_per_habitant)
  warehouse_costs[i] <- warehouse_size[i] * centroid_costPerSquareMeter[i]
}

warehouse_locations <- data.frame(
  id = 1:m,
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
  #return(haversine(c(customer$x, customer$y), c(warehouse$x, warehouse$y)) 
  #       * (2.5/25) * (warehouse$warehouse_size * 12/0.3))
  return(distanc(customer$x, customer$y, warehouse$x, warehouse$y) 
         * (2.5/25) * (warehouse$warehouse_size * 12/0.3))
  
}
transportcost_func(1,5)


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

grid_size <- 1000
#principal PLOT
p <- ggplot(customer_locations, aes(x, y)) +
  geom_point() +
  geom_point(data = warehouse_locations, color = "red", alpha = 0.5, shape = 17) +
  scale_x_continuous(limits = c(-25, grid_size)) +
  scale_y_continuous(limits = c(-53, grid_size)) +
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
          "Big red triangles show warehouses that will be built, light red are unused warehouse 
          locations. Dots represent customers served by the respective warehouses.")

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

