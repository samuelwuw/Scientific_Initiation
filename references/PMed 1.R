x<-rnorm(100)
y<-rnorm(100)

coor<-cbind(x,y)
plot(coor)

install.packages("tbart")
library(tbart)

#vec com coordenadas x e y dos pontos (customers)
coor<-cbind(x,y) 

# função allocate
alocar <- allocate(coor,p=5) 

diagrama <- star.diagram(coor,alloc=alocar)

plot(diagrama, axes = TRUE)

points(coor)
