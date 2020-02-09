#importar base de dados - cereais

library(readxl)
cereals <- read_excel("cereals.xlsx")
View(cereals)

#### Analisar e tratar os dados ####

# 1) Analise univariada

# a) Missing values: verificando valores missing

which(is.na(cereals)) # nao ha missing values

# analisando variaveis 
summary (cereals)

# b) Valores negativos: as variaveis carbo, sugars, potass possuem valores negativos. 
#Sao tres registros no total.

#identificar as linhas com valores negativos:
library(dplyr)
cereals %>%
  filter( carbo < 0 | sugars <0 | potass <0) -> cereals.neg

cereals.neg

# excluindo as variaveis negativas
cereals %>%
  filter( carbo >= 0 & sugars >=0 & potass >=0) -> ncereals

View (ncereals)
summary (ncereals)


# c) Outliers

colnames(ncereals)

#boxplot das variaveis quanti:

par(mfrow=c(1,4))
boxplot(ncereals$calories, main="calories", col="blue")
boxplot(ncereals$protein, main="protein",col="yellow")
boxplot(ncereals$fat, main="fat",col="green")
boxplot(ncereals$sodium, main="sodium", col="red")

par(mfrow=c(1,4))
boxplot(ncereals$fiber, main="fiber", col="blue")
boxplot(ncereals$carbo, main="carbo",col="yellow")
boxplot(ncereals$sugars, main="sugars",col="green")
boxplot(ncereals$potass, main="potass", col="red")

par(mfrow=c(1,4))
boxplot(ncereals$rating, main="rating", col="blue")

#fat, fiber e potass foram consideradas assimétricas

#optou-se pela exclusao dos outliers conforme filtros abaixo

#cria tabela com os outliers
ncereals %>%
  filter(
    calories <= 80 | calories >= 130 |
      protein == 6 |
      sodium == 0 |
      rating >= 93,7
  ) -> outliers

#exclui os outliers

ncereals %>%
  filter(
    calories > 80 & calories < 130 &
      protein != 6 &
      sodium != 0 &
      rating < 93,7
  ) -> ncereals1

#testar se ainda restam outliers

par(mfrow=c(1,4))
boxplot(ncereals1$calories, main="calories", col="blue")
boxplot(ncereals1$protein, main="protein",col="yellow")
boxplot(ncereals1$sodium, main="sodium", col="red")
boxplot(ncereals1$rating, main="rating", col="green")

#transformar as variaveis mfr e type em factor
ncereals1$mfr=as.factor(ncereals1$mfr)
ncereals1$type=as.factor(ncereals1$type)
ncereals1$name=as.factor(ncereals1$name)

#analise descritiva do dataframe ncereals1
summary (ncereals1)

# 2) verificar a correlacao entre as variaveis quantitativas (colunas 4 a 12)

correl=cor(ncereals1[,4:12])
round(correl,digits=3)


#          calories protein    fat sodium  fiber  carbo sugars potass rating
#calories    1.000  -0.245  0.412  0.050 -0.143 -0.208  0.426  0.007 -0.629
#protein    -0.245   1.000  0.185 -0.108  0.676 -0.066 -0.351  0.650  0.661
#fat         0.412   0.185  1.000 -0.336  0.116 -0.516  0.118  0.257 -0.291
#sodium      0.050  -0.108 -0.336  1.000 -0.139  0.627 -0.368 -0.089 -0.063
#fiber      -0.143   0.676  0.116 -0.139  1.000 -0.248 -0.075  0.912  0.569
#carbo      -0.208  -0.066 -0.516  0.627 -0.248  1.000 -0.769 -0.314  0.409
#sugars      0.426  -0.351  0.118 -0.368 -0.075 -0.769  1.000  0.035 -0.701
#potass      0.007   0.650  0.257 -0.089  0.912 -0.314  0.035  1.000  0.367
#rating     -0.629   0.661 -0.291 -0.063  0.569  0.409 -0.701  0.367  1.000

#observa-se que fibra e potassio possuem alta correlacao (0,912)

#grafico para mostrar a correlacao
library(corrplot)
par(mfrow=c(1,2))
corrplot(correl, method ="number", type = "upper", order="hclust",tl.col="black", tl.srt=45) 
corrplot(correl, method ="circle", type = "upper", order="hclust",tl.col="black", tl.srt=45)

#excluir o potassio por ser considerado um componente nutricional menos relevante
#a fibra

fcereals=ncereals1[,-11]

View (fcereals)

# 3) Cluster Analysis

# a) considerando todas as variaveis como drivers

# calcular a matriz de distancias com daisy pq temos variaveis quali e quanti
# nao precisamos padronizar. daisy usa a metrica de Gower que ja faz isso
# utilizar daisy apenas quando existir qualitativa. Quando somente quantitativa 
#usar dist().

library(cluster)
cereals.todas <- fcereals
dist.cereals.todas=daisy(cereals.todas) #calculo da distancia


#fazer dendograma para ter uma ideia da quantidade de clusters

hc.cereals=hclust(dist.cereals.todas, method = "ward.D2")
plot(hc.cereals, hang=-1)

#algorimo pamk (pamk = particao ao redor da medoide) da library fpc ajuda a 
#definir numero de clusters com asw (para os casos de variaveis quali)

library(fpc)
set.seed(23) 
kk=pamk(dist.cereals.todas, krange = 2:6, diss = T, critout = T ) 

#diss = T informa que estamos usando a matriz de distancia, krange significa 
#que pode ter de 2 a 6 clusters (por causa do dendograma).
#a quantidade de cluster escolhida foi a que apresentar maior valor (4):

#2  clusters  0.1741282 
#3  clusters  0.1297191 
#4  clusters  0.1909146 
#5  clusters  0.1569641 
#6  clusters  0.1759362

#mostra quantos clusters temos
kk$nc
#[1] 4

#mostra quem sao os medoids
kk$pamobject$medoids

#cria a variavel que marca o cluster a que pertence cada cereal
cereals.todas$kpamc=kk$pamobject$clustering
cereals.todas$kpamc

View(cereals.todas)

#pacote para ajudar a fazer relatorios, graficos
library(factoextra)

# grafico com a projecao do cluster
fviz_cluster(list(data = cereals.todas[,4:11], cluster = cereals.todas$kpamc), show.clust.cent = F)

#marcas em cada cluster =>mostra quais cereais estao em cada cluster
cereals.todas$name[cereals.todas$kpamc==1]
cereals.todas$name[cereals.todas$kpamc==2]
cereals.todas$name[cereals.todas$kpamc==3]
cereals.todas$name[cereals.todas$kpamc==4]

#analises

colnames(cereals.todas)
par(mfrow=c(1,4))

#analise quantitaiva 
boxplot(cereals.todas$calories~cereals.todas$kpamc, col=rainbow(4), main="calories")
boxplot(cereals.todas$protein~cereals.todas$kpamc, col=rainbow(4), main="protein")
boxplot(cereals.todas$fat~cereals.todas$kpamc, col=rainbow(4), main="fat")
boxplot(cereals.todas$sodium~cereals.todas$kpamc, col=rainbow(4), main="sodium")

boxplot(cereals.todas$fiber~cereals.todas$kpamc, col=rainbow(4), main="fiber")
boxplot(cereals.todas$carbo~cereals.todas$kpamc, col=rainbow(4), main="carbo")
boxplot(cereals.todas$sugars~cereals.todas$kpamc, col=rainbow(4), main="sugars")
boxplot(cereals.todas$rating~cereals.todas$kpamc, col=rainbow(4), main="rating")

#analise qualitativa
table(cereals.todas$name, cereals.todas$kpamc)
table(cereals.todas$mfr, cereals.todas$kpamc)
table(cereals.todas$type, cereals.todas$kpamc)


#inserir coluna com o num. de cluster gerado pela funcao 
#hclust e pamk

cereals.todas$kpamc <- kk$pamobject$clustering
cereals.todas$hc <- cutree(hc.cereals, k=4)

#verifica a distribuicao dos clusters
table(cereals.todas$kpamc)
table(cereals.todas$hc)
table(cereals.todas$kpamc,cereals.todas$hc)

# comparando dendo e kmedoid
clust_stats <- cluster.stats(dist.cereals.todas, cereals.todas$kpamc, cereals.todas$hc) # Corrected Rand index 
clust_stats$corrected.rand # valor bem abaixo de um - muita divergencia entre os modelos.
clust_stats <- cluster.stats(dist.cereals.todas, cereals.todas$kpamc, ereals.todas$hc) # Corrected Rand index 
clust_stats$corrected.rand # valor bem abaixo de um - muita divergencia entre os modelos.


#b) considerando apenas quantitativas como drivers

#seleciona as variaveis drivers 
fcereals.drivers=fcereals[,4:11]
colnames(fcereals.drivers)

#padroniza as variaveis
fcereals.scale <- scale(fcereals.drivers)
head(fcereals.scale)

#calcular as distancias
fcereals.dist <- dist(fcereals.scale)

#fazer dendograma para ter uma ideia da quantidade de clusters
hc.fcereals=hclust(fcereals.dist, method = "ward.D2")
plot(hc.fcereals, hang=-1)

#destacar 3 clusters
hc.fcereals <- hclust (fcereals.dist, method = "ward.D2")
plot(hc.fcereals, hang =-1, main="Cereals")
rect.hclust(cereals.hc, k=3, border="red")

#definir a quantidade de cluster com Nbcluster
library(NbClust)
nb <- NbClust(data = fcereals.scale, diss = fcereals.dist, 
              distance = NULL, min.nc = 2, max.nc=12, 
              method = "ward.D2", index = "all")

#According to the majority rule, the best number of clusters is  3 

# rodar K-means com 3 clusters
kmn <- kmeans(fcereals.scale,3,nstart=25) 
kmn$size

#criar coluna com o num. de cada cluster na tabela fcereals
fcereals$cluster <- kmn$cluster  #por kmeans
fcereals$hc <- cutree(hc.fcereals,k=3) #por dendo

# grafico com a projecao do cluster
fviz_cluster(list(data = fcereals[,4:11], cluster = fcereals$cluster), show.clust.cent = F)

#marcas em cada cluster =>mostra quais cereais estao em cada cluster
fcereals$name[cereals.todas$kpamc==1]
fcereals$name[cereals.todas$kpamc==2]
fcereals$name[cereals.todas$kpamc==3]

#analises

colnames(fcereals)
par(mfrow=c(1,4))

#analise quantitaiva 
boxplot(fcereals$calories~fcereals$cluster, col=rainbow(4), main="calories")
boxplot(fcereals$protein~fcereals$cluster, col=rainbow(4), main="protein")
boxplot(fcereals$fat~fcereals$cluster, col=rainbow(4), main="fat")
boxplot(fcereals$sodium~fcereals$cluster, col=rainbow(4), main="sodium")

boxplot(fcereals$fiber~fcereals$cluster, col=rainbow(4), main="fiber")
boxplot(fcereals$carbo~fcereals$cluster, col=rainbow(4), main="carbo")
boxplot(fcereals$sugars~fcereals$cluster, col=rainbow(4), main="sugars")
boxplot(fcereals$rating~fcereals$cluster, col=rainbow(4), main="rating")

#analise qualitativa
table(fcereals$name, fcereals$cluster)
table(fcereals$mfr, fcereals$cluster)
table(fcereals$type, fcereals$cluster)

#verifica a distribuicao dos clusters
table(kmeans=fcereals$cluster)
table(dendo=fcereals$hc)
table(kmeans=fcereals$cluster,dendo=fcereals$hc)

# comparando dendo e kmeans
clust_stats <- cluster.stats(fcereals.dist, fcereals$cluster, fcereals$hc) # Corrected Rand index 
clust_stats$corrected.rand # valor proximo de um -- bom
