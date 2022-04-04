########################################################################
#             TRABALHO FINAL  - RECONHECIMENTO DE PADRÕES              #
#                                                                      #
# Análise de Componentes Principais e Análise de Agrupamento de Dados: #
#   Os municípios do Brasil                                            #
#                                                                      #
#  WEIDMAM MILAGRES LELES - ISCTE-IUL -  19 DE DEZEMBRO DE 2020        #
#                                                                      #
########################################################################


######Packages######
#install.packages("psych")
library(psych)
#install.packages("corrplot")
library(corrplot)
#install.packages("xlsx")  
library(xlsx)
#install.packages('dplyr')
library(dplyr)
#install.packages('stringr')
library(stringr)
#install.packages('ggfortify')
library(ggfortify)
#install.packages('dplyr')
library(dplyr)
#install.packages('mclust')
library(mclust)
#install.packages('cluster')
library (cluster)
#install.packages("sf")
library(sf)
#install.packages("ggmap")
library(ggmap)
#install.packages("factoextra")
library("factoextra")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("scales")
library(scales)


######PCA######

#ABRIR CSV BRAZIL_CITIES
dataset_Brazil<- read.delim(file.choose(), header=TRUE, sep=";", dec=",")

#VERIFICAR AS CARACTERÃÂTICAS DO
str(dataset_Brazil)

dataset_Brazil$IDHM <- as.numeric(dataset_Brazil$IDHM)
dataset_Brazil$IDHM_Renda <- as.numeric(dataset_Brazil$IDHM_Renda)
dataset_Brazil$IDHM_Longevidade <- as.numeric(dataset_Brazil$IDHM_Longevidade)
dataset_Brazil$IDHM_Educacao <- as.numeric(dataset_Brazil$IDHM_Educacao)
dataset_Brazil$AREA <- str_replace_all(dataset_Brazil$AREA, "," , "") #remove todas as vÃÂ­rgulas como separador de milhar
dataset_Brazil$AREA <- as.numeric(dataset_Brazil$AREA)
dataset_Brazil$GVA_AGROPEC <- as.numeric(dataset_Brazil$GVA_AGROPEC)
dataset_Brazil$GVA_INDUSTRY <- as.numeric(dataset_Brazil$GVA_INDUSTRY)
dataset_Brazil$GVA_SERVICES <- as.numeric(dataset_Brazil$GVA_SERVICES)
dataset_Brazil$GVA_PUBLIC <- as.numeric(dataset_Brazil$GVA_PUBLIC)
dataset_Brazil$GVA_TOTAL <- as.numeric(dataset_Brazil$GVA_TOTAL)
dataset_Brazil$TAXES <- as.numeric(dataset_Brazil$TAXES)
dataset_Brazil$GDP <- as.numeric(dataset_Brazil$GDP)
dataset_Brazil$GDP_CAPITA <- as.numeric(dataset_Brazil$GDP_CAPITA)
dataset_Brazil$UBER[is.na(dataset_Brazil$UBER)] <- 0
dataset_Brazil$MAC[is.na(dataset_Brazil$MAC)] <- 0
dataset_Brazil$WAL.MART[is.na(dataset_Brazil$WAL.MART)] <- 0



input_brazil <- data.frame(CITY = dataset_Brazil$CITY, IDHM= dataset_Brazil$IDHM, 
                        GVA_CAPITA= round((dataset_Brazil$GVA_TOTAL/dataset_Brazil$IBGE_RES_POP),2), TAXES_CAPITA = round((dataset_Brazil$TAXES/dataset_Brazil$IBGE_RES_POP)*1000, 2),
                          GDP_CAPITA = dataset_Brazil$GDP_CAPITA, GAS_MUN_CAPITA = round((dataset_Brazil$MUN_EXPENDIT/dataset_Brazil$IBGE_RES_POP),2), COMP_CAPITA = round((dataset_Brazil$COMP_TOT/dataset_Brazil$IBGE_RES_POP),5), 
                          PER_POP_REGULAR = round((dataset_Brazil$IBGE_POP*100/dataset_Brazil$IBGE_RES_POP),2)

                          )



input_brazil <- na.omit(input_brazil)


#Faz matriz dos diagramas de correlação
pairs(input_brazil[2:8],pch = 20, lower.panel = NULL)

#Correlaçãoo de Pearson
correlation <- cor(input_brazil[2:8])
par(oma = c(0, 0, 0, -0)) # space around for text
corrplot.mixed(correlation,
               order = "hclust", #order of variables
               tl.pos = "lt", #text left + top
               upper = "ellipse")
#Correlation matrix
round(correlation, 3)


#Bartlett test and KMO
#Input is the correlation matrix
cortest.bartlett(correlation, n =100)
KMO(correlation)

#Faz scale dos dados
SCALED_DATA <- (data.frame(scale(input_brazil[2:7])))

#Cria soluÃÂ§ÃÂ£o para 6 componentes - Quantidade das variÃÂ¡veis de INPUT
pc6 <- principal(SCALED_DATA, nfactors=6, rotate="none", scores=TRUE)  
pc6
round(pc6$values,3)

#Faz Scree Plot
plot(pc6$values, type = "b", main = "Scree plot Para a base de dados do Brasil",
     xlab = "NÃÂºmero de Componentes Principais", ylab = "Autovalores")   

#Communality para a solucao com 6 componentes
round(pc6$communality,2)


#Cria Solucao para 2 componentes
pc2 <- principal(SCALED_DATA, nfactors=2, rotate="none")
pc2
round(pc2$communality,2)

#Remove GAS_MUN_CAPITA, pois tem Communality de 0,41
AJUSTED_SCALED_DATA <- data.frame(IDHM = SCALED_DATA$IDHM, GVA_CAPITA = SCALED_DATA$GVA_CAPITA, TAXES_CAPITA = SCALED_DATA$TAXES_CAPITA,
                                  GDP_CAPITA = SCALED_DATA$GDP_CAPITA,COMP_CAPITA = SCALED_DATA$COMP_CAPITA)

#Grava solucao para 2 componentes sem GAS_MUN_CAPITA
pc2 <- principal(AJUSTED_SCALED_DATA, nfactors=2, rotate="none")

#verifica outra vez communality, uma vez que removeu uma variavel
round(pc2$communality,2)

#Roda a solucao para 2 componentes e faz interpretacao dos dados                                  
pc2r <- principal(AJUSTED_SCALED_DATA, nfactors=2, rotate="varimax")
pc2r$loadings
round(pc2r$communality,2)


#Computa os scores para a solucao com duas componentes
pc2sc <- principal(AJUSTED_SCALED_DATA, nfactors=2, rotate="none", scores=TRUE)  
round(pc2sc$scores,3)
mean(pc2sc$scores[,1])
sd(pc2sc$scores[,1])


#Insere as pontuacoes em cada uma das variaveis
input_brazil$PRODUCAO <- pc2sc$scores[,1]
input_brazil$BEM_ESTAR <- pc2sc$scores[,2]

#verificar se a variancia e 1 
var(input_brazil$BEM_ESTAR)
var(input_brazil$PRODUCAO)

#CRIA CSV FINAL
FINAL <- merge(input_brazil, dataset_Brazil[,-c(10, 19, 20, 38, 39, 41, 42, 44, 45)], by= "CITY")
FINAL <- FINAL %>% distinct(PRODUCAO, BEM_ESTAR, .keep_all = TRUE)

#salva o csv final
write.csv2(FINAL, "New_Brazil.csv", row.names=FALSE) 

# PC1 vs PC2
plot(input_brazil$PRODUCAO, input_brazil$BEM_ESTAR, pch = ".",
     xlab="PRODUÇÃO", ylab="BEM_ESTAR", main = "Scores: PRODUÇÃO vs BEM ESTAR")
text(input_brazil$PRODUCAO, input_brazil$BEM_ESTAR-0.01, input_brazil[,1], cex=0.8) #(x,y,labels)



###### Identificacao da heterogeneidade na base de dados######



#ABRIR CSV NEW_BRAZIL
New_Brazil <- read.delim(file.choose(), header=TRUE, sep=";", dec=",")


dados <- data.frame(PRODUCAO =New_Brazil$PRODUCAO, BEM_ESTAR= New_Brazil$BEM_ESTAR)

# Standardized * Euclidian * Ward.D2 
demodist <- dist(scale(dados)) # compute distance
hclust_demo <- hclust(demodist,method='ward.D2')
plot(hclust_demo,label=FALSE,hang=-1)

# Cut the dendrogram
groups.k <- cutree(hclust_demo, k=3) # cut tree into 4 clusters
rect.hclust(hclust_demo, k=3, border="red") 
aggregate(dados,list(groups.k), mean)



# K-Means
std_dados <- scale(dados)
kmeans.k <- kmeans(std_dados, 3) 
# k = 3 rom hclust, nstart = initial random solutions
attributes(kmeans.k)  
kmeans.k$centers
CLUSTER <- kmeans.k$cluster
kmeans.k$size
table(groups.k,kmeans.k$cluster)
New_Brazil$CLUSTER <- kmeans.k$cluster


#Silhouette
demodist <- dist(scale(dados)) # compute distance
plot(silhouette(kmeans.k$cluster,demodist), border=NA, col = 2:4
    ) 

#Método da média da silhueta - utilizado para ajudar na decisão da quantidade de clusters a utilizar. 
fviz_nbclust(std_dados, kmeans, method = "silhouette")



#Plot cluster municípios 
o <- order(kmeans.k$cluster)
o1 <-data.frame(New_Brazil$CITY[o],kmeans.k$cluster[o])
plot(New_Brazil$PRODUCAO, New_Brazil$BEM_ESTAR, type="n", xlab="Produção", ylab="Bem Estar")
text(New_Brazil$PRODUCAO, y=New_Brazil$BEM_ESTAR,col=kmeans.k$cluster+2+4+3, labels=New_Brazil$CITY)



#representacao no mapa do BR 
CLUSTER1 <-  data.frame(New_Brazil[New_Brazil$CLUSTER == 1, 1:81])
CLUSTER2 <-  data.frame(New_Brazil[New_Brazil$CLUSTER == 2, 1:81])
CLUSTER3 <-  data.frame(New_Brazil[New_Brazil$CLUSTER == 3, 1:81])

#my_sf <- st_as_sf(CLUSTER1, coords = c("LONG", "LAT" ))
#my_sf <- st_as_sf(CLUSTER2, coords = c("LONG", "LAT" ))
#my_sf <- st_as_sf(CLUSTER3, coords = c("LONG", "LAT" ))
my_sf <- st_as_sf(New_Brazil, coords = c("LONG", "LAT" ))
                  
my_sf <- st_set_crs(my_sf, value  = 4326)
                  

                  
                  ggplot(my_sf) +
                    geom_sf(aes(color = as.factor(CLUSTER))) + 
                              scale_color_manual(
                                name = "Agrupamentos", 
                                values = c(2, 4, 3), 
                                labels = c("CLUSTER 1", "CLUSTER 2", "CLUSTER 3")
                                )
                  
#Processo de criacao de dummies
Dummy_IDHM_RENDA <- cut(New_Brazil$IDHM_Renda, 
breaks=c(-Inf, 0.555, 0.700, 0.800, Inf), 
labels=c("BAIXO","MÉDIO","ALTO", "MUITO ALTO"))

#Processo de criacao de dummies
Dummy_IDHM_EDUCACAO <- cut(New_Brazil$IDHM_Educacao, 
breaks=c(-Inf, 0.555, 0.700, 0.800, Inf), 
labels=c("BAIXO","MÉDIO","ALTO", "MUITO ALTO"))
                  
#Processo de criacao de dummies
Dummy_IDHM_Longevidade <- cut(New_Brazil$IDHM_Longevidade, 
breaks=c(-Inf, 0.555, 0.700, 0.800, Inf), 
labels=c("BAIXO","MÉDIO","ALTO", "MUITO ALTO"))
                  
#Processo de criacao de dummies
Dummy_COMP_CAPITA <- cut(New_Brazil$COMP_CAPITA, 
breaks=c(-Inf, 0.01016, 0.0189, 0.03089, Inf), 
labels=c("BAIXO","MÉDIO","ALTO", "MUITO ALTO"))

#Processo de criacao de dummies
New_Brazil$HAB_KM_2 <- New_Brazil$IBGE_RES_POP / New_Brazil$AREA
Dummy_HAB_KM_2 <- cut(New_Brazil$HAB_KM_2, 
breaks=c(-Inf, 20, 100, Inf), 
labels=c("BAIXO","MÉDIO","ALTO"))

#Processo de criacao de dummies
COMP_A <- cut(New_Brazil$COMP_A, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_B <- cut(New_Brazil$COMP_B, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("NÃ£o tem nenhuma empresa","AtÃ© 20 empresas","AtÃ© 100 empresas","AtÃ© 500 empresas", "Mais de 501 empresas"))

COMP_C <- cut(New_Brazil$COMP_C, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("NÃ£o tem nenhuma empresa","AtÃ© 20 empresas","AtÃ© 100 empresas","AtÃ© 500 empresas", "Mais de 501 empresas"))

COMP_D <- cut(New_Brazil$COMP_D, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_E <- cut(New_Brazil$COMP_E, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_F <- cut(New_Brazil$COMP_F, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_G <- cut(New_Brazil$COMP_G, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_H <- cut(New_Brazil$COMP_H, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("NÃ£o tem nenhuma empresa","AtÃ© 20 empresas","AtÃ© 100 empresas","AtÃ© 500 empresas", "Mais de 501 empresas"))
COMP_I <- cut(New_Brazil$COMP_I, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_J <- cut(New_Brazil$COMP_J, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_K <- cut(New_Brazil$COMP_K, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("NÃ£o tem nenhuma empresa","AtÃ© 20 empresas","AtÃ© 100 empresas","AtÃ© 500 empresas", "Mais de 501 empresas"))

COMP_L <- cut(New_Brazil$COMP_L, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_M <- cut(New_Brazil$COMP_M, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_N <- cut(New_Brazil$COMP_N, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_O <- cut(New_Brazil$COMP_O, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_P <- cut(New_Brazil$COMP_P, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_Q <- cut(New_Brazil$COMP_Q, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))

COMP_R <- cut(New_Brazil$COMP_R, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_S <- cut(New_Brazil$COMP_S, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
COMP_U <- cut(New_Brazil$COMP_U, 
              breaks=c(-Inf,1, 20, 100, 500, Inf), 
              labels=c("Não tem nenhuma empresa","Até 20 empresas","Até 100 empresas","Até 500 empresas", "Mais de 501 empresas"))
#populacao de cada cluster
POP_CLUSTER1 <-  data.frame(New_Brazil[New_Brazil$CLUSTER == 1, 13])
POP_CLUSTER1 <- colSums(POP_CLUSTER1)

POP_CLUSTER2 <-  data.frame(New_Brazil[New_Brazil$CLUSTER == 2, 13])
POP_CLUSTER2 <- colSums(POP_CLUSTER2)

POP_CLUSTER3 <-  data.frame(New_Brazil[New_Brazil$CLUSTER == 3, 13])
POP_CLUSTER3 <- colSums(POP_CLUSTER3)

TOTAL_POP <- POP_CLUSTER1+POP_CLUSTER2+POP_CLUSTER3
PER_POP_CLUSTER1 <- POP_CLUSTER1*100/TOTAL_POP
PER_POP_CLUSTER2 <- POP_CLUSTER2*100/TOTAL_POP
PER_POP_CLUSTER3 <-POP_CLUSTER3*100/TOTAL_POP

POP_comparacao <- data.frame(POP_CLUSTER1, POP_CLUSTER2, POP_CLUSTER3,PER_POP_CLUSTER1, PER_POP_CLUSTER2,PER_POP_CLUSTER3)


#tabelas cruzadas para camparação dos clusters 
GVA_CLUSTER <- as.data.frame.matrix(table(New_Brazil$GVA_MAIN,kmeans.k$cluster))
CAPITAL_CLUSTER <- as.data.frame.matrix(table(New_Brazil$CAPITAL,kmeans.k$cluster))
IDHM_RENDA_CLUSTER <- as.data.frame.matrix(table(Dummy_IDHM_RENDA,kmeans.k$cluster))
IDHM_EDUCACAO_CLUSTER <- as.data.frame.matrix(table(Dummy_IDHM_EDUCACAO,kmeans.k$cluster))
IDHM_Longevidade_CLUSTER <- as.data.frame.matrix(table(Dummy_IDHM_Longevidade,kmeans.k$cluster))
HAB_KM_2_CLUSTER <- as.data.frame.matrix(table(Dummy_HAB_KM_2,kmeans.k$cluster))
COMP_CAPITA_CLUSTER <- as.data.frame.matrix(table(Dummy_COMP_CAPITA,kmeans.k$cluster))
COMP_A_CLUSTER <- as.data.frame.matrix(table(COMP_A,kmeans.k$cluster))
COMP_B_CLUSTER <- as.data.frame.matrix(table(COMP_B,kmeans.k$cluster))
COMP_C_CLUSTER <- as.data.frame.matrix(table(COMP_C,kmeans.k$cluster))
COMP_D_CLUSTER <- as.data.frame.matrix(table(COMP_D,kmeans.k$cluster))
COMP_E_CLUSTER <- as.data.frame.matrix(table(COMP_E,kmeans.k$cluster))
COMP_F_CLUSTER <- as.data.frame.matrix(table(COMP_F,kmeans.k$cluster))
COMP_G_CLUSTER <- as.data.frame.matrix(table(COMP_G,kmeans.k$cluster))
COMP_H_CLUSTER <- as.data.frame.matrix(table(COMP_H,kmeans.k$cluster))
COMP_I_CLUSTER <- as.data.frame.matrix(table(COMP_I,kmeans.k$cluster))
COMP_J_CLUSTER <- as.data.frame.matrix(table(COMP_J,kmeans.k$cluster))
COMP_K_CLUSTER <- as.data.frame.matrix(table(COMP_K,kmeans.k$cluster))
COMP_L_CLUSTER <- as.data.frame.matrix(table(COMP_L,kmeans.k$cluster))
COMP_M_CLUSTER <- as.data.frame.matrix(table(COMP_M,kmeans.k$cluster))
COMP_N_CLUSTER <- as.data.frame.matrix(table(COMP_N,kmeans.k$cluster))
COMP_O_CLUSTER <- as.data.frame.matrix(table(COMP_O,kmeans.k$cluster))
COMP_P_CLUSTER <- as.data.frame.matrix(table(COMP_P,kmeans.k$cluster))
COMP_Q_CLUSTER <- as.data.frame.matrix(table(COMP_Q,kmeans.k$cluster))
COMP_R_CLUSTER <- as.data.frame.matrix(table(COMP_R,kmeans.k$cluster))
COMP_S_CLUSTER <- as.data.frame.matrix(table(COMP_S,kmeans.k$cluster))
COMP_U_CLUSTER <- as.data.frame.matrix(table(COMP_U,kmeans.k$cluster))

#Comparacao do GDP dos Clusters
GDP_TOTAL <-sum(New_Brazil$GDP)
GDP_CLUSTER1 <- sum(CLUSTER1$GDP)
GDP_CLUSTER2 <- sum(CLUSTER2$GDP)
GDP_CLUSTER3 <- sum(CLUSTER3$GDP)
PER_C1 <- GDP_CLUSTER1*100/GDP_TOTAL
PER_C2<- GDP_CLUSTER2*100/GDP_TOTAL
PER_C3 <-GDP_CLUSTER3*100/GDP_TOTAL

GDP_COMPRA <-data.frame(GDP_TOTAL, GDP_CLUSTER1, PER_C1,  GDP_CLUSTER2, PER_C2,GDP_CLUSTER3, PER_C3)
PER_GDP <- c(PER_C1, PER_C2, PER_C3)
PER_POP <- c(PER_POP_CLUSTER1, PER_POP_CLUSTER2,PER_POP_CLUSTER3)

#plot da comparacao PIB e populacao
plot(PER_GDP,type = "o",col = "red", xlab = "Agrupamentos", ylab = "Percentagem", 
     main = "PIB vs População", pch =17, xaxt="n")
lines(PER_POP, type = "o", col = "blue", pch =19)
axis(1, 1:3, labels=FALSE)
mtext(c("Cluster 1", "Cluster 2", "Cliuster 3"), 1, 1, at=1:3)

legend("topright", 
       legend = c("PIB", "População"), 
       col = c("red", "blue"), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


