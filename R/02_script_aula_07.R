#dados j? existentes no R sendo carregados
data("anscombe")
#------------------Fun??es b?sicas para checar os dados--------------#
dim(anscombe) # dimensao dos dados, N de linhas e N de colunas
head(anscombe) # seis primeiras linhas dos dados
class(anscombe) # classe do objeto
str(anscombe) # estrutura do objeto
#--------------Selecionando coluna dos dados-----------------#
mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)
#------------Fun??o apply-----------#
# o mesmo calculo, agora apenas em 1 linha de comando
## media de todos os vetores x
apply(anscombe[,1:4], 2, mean) #aplica uma funcao a todas as linhas de um objeto
## media de todos os vetores y
apply(anscombe[,5:8], 2, mean)
#------------Descri??o estat?stica dos dados-------#
# vari?ncia dos dados
apply(anscombe, 2, var) # aplica a funcao var a todas as linhas do objeto
#Ententendo a correla??o e coeficiente de regress?o dos conjuntos x e y
# correla??o
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)
# coeficiente de regress?o
## primeiro criamos objetos com as regressoes dos quatro conjuntos
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)
## vamos criar agora uma lista com todos os modelos para facilitar o trabalho
mlist <- list(m1, m2, m3, m4)
## agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef) 
#Os dados t?m mesma m?dia, mesma vari?ncia, mesma correla??o e mesmo valores dos coeficientes (intercepto e inclina??o do modelo linear). Em que os dados s?o diferentes
anscombe
#Os valores parecem difentes. Mas qu?o diferentes?
# funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow = c(2, 2), #abre uma janela gr?fica com 2 linhas  e 2 colunas
    las = 1, # deixa as legendas dos eixos na vertical
    bty = "l") # tipo do box do grafico em L 
plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])
par(mfrow=c(1, 1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna

#-----------------------------------------------------------------------#

#Parte 2: Uma rotina (entre muitas poss?veis) de an?lise explorat?ria
#Padr?es morfol?gicos de esp?cies de Iris
#O conjunto de dados iris que voc?s j? utilizaram, foi coletado por Edgar Anderson e ficou famoso pelo trabalho de Ronald E. Fisher. Vamos carregar os dados no R.
#Ap?s carregar o conjunto de dados, use o comando ?iris para entender mais sobre o conjunto de dados. Vamos ent?o come?ar com as inspe??es b?sicas do arquivo.
read.csv('data/dados_iris.csv')
head(iris)
summary(iris) 
#Conhecendo as funções aggregate e tapply
#Quantas informações por espécie?
table(iris$Species)
#Qual a média das variáveis por espécie? Vamos usar as funções agreggate e tapply. As duas funções são semelhantes, o que muda são os argumentos e o formato de saída de cada uma delas.
# media do comprimento de sepala por especie
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
# a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
# ainda a mesma tarefa, com a mesma função mas em uma notação diferente
aggregate(Sepal.Length ~ Species, data = iris, mean)
#E agora vamos calcular o desvio padrão das variáveis.
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)
# criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol = 3, nrow = 4)
# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]
for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)  
}
medias
#Medidas de tendência central
#Média
vars <- iris[, -5]
apply(vars, 2, mean)
#Mediana: 50º quantil, de forma que divide os dados em duas metades
apply(vars, 2, median)
#Moda: valor mais frequente na amostra
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]      
#Desvio padrão: raiz quadrada da variância
sd01 <- apply(vars, 2, sd)
# outra forma:
sd02 <- apply(vars, 2, function(x) sqrt(var(x)))
sd01
sd02
sd01 == sd02
#Coeficiente de variação: medida relativa de desvio padrão
cv <- function(x){
  sd(x)/mean(x)*100
}
apply(vars, 2, cv)
# sumario de 5 numeros
apply(vars, 2, quantile)
# 5%, 50% e 95%
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))
#Intervalo (range)
# a funcao range nos retorna os valores minimo e maximo
apply(vars, 2, range)
# aplicando a funcao diff ao resultado do range, temos o valor desejado
# uma boa pratica é nunca sobrescrever um objeto já existente no R, por isso
# nunca nomeie um objeto com um nome já existente
my_range <- function(x){ 
  diff(range(x)) 
}
apply(vars, 2, my_range)
#Intervalo interquartil (IIQ)
apply(vars, 2, IQR)
#Correlação
cor(vars)
#Métodos gráficos
#gráfico de barras
barplot(table(iris$Species))
#Histograma
par(mfrow=c(2, 2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)
par(mfrow=c(1, 1))
#Agora para o comprimento da sépala das espécies de Iris, vamos ver o efeito do número de intervalos no histograma com o argumento breaks.
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)
par(mfrow=c(1, 1))
#Curva de densidade
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)
par(mfrow=c(1, 1))
#No R podemos ver a curva de densidade a usando a função por meio do plot da função density.
par(mfrow=c(1, 2))
# plot da curva de densidade
plot(density(iris$Sepal.Width))
# plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue") # note que agora estamos usando a funcao o comando add=TRUE
par(mfrow=c(1, 1))
#Vamos agora fazer os box-plots das variáveis contidas no objeto iris. Vamos começar com as variáveis gerais.
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)
#Agora vamos olhar para os valores por espécie.
boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)
#Você identifica outliers no conjunto de dados? Como podemos checar os outliers? Vamos usar a própria função boxplot para identificar os outliers.
boxplot(iris$Sepal.Width)
my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out
#qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)
# vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]
#No caso anterior consideramos outliers em relação à distribuição da variável para todas as espécies juntas. É razoável assumir que cada espécie tenha um padrão morfométrico distinto de modo que poderíamos identificar outliers de maneira espécie específica.
boxplot(Sepal.Width ~ Species, data = iris)
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out
# neste caso, queremos apenas os outliers da especie setosa
# vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 & 
       iris$Species == "setosa", 
     c("Sepal.Width", "Species")]
#Vamos olhar para os dados morfométricos das espécies de Iris e comparar com uma distribuição normal. No R, isto pode ser feito de forma visual com as funções qqnorm e qqline.
par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "setosa"], 
       main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"])
qqnorm(iris$Sepal.Length[iris$Species == "versicolor"], 
       main = "versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
qqnorm(iris$Sepal.Length[iris$Species == "virginica"], 
       main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])
par(mfrow=c(1,1))
#relação entre variáveis 
pairs(vars)
