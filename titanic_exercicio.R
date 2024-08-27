#regressão logística
#não dá para usar matriz de correlações

###############################
#PACOTES
library(dplyr)
install.packages(esquisse)
library(esquisse)
install.packages("caret")
library(caret)
############################

glimpse(titanic)

titanic$Sobreviveu <- factor(titanic$Sobreviveu)

glimpse(titanic)

titanic$Classe <- factor(titanic$Classe)

#Análise da associação entre sexo e sobrevivência
esquisser(titanic)

#Análise da associação entre tarifa e sobrevivência
titanic %>% group_by(Sobreviveu) %>% summarise(media = mean(Tarifa))

#Análise da associação entre idade e sobrevivência
titanic %>% group_by(Sobreviveu) %>% summarise(media = mean(Idade))

#Análise da associação entre Classe e sobrevivência
esquisser(titanic)

#####################################################################

indices <- sample(1:891, 712, replace = FALSE)

treino <- titanic[indices,]
teste <- titanic[-indices,]

modelo <- glm(formula = Sobreviveu ~., family = "binomial", data = treino)
summary(modelo)

modelo_2 <- glm(formula = Sobreviveu ~ Classe + Sexo + Idade + IrmEsp, family = "binomial", data = treino)
summary(modelo_2)

#Predições na amostra de teste

predicoes <- predict(modelo_2, newdata = teste, type = 'response')
predicoes <- ifelse(predicoes > 0.5, 1, 0)

confusionMatrix(factor(predicoes), factor(teste$Sobreviveu))

#Eu morreria?
eu <- data.frame(Classe = "3", Sexo = "female", Idade = 19, IrmEsp = 1, PaiFilho = 0, Tarifa = 15, Embarque = "C")

eu_morreria <- predict(modelo_2, newdata = eu, type = 'response')
ifelse(eu_morreria > 0.5, 'sobrevivi', 'morri')

