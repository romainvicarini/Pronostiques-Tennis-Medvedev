# Installation des librairies install.packages
install.packages("tibble")
install.packages("tidyr")
install.packages("dplyr")
install.packages("glmnet")
install.packages("rpart")
install.packages("ggplot2")
library(tibble)
library(tidyr)
library(dplyr)
library(glmnet)
library(rpart)
library(ggplot2)

# Projet Prédiction Tennis

# Selection du fichier source
setwd(dir = "C:/Users/AlbanVicarini/OneDrive - Borakay Software/Documents/R")

# Importation des données

tennis <- read.table("C:/Users/AlbanVicarini/OneDrive - Borakay Software/Documents/R/atp_matches_2016-2022_Medvedev.csv", header = TRUE, sep = ";", dec = ".")

# Analyse des données de tennis
dim(tennis)
class (tennis)
summary(tennis)

# On enlève les données incompletes : tailles de joueurs ou classement

tennis = na.omit(tennis)
summary(tennis)
# on observe avec la fonction ci-dessus que l'on a supprimé 18 lignes ou l'on n'avait pas de valeur

# si on veut faire une régression logistique, il est important de transfromer les valeus qualitatives en facteurs (ex Y, surface,nom des opposants, etc. )
# on voit pas exemple que Y est considéré comme un entier dans la classe et non comme un facteur
class(tennis$Y)
tennis$Y <- as.factor(tennis$Y)
tennis$Opponents <- as.factor(tennis$Opponents)
tennis$O_hand <- as.factor(tennis$O_hand)

# on essaie de regarder la taille moyenne des jouers de tennis
mean(tennis$O_ht)

# on essaie de restituer un arbre de décision entre les victoires et les défaites
# on doit utiliser le package RPART et RPART plot

arbre <- rpart(Y~., data = tennis, cp=0.02)
print(arbre)
n=300
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(arbre, main="Match Medvedev")


summary(tennis)
tennis2<- tennis[,-4] # on supprime lla colonne du nom de l'opposant de Medvedev
tennis2
arbre2 <- rpart(Y~., data = tennis2, cp=0.02)
rpart.plot(arbre2, main="Match Medvedev sans les noms")
# le résultat est bien plus représentatif


# on veut maintenant prédire la victoire ou la défaite de Medvedev
# On va diviser notre fichier en 2: l'un pour l'apprentissage 80% et l'autre pour le test 20%

mod <- glm(Y~., data = tennis2, family = 'binomial')
set.seed(1234)
echant <- sample(1:312,60)
echant
tennis2A <- tennis2[-echant,]
tennis2T <- tennis2[echant,]
summary(tennis2T)
mod <- glm(Y~., data = tennis2A, family = 'binomial')
RES <- predict(mod, tennis2T)
RES <- as.data.frame(RES)
RES
monerreur <- function(x,y){mean((x-y)^2)}
apply(RES,2,monerreur,y=RES$Y)
dim(RES)
class(RES)
summary(mod)
# les étoiles permettent de voir quelles sont celles qui influent dans le résultat: 3 étoiles pour le classement et la date du tournoi et 1 étoile pour la surface.
# il n'y a qu'un match par jour ce qui explique 3 étoiles pour le résultat.


# visuel: on installe le package ggplot2
library("ggplot2")
ggplot(tennis2)+aes(x=tennis$O_rank, y=tennis2$O_age)+geom_point(bins=40)




