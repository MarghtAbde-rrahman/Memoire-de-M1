rm(list=ls())

## Import Data ##############################
data.selector <- "all"                     ##
price.Otl      <- 200                      ##
source("1_TwoStepEstim.R")                 ##
## ##########################################

library(ggplot2)
library(gridExtra)
library(forecast)
library(zoo)

#######################################################################################
# Plot pour visualiser les prix de l electricite avant/apres avor enelve les outliers #
#######################################################################################


# Préparation des données pour ggplot
data.frame_original <- data.frame(
  Price = as.vector(Y.mat.orig),
  Hour = rep(1:N, times = T),
  Day = rep(1:T, each = N)
)

data.frame_clean <- data.frame(
  Price = as.vector(Y.mat),
  Hour = rep(1:N, times = T),
  Day = rep(1:T, each = N)
)

# Création des graphiques
p1 <- ggplot(data.frame_original, aes(x = Hour, y = Price, group = Day, color = as.factor(Day))) +
  geom_line(alpha = 0.3) +  # Utiliser une transparence pour mieux voir les tendances
  labs(title = "Prix de l'Électricité Avant Nettoyage",
       x = "",
       y = "Prix",
       color = "Jour") +
  theme_minimal() +
  theme(legend.position = "none")  # Enlever la légende pour clarifier

p2 <- ggplot(data.frame_clean, aes(x = Hour, y = Price, group = Day, color = as.factor(Day))) +
  geom_line(alpha = 0.3) +
  labs(title = "Prix de l'Électricité Après Nettoyage",
       x = "",
       y = "Prix",
       color = "Jour") +
  theme_minimal() +
  theme(legend.position = "none") # Ajustement des abcisses

# Afficher les graphiques côte à côte
gridExtra::grid.arrange(p1, p2, ncol = 2)



#############################################################################
# Plot pour visualiser la saisonnalité, la tendance et les résidus des prix #
#############################################################################

# Vérifier s'il y a des NA
sum(is.na(data.frame_clean$Price))

# Gere les NA de'data.frame_clean$Price' (à expliquer)
data.frame_clean$Price <- na.approx(data.frame_clean$Price)

# Vérifier encore s'il reste des NA
sum(is.na(data.frame_clean$Price))

# Supposons que price est une série temporelle après imputation
decomp <- stl(ts(data.frame_clean$Price, frequency = 365), s.window = "periodic")

#Affichage la décomposition en termes de saisonnalité, tendance, résidus de la série temporelle des prix
plot(decomp) 


############################################################################################################
# Ajstement d'une regression linéaire pour estimer les fonctions demande-prix et affichage sur les données #
############################################################################################################

scl     <- 1
scl.axs <- 1.2
scl.lab <- 1.3



Y.tmp <- as.vector(Y.mat[,502])
U.tmp <- as.vector(U.mat[,502])
# Fit a linear model
model <- lm(Y.tmp ~ U.tmp)
# Plotting the observed data vs. the predicted data
abline(model, col="red")  # Adds the regression line
model$coefficients
plot(y=model$coefficients[1] + model$coefficients[2]*seq(min(U.mat[,502],na.rm=T), max(U.mat[,502],na.rm=T), 40),
     x=seq(min(U.mat[,502],na.rm=T), max(U.mat[,502],na.rm=T), 40),
     type="l", ylab="EUR/MWh", xlab="GW", cex.lab=scl.lab, cex.axis=scl.axs,
     xlim=range(c(
       min(U.mat[,502],na.rm=T),
       max(U.mat[,506],na.rm=T)-min(U.mat[,506],na.rm=T)+
         max(U.mat[,505],na.rm=T)-min(U.mat[,505],na.rm=T)+
         max(U.mat[,504],na.rm=T)-min(U.mat[,504],na.rm=T)+
         max(U.mat[,503],na.rm=T)-min(U.mat[,503],na.rm=T)+max(U.mat[,502],na.rm=T)+5*4*1000)),
     ylim=range(re.ord.Y.mat[,c(502:506)], na.rm=T),
     axes=FALSE, frame=TRUE,
     main="Estiamtion des Fonctions Prix-Demande (5 Jours ouvrés: 3-7 Mars 2008)"
)
Y.tmp <- as.vector(Y.mat[,502])
U.tmp <- as.vector(U.mat[,502])
# Ajustement d'un modele linéaire
model <- lm(Y.tmp ~ U.tmp)
# Plotting the observed data vs. the predicted data
abline(model, col="red")  # Adds the regression line
axis(2, at=seq(20,90,by=10),labels=seq(20,90,by=10), cex.axis=scl.axs)
points(y=Y.mat[,502],x=U.mat[,502])
## Mo
axis(1,at=c(
  seq(min(U.mat[,502],na.rm=T), max(U.mat[,502],na.rm=T), length=5)          
), labels=round(c(
  seq(min(U.mat[,502],na.rm=T), max(U.mat[,502],na.rm=T), length=5)
)/1000,digits=0), cex.axis=scl.axs, line=.3)
## Tu
axis(1,at=c(
  seq(min(U.mat[,503],na.rm=T), max(U.mat[,503],na.rm=T), length=5)-min(U.mat[,503],na.rm=T)+
    max(U.mat[,502],na.rm=T)+5*1000   
), labels=round(c(
  seq(min(U.mat[,503],na.rm=T), max(U.mat[,503],na.rm=T), length=5)
)/1000,digits=0), cex.axis=scl.axs, line=.3)
## We
axis(1,at=c(
  seq(min(U.mat[,504],na.rm=T), max(U.mat[,504],na.rm=T), length=5)-min(U.mat[,504],na.rm=T)+
    max(U.mat[,503],na.rm=T)-min(U.mat[,503],na.rm=T)+max(U.mat[,502],na.rm=T)+5*2*1000
), labels=round(c(
  seq(min(U.mat[,504],na.rm=T), max(U.mat[,504],na.rm=T), length=5)
)/1000,digits=0), cex.axis=scl.axs, line=.3)
## Th
axis(1,at=c(
  seq(min(U.mat[,505],na.rm=T), max(U.mat[,505],na.rm=T), length=5)-min(U.mat[,505],na.rm=T)+
    max(U.mat[,504],na.rm=T)-min(U.mat[,504],na.rm=T)+
    max(U.mat[,503],na.rm=T)-min(U.mat[,503],na.rm=T)+max(U.mat[,502],na.rm=T)+5*3*1000
), labels=round(c(
  seq(min(U.mat[,505],na.rm=T), max(U.mat[,505],na.rm=T), length=5) 
)/1000,digits=0), cex.axis=scl.axs, line=.3)
## Fr
axis(1,at=c(
  seq(min(U.mat[,506],na.rm=T), max(U.mat[,506],na.rm=T), length=5)-min(U.mat[,506],na.rm=T)+
    max(U.mat[,505],na.rm=T)-min(U.mat[,505],na.rm=T)+
    max(U.mat[,504],na.rm=T)-min(U.mat[,504],na.rm=T)+
    max(U.mat[,503],na.rm=T)-min(U.mat[,503],na.rm=T)+max(U.mat[,502],na.rm=T)+5*4*1000     
), labels=round(c(
  seq(min(U.mat[,506],na.rm=T), max(U.mat[,506],na.rm=T), length=5)
)/1000,digits=0), cex.axis=scl.axs, line=.3)

## Lignes de separations
lines(y=c(20.13141, 87.48051), x=rep(max(U.mat[,502],na.rm=T)+
                                       2.5*1000,2),lty=3)
lines(y=c(20.13141, 87.48051), x=rep(max(U.mat[,503],na.rm=T)-min(U.mat[,503],na.rm=T)+max(U.mat[,502],na.rm=T)+
                                       5*2*1000  -2.5*1000,2),lty=3)
lines(y=c(20.13141, 87.48051), x=rep(max(U.mat[,504],na.rm=T)-min(U.mat[,504],na.rm=T)+
                                       max(U.mat[,503],na.rm=T)-min(U.mat[,503],na.rm=T)+max(U.mat[,502],na.rm=T)+
                                       5*3*1000-2.5*1000,2),lty=3)
lines(y=c(20.13141, 87.48051), x=rep(max(U.mat[,505],na.rm=T)-min(U.mat[,505],na.rm=T)+
                                       max(U.mat[,504],na.rm=T)-min(U.mat[,504],na.rm=T)+
                                       max(U.mat[,503],na.rm=T)-min(U.mat[,503],na.rm=T)+max(U.mat[,502],na.rm=T)+
                                       5*4*1000-2.5*1000,2),lty=3)

## Tu
Y.tmp <- as.vector(Y.mat[,503])
U.tmp <- as.vector(U.mat[,503])
# Fit a linear model
model <- lm(Y.tmp ~ U.tmp)
# Plotting the observed data vs. the predicted data
b_new <- coef(model)[1] - 49  # Décaler l'intercept
a <- coef(model)[2]
abline(b_new, a, col="red")
points(y=Y.mat[,503],
       x=c(U.mat[,503]-min(U.mat[,503],na.rm=T)+
             max(U.mat[,502],na.rm=T)+5*1000))
## We
Y.tmp <- as.vector(Y.mat[,504])
U.tmp <- as.vector(U.mat[,504])
# Fit a linear model
model <- lm(Y.tmp ~ U.tmp)
# Plotting the observed data vs. the predicted data
b_new <- coef(model)[1] - 95  # Décaler l'intercept
a <- coef(model)[2]
abline(b_new, a, col="red")
points(y=Y.mat[,504],
       x=c(U.mat[,504]-min(U.mat[,504],na.rm=T)+
             max(U.mat[,503],na.rm=T)-min(U.mat[,503],na.rm=T)+max(U.mat[,502],na.rm=T)+5*2*1000))
## Th
# Fit a linear model
model <- lm(Y.tmp ~ U.tmp)
# Plotting the observed data vs. the predicted data
b_new <- coef(model)[1] - 150  # Décaler l'intercept
a <- coef(model)[2]
abline(b_new, a, col="red")
points(y=Y.mat[,505],
       x=c(U.mat[,505]-min(U.mat[,505],na.rm=T)+
             max(U.mat[,504],na.rm=T)-min(U.mat[,504],na.rm=T)+
             max(U.mat[,503],na.rm=T)-min(U.mat[,503],na.rm=T)+max(U.mat[,502],na.rm=T)+5*3*1000))
## Fr
# Fit a linear model
model <- lm(Y.tmp ~ U.tmp)
# Plotting the observed data vs. the predicted data
b_new <- coef(model)[1] - 195  # Décaler l'intercept
a <- coef(model)[2]
abline(b_new, a, col="red")
points(y=Y.mat[,506],
       x=c(U.mat[,506]-min(U.mat[,506],na.rm=T)+
             max(U.mat[,505],na.rm=T)-min(U.mat[,505],na.rm=T)+
             max(U.mat[,504],na.rm=T)-min(U.mat[,504],na.rm=T)+
             max(U.mat[,503],na.rm=T)-min(U.mat[,503],na.rm=T)+max(U.mat[,502],na.rm=T)+5*4*1000))


#######################################################################
# Affichage des K plus grandes valeurs propres et leur taux d'inertie #
#######################################################################

K <- 3
(vp_K <- sort(e.v, decreasing = TRUE)[1:K])

#Calcul de l'inetrie
(taux_inertie_1 <- vp_K[1]/sum(e.v[e.v>0]))
(taux_inertie_2 <- vp_K[2]/sum(e.v[e.v>0]))
(taux_inertie_3 <- vp_K[3]/sum(e.v[e.v>0]))
round((taux_inertie_1 + taux_inertie_2 + taux_inertie_3)*100, digits = 3)

#############################
# Calcul de l'AIC des bases #
#############################

(AIC.K1) 
(AIC.K2) 
(AIC.K3)

#######################
# Prédiction des prix #
#######################

T <- 638
sep <- 550
dim(U.mat)
train <- Y.mat[,1:sep]
test <- Y.mat[,(sep+1):T]
#Pourcentage de notre échantillons de test
round(dim(test)[2]/T, digits = 2)*100

coeeficient_beta     <- matrix(NA,K.tmp,T)
for(t in 1:T){
  tmp.lm     <- summary(lm(as.vector(X.tilde.mat[,t][!is.na(X.tilde.mat[,t])])~0 +
                             as.vector(phi.i.hat[[t]][,1])+
                             as.vector(phi.i.hat[[t]][,2])
  ))
  coeeficient_beta[,t] <- tmp.lm$coefficients[,1]
}







