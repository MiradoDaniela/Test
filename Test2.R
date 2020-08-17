####TEST DE RECRUTEMENT D'UN CONSULTANT JUNIOR EN SUIVI-EVALUATION POUR LE
####PROGRQMME NATIONAL BAREFOOT COLLEGE####


####1.PRETRAITEMENT DES DONNEES ENTREES#### 

####a. Ouverture de Cspro et exportation des donnees CSPRO (au format csdb) 
####dans un format (.txt) qui peut être lu avec R####

####b. Importation des donness dans R####
Baseline <- read.delim("E:/R_WWF/Daniela.txt")  #Entree des donnees#
View(Baseline) #Pour regarder les donnees#
names(Baseline) #Pour connaitre les classements des donnees#
attach(Baseline) #pour attacher les donnees#


####d. Format de la date en format POSIXct et affichage de la date en mois et annee####
install.packages("lubridate") #installation du package lubridate qui est necessaire pour le changement de la format de la date en forme POSIXct#
library(lubridate) #activation du package lubridate#
Baseline$DATE <- strptime(Baseline$DATE, format= "%d%m%Y") #pour faire appel aux donnees 'DATE'# 
format(Baseline$DATE, format= "%Y-%m-%d")#pour activer le format POSIXct#
str(Baseline) #pour verifier les changements dans les donnees#
Baseline$DATE <- format(as.Date(Baseline$DATE), "%m-%Y") #Affichage de la date en mois et année#
View(Baseline) #pour verifier les changements dans les donnees#


####c. Anonymisation de la colonne « NOM_ENQUETE_CHR »####
install.packages("digest") #installation package digest#
install.packages("data.table") #installation package data.table utilise aussi lors du changement en anonymat#
library(data.table) #Pour activer le package data.table#
library(digest) #Pour activer le package digest#
cols_to_mask <- c("NOM_ENQUETE_CHR") #Pour choisir la colonne à  masquer#

anonymize <- function(x, algo="crc32") {
  sapply(x, function(y) if(y == "" | is.na(y)) "" else digest(y, algo = algo))
} #pour changer en anonyme les noms des personnes enquetees#

setDT(Baseline) 
Baseline[, (cols_to_mask) := lapply(.SD, anonymize), .SDcols = cols_to_mask] #pour activer l'anonymat des personnes enquetes#
View(Baseline) #pour voir le changement dans les donnees#


####e. Calcul et representation graphique####
#Taux d'utilisation par type de source d'éclairage par ménage#

#Selection des colonnes a etudier#
Calcul <- select(.data = Baseline, Q1_SOURCE_ECLAIRAGE_SINGLE_FCT_1, Q1_SOURCE_ECLAIRAGE_SINGLE_FCT_2, NOM_ENQUETE_CHR) 
View(Calcul) #Pour afficher les donnees#

#Calcul du nombre de type de source d'eclairage utilise par menage#
summary(Calcul)
table(Q1_SOURCE_ECLAIRAGE_SINGLE_FCT_1)
table(Q1_SOURCE_ECLAIRAGE_SINGLE_FCT_2) 

#Representation graphiquement le taux d'utilisation (en pourcentage) par type de source d'éclairage par menage#
slices <- c(2, 24) #2 et 24 represente ici le nombre de menage utilisant le source d'eclairage B (bougie) et C (torche) respectivement#
lbls <- c("Bougie", "Torche")
pct <- round(slices/sum(slices)*100) 
lbls <- paste(lbls, pct)

#Labellisation#
lbls <- paste(lbls, "%", sep = " ")
pie(slices, labels = lbls,
    col = c("Red", "Blue"),
    main = "Taux d'utilisation de type de source d'éclairage
par ménage")
legend("topright", c("Bougie","Torche"), cex = 0.8, fill = c("Red", "Blue"))


#Calcul du coût moyen par type de source d'éclairage 
install.packages("plyr") #installation package plyr#
library(plyr) #Pour activer le package plyr#
ddply(Baseline, .(Q1_SOURCE_ECLAIRAGE_SINGLE_FCT_1), summarise, Q1_COUT_MENSUEL_CONSOMMATION_NUM_1=mean(Q1_COUT_MENSUEL_CONSOMMATION_NUM_1))
ddply(Baseline, .(Q1_SOURCE_ECLAIRAGE_SINGLE_FCT_2), summarise, Q1_COUT_MENSUEL_CONSOMMATION_NUM_2=mean(Q1_COUT_MENSUEL_CONSOMMATION_NUM_2))


#Calcul et affichage de la moyenne et l'écart_type de toutes les variables de type numérique#
#Affichage des colonnes avec les variables de type numerique#
sapply(Baseline, is.numeric)
#Calcul de la moyenne et ecart_type#
sapply(Baseline, function(Baseline)c(
  "Moyenne" = mean(Baseline, na.rm = TRUE),
  "Ecart_type" = sd(Baseline, na.rm = TRUE)
))   
