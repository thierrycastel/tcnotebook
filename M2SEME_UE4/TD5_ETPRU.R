setwd("../Data")
load("mydrias.RData")
head(mydrias)
dim(mydrias)

str(mydrias)

## ajout des jours juliens au dataframe mydrias qui contient les données climat
## attention ici de bien vérifier comment vos dates sont organisées
mydrias$date <- as.Date(mydrias$date,"%m/%d/%Y")
mydrias$doi <- as.integer(format(mydrias$date,"%j"))

tail(mydrias)
str(mydrias)

## Vérification que Tx > Tn
indn <- which(mydrias$Tn>mydrias$Tx)
length(mydrias$Tx[indn])

## si length(mydrias$Tx[indn]) > 0 donc il y a des jours pour lesquels
## Tn > Tx ce qui est bien évidemment un problème et pose question
## A votre avis à quoi cela peut-il être du ?

## Charge la fonction de calcul de l'ETP Hargreaves
source("../FonctionsR/ETPHargreaves.R")

ls() ## liste des objets chargés. La fonction etht qui calcule l'ETP 
## est maintenant chargée et utilisable

etht ## détail sur la fonction de calcul de l'ETP

## La fonction etht calcule l'ETP au pas de temps journalier
## les arguments de la fonction sont : tn, tx, latitude en degré décimal et jour de l'année
## Nous utiliserons ici la fonction de R mapply qui permet d'appliquer itérativement une fontion
## nécessitant plusieurs arguments. Cette fonction évite donc l'utilisation de boucle for moins efficace 
## en particulier pour des gros dataframes
## utilisation de la fonction suppressWarnings afin d'éviter l'affichage des messages d'alerte

## Teste la fonction pour le premier jour
etht(tn = mydrias$Tn[1], tx = mydrias$Tx[1],latdeg = mydrias$lat[1],
     doy = mydrias$doi[1]) # test sur la 1ere valeur

## lance le calcul pour l'ensemble
ETP0 <- mapply(etht, tn=mydrias$Tn, tx=mydrias$Tx, 
                                latdeg=mydrias$lat, doy=mydrias$doi)


## check des résultats vérification de présence de NaN
## NaN problème dans le calcul lié au fait que pour certains jours
## tn > tx
length(which(is.nan(ETP0)))

## si présence de NaN -> remplace NaN par des NA
ETP0[is.nan(ETP0)] <- NA

## Charge la librairie zoo afin d'utiliser la fonction na.approx pour remplacer
## les NAs par une valeur approximée par une méthode d'interpolation linéaire
#library(zoo)
## Remplacement des NAs par interpolation linéaire
#ETP0 <- na.approx(ETP0)
#length(which(is.nan(ETP0)))
#length(ETP0)


## Vérification des résultats. Trace les valeurs d'ETP pour les 365 premiers jours
## Cela vous parait-il cohérent ?
plot(ETP0[1:365],type="l")

#### Ajout de l'ETP au dataframe mydrias
mydrias$ETP0 <- ETP0

## Explore et compare les données ETP (Drias) et ETP0 (calculé avec la fonction etht)
head(mydrias)
plot(mydrias$ETP[1:365],type="l")
lines(mydrias$ETP0[1:365], col="red")
cor(mydrias$ETP[1:365],mydrias$ETP0[1:365])


########################################################################
## Récupération des données de Kc (cf. script TD4_Kcjj.R))
####################################################################
load("Kc.RData")

## jointure des données de kc aux données driaspoints_drias_RU.shp
## on utilise ici le doi (jour de l'année) comme
## clef de jointure afin d'attribuer le bon Kc au bon jour
mydrias <- merge(mydrias, Kc, by.x = "doi", by.y = "doi")

head(mydrias)

## Calcul de l'ETM = ETP x Kc
mydrias$ETM <- mydrias$ETP * mydrias$kcl
mydrias$ETM0 <- mydrias$ETP0 * mydrias$kcl

head(mydrias)

## Sauvegarde de mydrias 
save(mydrias,file="mydrias.RData")
## A ce stade il ne reste plus qu'à récupérer la taille du réservoir i.e.RU
## La RU a été calculé pour chaque point de grille avec QGIS

######################################################################
## Récupération des données de RU pour chaque point (cf. traitement QGIS)
## lecture du shapefile contenant les valeur de RU par point de grille
## Nous allons utiliser le package maptools de R qui permet de lire 
## des données spatiales issues d'un SIG comme QGIS
######################################################################
#library(maptools)
library(sf)

ptdriasRU <- st_read('points_drias_RU.shp')
#str(ptdriasRU)

## Récupération des données de la table contenat les valeur de RU 
## ainsique les identifiants des points de grille
RU <- as.data.frame(ptdriasRU)
## Sélection de l'identifiant, de la valeur moyenne et de la valeur majoritaire
## faite une head(RU) pour mettre les bons noms des colonnes)

head(RU)
RU <- RU[,-ncol(RU)] ## supprime la dernière colonne
head(RU)

## Jointure des données de RU à mydrias
mydrias <- merge(mydrias, RU, by.x = "idpt", by.y = "idpt")

head(mydrias) ## les données sont-elles ordonnées selon idpt et la date ?

mydrias <- mydrias[order(mydrias$idpt, mydrias$date),]
str(mydrias)points_drias_RU.shp

## Sauvegarde de mydrias complet i.e. avec l'ensemble des données nécessaires
## pour faire le calcul du bilan hydrique au pas de temps journalier
save(mydrias,file="mydrias.RData")

## A ce stade nous avons toute les variables nécessaires pour calculer le bilan hydrique
##########################################################################################
