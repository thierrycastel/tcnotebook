
######################################################################
setwd("../Data")
load("mydrias.RData")

## Charge la fonction du modèle de bilan hydrique à 2 réservoirs
source("../FonctionsR/FonctionBilanHydrique2R.R")

## Partition de la RU en 2 réservoirs
## Nom de colonne de la RU à adapter en fonction du nom que vous avez données pour la RU

mydrias$RU1 <- mydrias$X_mean * 0.4 ## réservoir superficiel
mydrias$RU2 <- mydrias$X_mean * 0.6 ## réservoir profond

## Création de l'objet idpt qui contient l'identifiant
## des points de grille Drias
idpt <- unique(mydrias$idpt)

## Création d'un liste vide qui va contenir les résultats
## la liste a autant d'éléments que de points de grille
resuBH <- vector("list", length(idpt))

### Calcul du BH : version séquentielle i.e. traite les points les uns à
## la suite des autres via une boucle for

ptm <- proc.time() ## start time
cpt <-1 ## initialisation d'un compteur pour le remplissage de la liste
for (i in idpt) { ## Boucle du calcul du BH pour chaque point de grille
  print(i)
  tmp <- subset(mydrias, idpt == i)
  ## construction du vecteur date. Odre jour, mois, années à adapter
  ## en fonction de l'ordre dans lequel sont vos dates
  #tmp$date <- as.Date(tmp$date, "%d/%m/%Y")
  ## ordonne les données par date
  tmp <- tmp[order(tmp$date), ]
  # application du modèle sur tout la période
  BH.tmp <- BH2R(RR = tmp$RR, ETM = tmp$ETM, RU1 = tmp$RU1[1], RU2 = tmp$RU2[1])
  BH.tmp <- cbind(tmp[, c("idpt", "date")], BH.tmp )
  ## remplissage de la liste avec les résultats
  resuBH[[cpt]] <- BH.tmp
  cpt <- cpt + 1
}

proc.time() - ptm ## end time

## sauvegarde de l'objet resuBH dasn un fichier RData
save(resuBH, file="resuBH_historic.RData")

## Examen des résultats pour le premier point de grille
BH.9378 <- resuBH[[1]]
head(BH.9378)
BH.1an <- BH.9378[1:365,]
## Calcul du bilan hydrique relatif 
BH.1an$BH <- (BH.1an$R1 + BH.1an$R2)/(BH.1an$R1[1] + BH.1an$R2[1])
## Trace l'évolution du BH relatif pour la première année 
## du premier point de grille 
plot(BH.1an$BH, type = "l")

## Partition ETM
plot(BH.1an$ETM, type="l")
lines(BH.1an$ETR1, col="blue")
lines(BH.1an$ETR2, col = "red")


##########################################################
##
##         Définir des indices synthétiques du BH
##
##########################################################


##########################################################
## Version parallèle du calcul de BH
lpts <- split(mydrias,f=as.factor(mydrias$idpt))

myfun <- function(x){
    #x$date <- as.Date(x$date,"%m/%d/%Y")
    x <- x[order(x$date),]
    resu <- cbind(x[,c("idpt","date")],
                  BH2R(RR=x$RR,ETM=x$ETM,RU1 = x$RU1[1], RU2 = x$RU2[1]))
    return(resu)
}

library(parallel)
cl <- makeCluster(12,type="PSOCK")
clusterExport(cl, list("BH2R"))
ptm <- proc.time()
resuparBH <- parLapply(cl = cl, lpts, myfun)
proc.time() - ptm

