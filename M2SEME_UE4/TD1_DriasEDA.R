## Chemin vers l'espace de travail
## ici le dossier Data -- Pensez à bien indiquer cela avec RStudio 

setwd("../Data")

###########################################################
## Lecture et mise en forme des données drias
###########################################################

## Il faudra que vous ajustiez cela en fonction de vos données

driasfile <- "tasminAdjusttasmaxAdjustprtotAdjustevspsblpotAdjust_France_CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63_Historical_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20040101-20041231.txt"
mydrias<-read.csv(driasfile, header = FALSE, sep=",", skip=60) ## Attention l'argument skip peut varier en fonction des variables récupérées
head(mydrias)
tail(mydrias)

## Nom ce colonnes à adapter en fonction de vos données
colnames(mydrias) <- c("idpt","lat","lon","alti","date","Tn","Tx","RR","ETP")
head(mydrias)
dim(mydrias)

###############################################################
## Sauvegarde l'ensemble des données Drias au format RData
## une fois réalisé les données pourrons directement être chargé avec la fonction load
dim(mydrias)
save(mydrias, file="mydrias.RData")
#### Recharger les données au format RData se fait avec la fonction
load("mydrias.RData")


## Combien de points de grille ?
mydrias.pts <- unique(mydrias[,c("idpt","lon","lat","alti")])
str(mydrias.pts)
dim(mydrias.pts)
## Sauvegarde les points de grille au format csv -> pour récupération avec QGIS
write.csv(mydrias.pts,file="ptsdrias.csv",quote=FALSE,row.names=FALSE)

## Vérifier avec QGIS que les points du fichier ptsdrias.csv
## couvrent la Bourgogne

#####################################################################
##               Evaluation des données DRIAS
#####################################################################
## Calcul de la climatologie des données Drias au pas de temps mensuel
## pour comparaison avec les données observées des stations Météo France
## 
#####################################################################

library(xts) ## charge le package xts

## calcul des moyennes mensuelles toutes années confondus
## initalise les objets pour l'ensemble des résultats
resuTn <- c(); resuTx <- c(); resuRR <- c()

## première boucle sur les 497 points Drias
for(i in 1:nrow(mydrias.pts)){
    tmp <- subset(mydrias,mydrias$idpt==mydrias.pts[i,"idpt"])
    mydate <- as.Date(as.character(tmp$date),"%m/%d/%Y")
    tmpxts <- xts(tmp[,c("idpt","Tn","Tx","RR")], order.by=mydate)
    ## initialise objet vide pour les moyennes mensuelles
    rmoiTn <- c();rmoiTx <- c();rmoiRR <- c()
    ## Deuxième boucle sur les mois
    for(j in 1:12){
        moi <- tmpxts[.indexmon(tmpxts) %in% (j-1)]
        rmoiTn <- cbind(rmoiTn,colMeans(moi[,"Tn"]))
        rmoiTx <- cbind(rmoiTx,colMeans(moi[,"Tx"]))
        tmprr <- apply.monthly(moi[,"RR"],sum)
        rmoiRR <- cbind(rmoiRR,colMeans(tmprr))
        }
    resuTn <- rbind(resuTn,rmoiTn)
    resuTx <- rbind(resuTx,rmoiTx)
    resuRR <- rbind(resuRR,rmoiRR)
}
## Ajoute les noms des colonnes
colnames(resuTn) <- month.abb
colnames(resuTx) <- month.abb
colnames(resuRR) <- month.abb
##################################################################
dim(resuRR)
head(resuRR)

######################################################################
## Vérifier avec un graphique que les résultats pour la température
## sont cohérents
######################################################################

plot(resuTx[1,], type="l") ## Trace le cycle annuel des Tx pour la 1er point de grille


######################################################################
## Sauvegarde des données DRIAS -- moyennes mensuelles
#####################################################################

save(resuTn,resuTx,resuRR,mydrias.pts,
     file = "DriasData_mois.RData")

