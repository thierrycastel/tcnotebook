## Chemin vers l'espace de travail
## ici le dossier Data -- Pensez à bien indiquer cela avec RStudio 
setwd("../Data")

############### Stations Météo France ###############################
####### lecture et chargement des données quotidiennes ################
#####################################################################

mydataMF <- read.csv("Q_21_previous-1950-2022_RR-T-Vent.csv", sep = ";")
class(mydataMF) ## Classe de l'objet
str(mydataMF) ## structure de l'objet avec le type des données contenues

## Exploration des données Météo-France##
########################################################
head(mydataMF) ## affichage les premières lignes des données
tail(mydataMF) ## affichage des dernières lignes des données
dim(mydataMF) ## dimension de l'objet

###########################################################
## Identification du nombre de stations Météo-France
###########################################################
MFposte <- unique(mydataMF[, c("NUM_POSTE", "NOM_USUEL", "LAT", "LON", "ALTI")])
head(MFposte) ## Affichage des premières lignes des données
dim(MFposte)

## Nettoyage des données i.e. suppression/conservation des certaines colonnes
## pour cela on s'aide du fichier "Q_descriptif_champs_RR-T-Vent.csv"
## récupéré sur le site https://meteo.data.gouv.fr/

mydataMF <- mydataMF[, c("NUM_POSTE", "AAAAMMJJ", "RR", "TN", "TX")]
dim(mydataMF)
str(mydataMF)

## Quel est le format de données de la colonne qui contient les dates ? ##
## création d'un vecteur date nécessaire pour créer un objet xts

mydates <- as.Date(as.character(mydataMF$AAAAMMJJ), "%Y%m%d")
## Chargement de la librairie xts
library(xts)

mydataMF.xts <- xts(mydataMF[, c("NUM_POSTE","TN", "TX", "RR")], order.by = mydates)
dim(mydataMF.xts)

## Ici on découpe la période d'intérêt
mydataMF.xts <- mydataMF.xts["1975/2005"]
dim(mydataMF.xts)

## Attention les données sont maintenant ordonnées selon les dates et non les stations
## vérifier cela

## Boucle pour calculer les moyennes mensuelles pour chaque station
## et pour les trois variables climatiques Tn, Tx, RR

## Création d'objets vide que l'on va remplir par le calcul
## 3 objets pour les moyennes et 3 pour sélectionner les stations
## avec suffisamment de données et peu/pas de NA

dfnatx <- c();dftx <- c()
dfnatn <- c();dftn <- c()
dfnarr <- c();dfrr <- c()

for (i in MFposte$NUM_POSTE) {
  print(i)## affiche le numéro de la station MF qui est traitée
  tmp <- mydataMF.xts[mydataMF.xts$NUM_POSTE %in% i]

  if (nrow(tmp) == 0) { ## si la station n'a pas de données pour la période 1975/2005
    dfnatx <- cbind(dfnatx, NA)
    dfnatn <- cbind(dfnatn, NA)
    dfnarr <- cbind(dfnarr, NA)
    dftx <- cbind(dftx, NA)
    dftn <- cbind(dftn, NA)
    dfrr <- cbind(dfrr, NA)
  } else { 
    ## On calcule le nombre de NA pour chaque variable et pour chaque mois
    tmpna <- apply.monthly(
        tmp[, c("TN", "TX", "RR")],
        function(x) apply(x, 2, function(x) length(which(is.na(x))))
      )
    ## On rempli les objets pour avoir le nombre de NA par station, par mois et par variable
    dfnatx <- cbind(dfnatx, tmpna$TX)
    dfnatn <- cbind(dfnatn, tmpna$TN)
    dfnarr <- cbind(dfnarr, tmpna$RR)
        
    ## On calcule les moyennes mensuelles pour les températures 
    tmpst <- apply.monthly(tmp[, c("TN", "TX")],
      FUN = function(x) colMeans(x, na.rm = TRUE)
    )
    ## On rempli les objets avec les résultats
    dftx <- cbind(dftx, tmpst$TX)
    dftn <- cbind(dftn, tmpst$TN)
    
    ## On calcule les cumuls mensuels
    tmpstr <- apply.monthly(tmp[, c("RR")],
      FUN = function(x) colSums(x, na.rm = TRUE)
    )
    dfrr <- cbind(dfrr, tmpstr$RR)
  }
}

## le calcul renvoie des NaN si que des NA dans la série
## On les remplace par des NA
dftn[dftn == "NaN"] <- NA; dfnatn[dfnatn == "NaN"] <- NA
dftx[dftx == "NaN"] <- NA; dfnatx[dfnatx == "NaN"] <- NA
dfrr[dfrr == "NaN"] <- NA; dfnarr[dfnarr == "NaN"] <- NA

class(dftx)
dim(dftx)

## nettoyage des données, on enlève les mois incomplets
##
indom <- which(as.numeric(format(index(dftx), format = "%d")) < 28)
dftx <- dftx[-indom, ]; dfnatx <- dfnatx[-indom, ]
indom <- which(as.numeric(format(index(dftn), format = "%d")) < 28)
dftn <- dftn[-indom, ]; dfnatn <- dfnatn[-indom, ]
indom <- which(as.numeric(format(index(dfrr), format = "%d")) < 28)
dfrr <- dfrr[-indom, ]; dfnarr <- dfnarr[-indom, ]
###################################################################

## calcul des moyennes mensuelles toutes années confondus
## initalise les objets pour l'ensemble des résultats

## Création d'une fonction pour écarter les stations & mois incomplets
##
MFclean <- function(myxts, myxtsna){

  ## identifie les mois avec plus de 5 jours sans données
  ## et ceux avec des NA
  ind <- which(myxtsna > 5 | is.na(myxtsna))

  myxts[ind] <- NA
  
  mwna <- length(which(is.na(myxts)))
  ## si mwna > 60 i.e. plus de 5 ans sans données (12*5)
  ## alors tout en NA pour écarter la station
  if(mwna>60){
    myxts[] <- NA
  }
 return(myxts)
}

## Boucle principale du calcul des moyennes climatologiques période 1975/2005
resuMFTn <- c()
resuMFTx <- c()
resuMFRR <- c()
for (i in 1:ncol(dftn)){## boucle su les station qui sont en colonne
  tmptn <- MFclean(dftn[, i], dfnatn[, i])
  tmptx <- MFclean(dftx[, i], dfnatx[, i])
  tmprr <- MFclean(dfrr[, i], dfnarr[, i])
  ## initialise objet vide pour les moyennes mensuelles
  rmoiTn <- c()
  rmoiTx <- c()
  rmoiRR <- c()
  for (j in 1:12) {## boucle pous faire la moyenne de tous les mois de janvier, puis février etc.
    moi <- tmptn[.indexmon(tmptn) %in% (j - 1)]
    rmoiTn <- cbind(rmoiTn, colMeans(moi, na.rm = TRUE))
    moi <- tmptx[.indexmon(tmptx) %in% (j - 1)]
    rmoiTx <- cbind(rmoiTx, colMeans(moi, na.rm = TRUE))
    moi <- tmprr[.indexmon(tmprr) %in% (j - 1)]
    rmoiRR <- cbind(rmoiRR, colMeans(moi, na.rm = TRUE))
  }
  resuMFTn <- rbind(resuMFTn, rmoiTn)
  resuMFTx <- rbind(resuMFTx, rmoiTx)
  resuMFRR <- rbind(resuMFRR, rmoiRR)
}

dim(resuMFTn)

## Change les noms des colonnes
colnames(resuMFTn) <- month.abb
colnames(resuMFTx) <- month.abb
colnames(resuMFRR) <- month.abb

## fonction pour identifier les stations sans valeurs 
## afin de les écarter
MFind <- function(mydf) {
  ind <- which(apply(mydf, 1,
                     function(x) length(which(is.na(x)))) > 11)
  return(as.vector(ind))
}

## selection les stations des données
resuMFTn[resuMFTn == "NaN"] <- NA
ind <- MFind(resuMFTn)
resuMFTn <- resuMFTn[-ind, ]
MFTnposte <- MFposte[-ind,]

resuMFTx[resuMFTx == "NaN"] <- NA
ind <- MFind(resuMFTx)
resuMFTx <- resuMFTx[-ind, ]
MFTxposte <- MFposte[-ind, ]

resuMFRR[resuMFRR == "NaN"] <- NA
ind <- MFind(resuMFRR)
resuMFRR <- resuMFRR[-ind, ]
MFRRposte <- MFposte[-ind, ]


dim(resuMFTn)
dim(resuMFRR)
## Qu'en déduisez-vous à partir des dimension des objets qui contiennent les résultats ?
                     
save(resuMFRR,resuMFTn,resuMFTx,
     MFRRposte,MFTnposte,MFTxposte,
     file = "MFData_mois.RData")
