
## Lecture des données de Kc
kc.moi <- read.csv('../Data/Tableau_Kc_Ebener1999.csv',header=TRUE)

## Date pour les valeurs de Kc mensuel
kc.dates <- as.Date(c("2016-1-1",paste("2016",kc.moi$mois,"15",sep="-"),"2016-12-31"))
## Passage au jour julien
kc.doi <- julian(kc.dates,origin = as.Date("2016-01-01"))
## kc.doi <- as.numeric(format(kc.dates, "%j"))
## Kc de la culture d'intérêt
kc.prairies <- c(kc.moi$Prairies[1],kc.moi$Prairies,kc.moi$Prairies[12])
kc.vigne <- c(kc.moi$Vigne[1],kc.moi$Vigne,kc.moi$Vigne[12])
kc.conifere <- c(kc.moi$Foret.de.conifères[1],kc.moi$Foret.de.conifères,kc.moi$Foret.de.conifères[12])
kc.mais <- c(kc.moi$Mais[1],kc.moi$Mais,kc.moi$Mais[12])

## dates vers jour julien ou day of the year pour une année type
mydates <- seq(as.Date("2016/1/1"), as.Date("2016/12/31"), by = "days")
doi <- julian(mydates,origin = as.Date("2016-01-01"))+1

##moi <- as.integer(format(mydates,"%m"))

## fonction pour l'interpolation de kc au pas de temps journalier
## selon 3 méthodes
kcinterp <- function(kc.doi=kc.doi,kc.os=NULL,doi=doi){
    kc.capprox <- approx(x=kc.doi,y=kc.os,xout=doi,method='constant')$y
    kc.lapprox <- approx(x=kc.doi,y=kc.os,xout=doi,method='linear')$y
    kc.spline <- spline(x=kc.doi,y=kc.os,xout=doi)$y
    return(cbind(doi,kc.capprox,kc.lapprox,kc.spline))
}

## Interpolation de Kc journalière
mykc <- kcinterp(kc.doi=kc.doi,kc.os=kc.vigne,doi=doi)

## création d'un dataframe avec jj et valeur de kc interpolée
Kc <- as.data.frame(mykc)
##remplace les NA
ind <- which(is.na(Kc),arr.ind=TRUE)
for(i in 1:nrow(ind)){Kc[ind[i,1],ind[i,2]] <- Kc[ind[i,1]-1,ind[i,2]]}
colnames(Kc) <- c("doi","kcc","kcl","kcs")

plot(Kc$doi, Kc$kcc, type = "l", lwd = 2)
lines(Kc$doi, Kc$kcl.lapprox, col = "red", lwd = 2)
lines(Kc$doi, Kc$kcs.spline, color= "blue", lwd = 2)
Kc$doi <- as.integer(Kc$doi)

## sauvegarde du Kc au pas de journalier
save(Kc, file="Kc.RData")

## trace evolution annuelle de Kc
#pdf("figKc.pdf", width = 6, height = 6)
plot(kcc~doi,data=Kc,type="l",col="red", xlab = "Jours", main="Kc Mais",
     ylab=list("Coefficient cultural",cex=1.2), ylim = c(min(Kc$kcs),max(Kc$kcs)))
lines(kcl~doi,data=Kc,type="l",col="darkgreen")
lines(kcs~doi,data=Kc,type="l",col="blue")
legend("topright", c("constant", "linéaire","spline"),col=c("red","darkgreen","blue"),lwd = 1)
#dev.off()
