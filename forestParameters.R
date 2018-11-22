############## Se TRUE attiva il debug del programma
guido.debug<-FALSE
#Durante il debug non vogliamo riscrivere (sovrascrivere) i risultati salvati come file RDS
#che servono a valutare i cambiamenti effettuati
guido.debug.write<-FALSE 
#A fine file compaiono le modifiche al programma quando vale TRUE guido.debug (vedi fine file)
##############


#######################################################
###anno inizio e fine delle elaborazioni
#######################################################
annoI<<-1985
annoF<<-2025

annoIntermedio<<-2010

regioni<-c("Abruzzo", "Alto_Adige", "Basilicata", "Calabria","Campania", "Emilia_Romagna", "Friuli_Venezia_Giulia", 
"Lazio", "Liguria", "Lombardia", "Marche", "Molise","Piemonte", "Puglia", "Sardegna", "Sicilia","Toscana", "Trentino","Umbria", "Valle_D_Aosta","Veneto")

#nomer per file output di Italia
NOME.ITALIA<<-"Italia"

#######################################################
#FATTORI DI CONVERSIONE
#######################################################
#cc2dm conversion factor: carbon content/dry matter
#cd2c conversion factor: carbon dioxide/carbonr
CC2DM<<-0.47
CD2C<<-44/12

#######################################################
#Fattori utilizzati nel file SintesiFinale del F: attualmente coprono il periodo dal 1990 al 2016
#######################################################

#Valori aggiornati 1990-2016 prova per calcolo For-est modificato per FRL
#i dati vengono letti tramite file csv


tryCatch({
  read.table(file="./serie_storiche/rapporto_area.csv",header=TRUE,sep=";")
},error=function(e){
  stop("Impossibile leggere file rapporto_area.csv nella directory serie")
})->datiRapporto

datiRapporto[["FL_FL_vs_total_FL"]]->>coefIncrease.sintesiFinale1
names(coefIncrease.sintesiFinale1)<-seq(1990,annoF,1)
rm(datiRapporto)

(1-coefIncrease.sintesiFinale1)->>coefIncrease.sintesiFinale2

#coeff Decrease
coefIncrease.sintesiFinale1->>coefDecrease.sintesiFinale1
(1-coefDecrease.sintesiFinale1)->>coefDecrease.sintesiFinale2

#coefficient plantations
c(0.39176178205149)->>coefIncrease.plantations1
c(0.60823821794851)->>coefIncrease.plantations2

coefIncrease.plantations1->>coefDecrease.plantations1
coefIncrease.plantations2->>coefDecrease.plantations2

#dom coefficients per SintesiFInale del F: foglio DOM. Aggiornamento 8 Gennaio 2016
coefIncrease.sintesiFinale1->>coefDOM.frf
(1-coefDOM.frf)->>coefDOM.lcf																						


#######################################################
#Pools
#######################################################

POOLS<<-c("aboveground","belowground","dead","litter","soil")	

#######################################################
#directory dei parametri
#######################################################
dir.parametri<-"parametri"
#nomi dei file dei parametri: possono chiamarsi come si preferisce purchè si distinguano con la scritta above/below/etc
#in caso contrario il programma restituisce un errore 
nomi.file.parametri<-list.files(path=paste0("./",dir.parametri),pattern="^.+csv$")

#######################################################
#directory delle serie...
#######################################################
dir.serie<-"serie_storiche"
nomi.serie<-c("harvests_input","harvests_proj","surfaces","fires")
nomi.file.serie<-paste(nomi.serie,".csv",sep="")

######inventory tipology names: NON MODIFICARE L'ORDINE DEI NOMI NEI VETTORI stands, coppices, plantations e other

stands<<-c("norway spruce","silver fir","larches","mountain pines","mediterranean pines","other conifers","european beech","turkey oak","other oaks","other broadleaves")

coppices<<-c("european beech","sweet chestnut","hornbeams","other oaks","turkey oak","evergreen oaks","other broadleaves","conifers")

plantations<<-c("eucaliptuses coppices","other broadleaves coppices","poplars stands","other broadleaves stands","conifers stands","others")

other<<-c("rupicolous forest","riparian forest","shrublands")

unstocked<<-c("gaps")

invTipology<<-list("stands"=stands,"coppices"=coppices,"plantations"=plantations,"other"=other,"unstocked"=unstocked)

nomi.var3<<-names(invTipology)

#data.frame con management var3 inventory.tipology
#utile quando si creano i file sintesi
data.frame(management=c(rep("productive",24),rep("protective",3),"NA")
,var3=c(rep(nomi.var3[1],10),rep(nomi.var3[2],8),rep(nomi.var3[3],6),rep(nomi.var3[4],3),rep(nomi.var3[5],1))
,inventory.tipology=as.character(unlist(invTipology)),stringsAsFactors=FALSE)->colonne.fattori


######################
#VOCI DA AZZERARE:
######################

#come vanno azzerati i valori?
VALORE.AZZERA<-0

#POPLAR STAND IN FIRE (COLONNA X)
c(nomi.var3[3],invTipology[[3]][3])->>azzera.poplar

#OTHER IN COLONNA AD
c(nomi.var3[4],invTipology[[4]][1],nomi.var3[4],invTipology[[4]][2],nomi.var3[4],invTipology[[4]][3])->>azzera.other

###TRONCARE
DIGITS<<-20

#SCRIVI FILE XLS COME OUTPUT?
SCRIVI.XLS<<-TRUE

if(guido.debug){
  SCRIVI.XLS<<-FALSE
  regioni<<-regioni[1:5] #5
}#fine guido.debug

