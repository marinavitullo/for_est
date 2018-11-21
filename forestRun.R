rm(list=objects())
library("dplyr")
library("stringr")
library("tidyr")
library("readr")
library("codetools")
library("compare")
source("forestParameters.R")
source("elabora.R")
#trasforma i warnings in errori e si blocca, più sicuro
options(warn=2,error=recover)

leggi.parametri(dir=dir.parametri,lista=nomi.file.parametri)
leggi.serie(dir=dir.serie,lista=nomi.file.serie)


#9 novembre
harvests<-harvests_proj

#24 gennaio 2017: lettura del vettore "coefIncrease.sintesiFinale1".
#Il file di input si trova nella cartella serie e viene letta all'interno del file
#forestParameters.R. Nelle versioni precedenti del programma i valori annuali
#erano riportati direttamente a mano nel file "forestParameteres.R"


#se annoF (l'anno di fine) manca in fires che fare? Calcolare tale valore come media dei
#cinque anni precedenti
ANNI.PRECEDENTI<-5
if(!length(which(fires$yy==annoF))){
  
  message("#########################################")				  
  message(sprintf("-->> ATTENZIONE: dato incendi per %s mancante",annoF))
  message(sprintf("-->> STIMO GLI INDICENDI COME MEDIA DEI %s ANNI PRECEDENTI",ANNI.PRECEDENTI))
  
  #estarggo gli ultimi cinque anni degli incendi
  subset(fires,yy>=annoF-6)->sfires
  #calcolo media sugli ultimi cinque anni
  aggregate(sfires[,c("stands","coppices")],by=list(regione=sfires$regione),FUN=mean,na.rm=TRUE)->stima.fires
  data.frame(yy=rep(annoF,nrow(stima.fires)),stands=stima.fires$stands,coppices=stima.fires$coppices,regione=stima.fires$regione)->df.stima.fires
  rbind(fires,df.stima.fires)->new.fires
  #aggiorniamo infine fires
  new.fires->>fires
  #non ne abbiamo più bisogno
  rm(new.fires)
  rm(sfires)
  rm(df.stima.fires)
  
}#fine if

#verifica anno inizio e fine: annoI/annoF nel file forestParameters
#verifica che le serie harvests/surfaces/fires inizino tutte nello stesso anno.
#Se le serie differiscono in anni differenti, il programma termina quando raggiunto il più piccolo dei tre anni fine

#verifica_anni_serie(annoI=annoI,annoF=annoF,fires=fires,harvests=harvests,surfaces=surfaces)

#lettura dei dati delle regioni in lista.nomi.regioni
lapply(regioni,FUN=function(nome.regione){
  
  nome.file<-paste0(nome.regione,".csv")
  
  if(!file.exists(nome.file)){
    message(sprintf("ATTENZIONE, FILE DI DATI NON TROVATO: %s.csv",nome.regione))
    NULL
  }else{
    read_delim(nome.file,delim=";",col_names=TRUE)    
  }

  
})->listaDatiRegioni #su file.exists

names(listaDatiRegioni)<-regioni  
Filter(Negate(is.null),listaDatiRegioni)->listaDatiRegioni

#Se non trovo neanche un file di dati, esco
if(!length(listaDatiRegioni)) stop("ATTENZIONE, NESSUN FILE DI DATI DISPONIBILE....ESCO!")

#nel caso in cui listaDatiRegioni contiene i dati di qualche regione verifichiamone la lunghezza.
#Se la lista ha una lunghezza pari a "regioni" vuol dire che i file per tutte le regioni sono presenti.
#Se la lista ha una lunghezza < lunghezza di regioni significa che mancano i file di qualche regione: dobbiamo
#allora aggiornare il vettore "regioni" con i nomi delle regioni effettivamente disponibili (names di listaDatiRegioni)    
if(length(listaDatiRegioni) != length(regioni)){
  
  stopifnot(length(listaDatiRegioni) < length(regioni)) #non posso avere più file che regioni
  print("ATTENZIONE: NUMERO FILE REGIONI MINORE DEL NUMERO EFFETTIVO DELLE REGIONI!")
  
  regioni<-names(listaDatiRegioni)
  
  #filtriamo i parametri in base alla regione. Ad esempio, fires: non teniamo in memoria tutto
  #il data.frame, lo filtriamo mantenendo i dati delle sole regioni presenti
  fires %<>% filter(regione %in% regioni)
  harvests %<>% filter(regione %in% regioni)
  deadwoodp %<>% filter(regione %in% regioni)
  functionp %<>% filter(regione %in% regioni)
  litterp %<>% filter(regione %in% regioni)
  soilp %<>% filter(regione %in% regioni)
  surfaces %<>% filter(regione %in% regioni)
  
}#fine if su length


elabora(lista.nomi.regioni=regioni,
        listaDatiRegioni=listaDatiRegioni,
        annoI=annoI,
        annoF=annoF,
        abovep=abovep,
        belowp=belowp,
        deadwoodp=deadwoodp,
        drainGrazingp=drainGrazingp,
        functionp=functionp,
        litterp=litterp,
        mortalityp=mortalityp,
        soilp=soilp,
        fires=fires,
        harvests=harvests,
        surfaces=surfaces,
        CC2DM=CC2DM,
        CD2C=CD2C)
