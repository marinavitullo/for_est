#creazione del file SintesiItalia.xls
rm(list=objects())
library("stringr")
library("reshape2")
library("openxlsx")
library("magrittr")
library("readr")
library("dplyr")


#funzioni di utilit� per la cotruzione dei fogli di sintesi
source("funzioniSintesi.R")
#funzione per creare fogli sintesi
source("forestSintesi.R")
#funzione per creare foglio SintesiFinale
source("forestSintesiFinale.R")
#sintesi finale del F
source("forestSintesiFinaleDelF.R")
source("forestSintesiFinaleDelFPerPool.R")
source("forestSintesiFinaleSuddivisoPerCPool.R")
#parametri vari
source("forestParameters.R")
#funzioni per la scrittura dei fogli excel
source("forestExcel.R")
#scrittura del foglio surface
source("forestSurface.R")
numero.anni<<-(annoF-annoI)

#caricamento file parametri
read.csv(file="./parametri/abovegroundBiomass.csv",sep=";",head=TRUE,stringsAsFactors=FALSE)->>abovep
read.csv(file="./parametri/belowgroundBiomass.csv",sep=";",head=TRUE,stringsAsFactors=FALSE)->>belowp
read.csv(file="./parametri/naturalMortality.csv",sep=";",head=TRUE,stringsAsFactors=FALSE)->>naturalMortalityp
read.csv(file="./parametri/drainGrazing.csv",sep=";",head=TRUE,stringsAsFactors=FALSE)->>drainGrazingp
read.csv(file="./parametri/deforestation.csv",sep=";",head=TRUE,stringsAsFactors=FALSE)->>deforestationp

lista.regioni<-c(NOME.ITALIA,regioni)

#questo serve per memorizzare i dati che costituiranno il file surface_regioni che verrà scritto
#alla fine della procedura dopo aver elaborato tutte le regioni

#questa parte crea mediante lapply oggetti lista che via via verranno riempiti per creare il foglio
#surface e i fogli FM
c("lista.per.surface","lista.per.cPool","lista.per.fm3")->nomi.delle.liste

lapply(nomi.delle.liste,FUN=function(nome){
  
  vector(mode="list",length=length(regioni))->tmp
  names(tmp)<-regioni
  assign(nome,tmp,envir=.GlobalEnv)
  
})



#ciclo su nomi.regioni e Italia
lapply(lista.regioni,FUN=function(nome.regione){ 

  print(sprintf("Elaboro regione: %s",nome.regione))
	if(!file.exists(nome.regione)) stop(sprintf("Non trovo la directory %s",nome.regione))

	#carica file pools e foglio ci: il risultato di questo lapplay che segue sono delle variabili globali
  #una per ogni pool: dati.aboveground,dati.belowground etc etc. Questi "dati" contengono una colonna di testo
  #"nome variabile" e una colonna var3. La colonna "nome.variabile" ci dice che tipo di valore sta nella corrispondente
  #riga della colonna "var3". Questo è frutto del comando "melt".
	lapply(c(POOLS,"ci"),FUN=function(nome.pool){
	
    		paste0("./",nome.regione,"/",nome.regione,"_",nome.pool,".xlsx")->nome.file
    
    
    		#lettura file xlsx: il file "ci" contiene un anno in mento. Se le elaborzioni partono dal 1985
    	  #il file ci parte dal 1986
    		if(length(grep("ci",nome.pool))==1){
    			anno.inizio<-(annoI+1)
    		}else{
    			anno.inizio<-annoI
    		}
    
    		lapply(anno.inizio:annoF,FUN=function(yy){
    
    			indice<-(yy-anno.inizio)+1
    
    			tryCatch({
    				read.xlsx(xlsxFile=nome.file,sheet=indice,startRow=1,colNames=TRUE,
    				rowNames = FALSE, detectDates = FALSE,skipEmptyRows =TRUE,rows = NULL, 
    				cols = NULL, check.names = FALSE, namedRegion=NULL)->dati
    				#aggiungi colonna anno
    				dati$yy<-yy	
    			},error=function(e){
    				print(sprintf("Errore lettura file %s",nome.file))
    				print(sprintf("Anno inizio elaborazioni %s",annoI))
    				print(sprintf("Anno fine elaborazioni  %s",annoF))
    				print("Sono corretti?")
    				stop("")
    			}) #fine tryCatch
    
    			return(dati)
    		})->out.lettura.pools
    
    		#melt della lista
    		melt(out.lettura.pools,c("pools","management","yy","var3","inventory.tipology"),stringsAsFactors=FALSE)->mout
    		mout$variable<-as.character(mout$variable)
    		#avremo dati.aboveground..dati.belowground etc etc
    		assign(paste0("dati.",nome.pool),mout,env=.GlobalEnv)

	})#fine lapply lettura pools 


	#a questo punto possiamo estrarre dati file "dati.aboveground" etc etc tutte le variabili che ci interessano
	#utilizzando "estraiDati" che restituisce un dataframe per la variabile estratta. Ogni colonna è un anno.
	
	#dsurface compare nel corso di quasi tutto il programma. Conviene calcolarlo una sola volta
	estraiDati(x=dati.aboveground,nome.variabile="surface")->datiSurface

	#creazione dei file di sintesi: questa funzione vale sia per l'Italia che per le Regioni
	sintesi.ItaliaRegioni(nome.regione=nome.regione,dsurface=datiSurface)->foglioSintesi.out

	#qui elaborazioni per sintesi finale 
	sintesiFinale.Italia(nome.regione=nome.regione,dsurface=datiSurface)->foglioSintesiFinale
	#sintesifinale_delF; qui dobbiamo passare nome.regione
	sintesiFinaleDelF1(foglioSintesi=foglioSintesiFinale,nome.regione=nome.regione)->foglioSintesiFinaleDelF1
	#qui nome regione serve perchè: se Italia, foglioSintesi è una lista normale
	#se invece è una regione allora foglioSintesi è una lista di due liste
	sintesiFinaleDelF2(foglioSintesi=foglioSintesi.out,nome.regione=nome.regione,dsurface=datiSurface)->foglioSintesiFinaleDelF2

  #i dati di foglioSintesiFinaleDelF2 sono quelli di CFromLandConvertingToForest: 
  #quanto segue scrive proprio CFromLandConvertingToForest. Il foglio viene scritto mediante una funzione anonima
	#per non avere oggetti di troppo nell'environment
	
  (function(){
    nome.file<-paste0("C_From_Land_Converting_To_Forest_",nome.regione,".xlsx")
    nomeFoglio<-"dead_organic_matter_last"
    createWorkbook("guido")->wb
    scriviExcel(my.wb=wb,list(nomeFoglio),list(foglioSintesiFinaleDelF2),prime.tre.colonne=NULL)->final.wb
    #scrittura etichetta e stile
    mapply(FUN=function(etichetta,stile,riga){
      writeData(final.wb,nomeFoglio,x=etichetta,startCol=1,startRow=riga)    
      addStyle(final.wb,nomeFoglio,style=stile,rows=riga:riga,cols=1:1,gridExpand=TRUE,stack=TRUE)
    },etichetta=list("DOM per Forest Remaining Forest, kt=Gg C","DOM per Land Converting to Forest, kt=Gg C"),stile=list(fgFRF.style,fgLCF.style),riga=list(1,35))
    saveWorkbook(final.wb,paste0("./",nome.regione,"/",nome.file),overwrite=TRUE)

  })()
	
  #prende i dati di foglioSintesiFinaleDelF2 e produce i dati del tab DOM per Sintesi Finale del F. La funzione
  #fa operazioni simili a sintesiFinaleDleF1
  sintesiFinaleDelF1.variant(foglioSintesi=foglioSintesiFinaleDelF2,nome.regione=nome.regione)->foglioSintesiFinaleDelF2.variant

  scriviFoglioSintesiFinaleDelF(foglio1=foglioSintesiFinaleDelF1,foglio2=foglioSintesiFinaleDelF2.variant,nome.regione)

  #qui inizia sintesiFinaleSuddivisoPerCPool. Se stiamo elaborando una regione
  #dovremo passare anche foglioSurface (che invece non è richiesto per l'Italia)
  #foglio surface
  if(length(grep("italia",tolower(nome.regione)))!=1){
    
    #scriviFoglioSurface in realtà non lo scrive il foglio fa solo i calcoli
    scriviFoglioSurface(nome.regione=nome.regione,dsurface=datiSurface)->foglioSurface
    #lo assegnamo alla lista che serve per poi creare il foglioSurface finale con tutte le regioni
    lista.per.surface[[nome.regione]]<<-foglioSurface
    
    sintesiFinaleSuddivisoPerCPool(foglioSintesi=foglioSintesi.out,nome.regione=nome.regione,dsurface=datiSurface,foglioSurface=foglioSurface)->foglioSintesiFinaleSuddivisoPerCPool
    lista.per.cPool[[nome.regione]]<<-foglioSintesiFinaleSuddivisoPerCPool
    
  }else{

    sintesiFinaleSuddivisoPerCPool(foglioSintesi=foglioSintesi.out,nome.regione=nome.regione,dsurface=datiSurface)->foglioSintesiFinaleSuddivisoPerCPool

  }#fine if su nome.regione
  
  
  
	##questa funzione che segue serve per scrivere
	sintesiFinaleDelFPerCPool(foglio=foglioSintesiFinaleSuddivisoPerCPool,nome.regione=nome.regione)

})#fine lapply iniziale su lista.regioni


#qui scrivo fogli FM
##################


nome.file.fm<-paste0("FM_.xlsx")
nome.file.fm2<-paste0("FM_2.xlsx")
nome.file.fmdom<-paste0("FM_DOM_SOILS.xlsx")
nome.file.fm3<-paste0("FM_3.xlsx")

fm.wb<- createWorkbook("Guido")
fm2.wb<- createWorkbook("Guido")
fmdom.wb<- createWorkbook("Guido")
fm3.wb<- createWorkbook("Guido")

#se lista.regioni comprebde Italia devo escluderla
grep("Italia",lista.regioni)->pos.italia
if(length(pos.italia)) lista.regioni[-pos.italia]->lista.regioni

lapply(lista.regioni,FUN=function(quale.regione){
  
  lista.sheet.name<-list(paste0(quale.regione,"_increase"),paste0(quale.regione,"_decrease"))
  
  lista.increase.fm<-list(lista.per.cPool[[quale.regione]]$fm.increase.above,
                       lista.per.cPool[[quale.regione]]$fm.increase.below)
  
  lista.decrease.fm<-list(lista.per.cPool[[quale.regione]]$fm.decrease.above,
                       lista.per.cPool[[quale.regione]]$fm.decrease.below) 
  
  
  lista.dati.fm<-list(lista.increase.fm,lista.decrease.fm)

  #file FM-2
  lista.dati.fm2<-list(sommaPerVar3(lista.per.cPool[[quale.regione]]$fm.increase.above),
                       sommaPerVar3(lista.per.cPool[[quale.regione]]$fm.increase.below),
                       sommaPerVar3(lista.per.cPool[[quale.regione]]$fm.decrease.above),
                       sommaPerVar3(lista.per.cPool[[quale.regione]]$fm.decrease.below)) 
  #file FM-DOM
  lista.dati.fmdom<-list(sommaPerVar3(lista.per.cPool[[quale.regione]]$moreLitter.frf2),
                         sommaPerVar3(lista.per.cPool[[quale.regione]]$moreDeadwood.frf2))   
  

  #file FM-3
  lapply(1:4,FUN=function(ii){
    lista.dati.fm2[[ii]] %>% dplyr::select(-c(var3)) %>% colSums %>% unname ->somma
    if(ii==3 | ii==4){
      (-1*somma)
    }else{
      somma
    }
  })->lista.somma
  
  names(lista.somma)<-c("aboveG.biomass.gains","belowG.biomass.gains",
                        "aboveG.biomass.losses","belowG.biomass.losses")
  as_data_frame(lista.somma)->df.somma
  
  df.somma %<>% mutate(aboveG.biomass.netChange=aboveG.biomass.gains+aboveG.biomass.losses,
                      belowG.biomass.netChange=belowG.biomass.gains+belowG.biomass.losses)
  
  lapply(1:2,FUN=function(ii){
    lista.dati.fmdom[[ii]] %>% dplyr::select(-c(var3)) %>% colSums %>% unname ->somma
    c(0,somma)
  })->lista.somma.fmdom
  
  names(lista.somma.fmdom)<-c("litter","deadwood")
  as_data_frame(lista.somma.fmdom)->df.somma.fmdom
  
  bind_cols(df.somma,df.somma.fmdom)->df.finale
  df.finale %<>% mutate(total.Gg.C=aboveG.biomass.netChange+belowG.biomass.netChange+litter+deadwood,
                        total.Gg.CO2=total.Gg.C * CD2C,regione=quale.regione,year=1990:annoF) %>% dplyr::select(regione,year,everything())
  
  lista.per.fm3[[quale.regione]]<<-df.finale
  
  scriviExcel(my.wb=fm.wb,lista.sheet.name,lista.dati.fm,prime.tre.colonne=NULL,mtg=FALSE)->>fm.wb
  
  #################etichette per foglio FM
  lista.etichette<-list(
    list("Increase (kt=Gg C) per Forest Remaining Forest: aboveground","Increase (kt=Gg C) per Forest Remaining Forest: belowground"),
    list("Decrease (kt=Gg C) per Forest Remaining Forest: aboveground","Decrease (kt=Gg C) per Forest Remaining Forest: belowground")
  )
  
  mapply(FUN=function(foglio,etichetta){
    writeData(fm.wb,foglio,x=etichetta[[1]],startCol=1,startRow = 1)
    writeData(fm.wb,foglio,x=etichetta[[2]],startCol=1,startRow = 35) 
    addStyle(fm.wb,foglio,style=fgFRF.style,rows=1:1,cols=1:1)    
    addStyle(fm.wb,foglio,style=fgFRF.style,rows=35:35,cols=1:1)
  },foglio=lista.sheet.name,etichetta=lista.etichette)
  #################fine etichette FM

  ######################################################
  #foglio FM2: riscriviamo other com "rupiculous and riparian"
  lapply(lista.dati.fm2,FUN=function(data){
    
    grep("other",data$var3)->index
    if(length(index)){data$var3[index]<-"rupicolous and riparian forests"}
    
    data
    
  })->lista.dati.fm2
  
  scriviExcel(my.wb=fm2.wb,quale.regione,list(lista.dati.fm2),prime.tre.colonne=NULL,mtg=FALSE,shift.prime.tre.colonne=0,riducicIniziale = 3)->>fm2.wb   
  #etichette FM2
  lista.etichette<-list("AboveG..: Increase for FRF (left), Decrease for LCF (right)",
                        "BelowG.: Increase for FRF (left), Decrease for LCF (right)")
  
  mapply(FUN=function(foglio,etichetta,riga){
    writeData(fm2.wb,foglio,x=etichetta,startCol=1,startRow = riga)
    addStyle(fm2.wb,foglio,style=fgFRF.style,rows=riga:riga,cols=1:1)
  },foglio=quale.regione,etichetta=lista.etichette,riga=list(1,12))  
  
  #fine etichette FM2
  ######################################################
  
  scriviExcel(my.wb=fmdom.wb,quale.regione,list(lista.dati.fmdom),shift.prime.tre.colonne=0,riducicIniziale = 3)->>fmdom.wb    
  
  #etichette su fm-dom
  mapply(FUN=function(foglio,etichetta,riga){
    writeData(fmdom.wb,foglio,x=etichetta,startCol=1,startRow = riga)
    addStyle(fmdom.wb,foglio,style=fgLCF.style,rows=riga:riga,cols=1:1)
  },foglio=quale.regione,etichetta=list("Litter, kt=Gg C","Deadwood, kt=Gg C"),riga=list(1,12))  
  
  

})#fine lapply

saveWorkbook(fm.wb,paste0("./",nome.file.fm),overwrite=TRUE)
saveWorkbook(fm2.wb,paste0("./",nome.file.fm2),overwrite=TRUE)
saveWorkbook(fmdom.wb,paste0("./",nome.file.fmdom),overwrite=TRUE)

#the last but not the least?
do.call("bind_rows",lista.per.fm3)->fm3SintesiPoolRegioni
fm3SintesiPoolRegioni %>% dplyr::select(-regione) %>% group_by(year) %>% summarise_each(funs(sum(.,na.rm=TRUE)))->fm3PrimoFoglio
fm3PrimoFoglio %<>% mutate(livingBiomass.gains=aboveG.biomass.gains+belowG.biomass.gains,
                           livingBiomass.losses=aboveG.biomass.losses+belowG.biomass.losses,
                           livingBiomass.netChange.Gg.C=aboveG.biomass.netChange+belowG.biomass.netChange,
                           livingBiomass.netChange.Gg.CO2=livingBiomass.netChange.Gg.C * CD2C)

fm3SintesiPoolRegioni$year<-as.character(fm3SintesiPoolRegioni$year)

lista.sheet.name<-list(paste0("FM90-",annoF),"sintesi-pool-regioni")
lista.dati.fm3<-list(fm3PrimoFoglio,fm3SintesiPoolRegioni)

scriviExcel(my.wb=fm3.wb,lista.sheet.name,lista.dati.fm3,prime.tre.colonne=NULL,mtg=FALSE,scriviTotali=FALSE,convertiTotali=FALSE,shift.prime.tre.colonne=0)->>fm.wb
#etichetta
writeData(fm3.wb,paste0("FM90-",annoF),x="FM. Variables in 'Gg C'")
addStyle(fm3.wb,paste0("FM90-",annoF),style=fgLCF.style,rows=1:1,cols=1:1)

writeData(fm3.wb,"sintesi-pool-regioni",x="Forest Remaining Forest. Variables in 'Gg C'")
addStyle(fm3.wb,"sintesi-pool-regioni",style=fgLCF.style,rows=1:1,cols=1:1)
saveWorkbook(fm3.wb,paste0("./",nome.file.fm3),overwrite=TRUE)


#se si volesse scrivere il file Surface il contenuto si trova nella lista "lista.per.surface"

