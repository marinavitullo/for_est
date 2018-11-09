sintesiFinale.Italia<-function(nome.regione,dsurface){

	#Creazione dati per file SintesiFinale.xls
	#foglio aboveground 
	estraiDati(x=dati.aboveground,nome.variabile="total.growing.stock")->dtotal.growing.stock
	estraiDati(x=dati.aboveground,nome.variabile="fire")->dharvest
	estraiDati(x=dati.aboveground,nome.variabile="harvest")->dfire
	#foglio ci
	estraiDati(x=dati.ci,nome.variabile="total.current.increment.carbon")->rsintesi.foglio1

	if(!verificaOrdine(x=dsurface,y=rsintesi.foglio1)) stop("Attenzione ordine differente dei due dataframe")
	if(!verificaOrdine(x=dtotal.growing.stock,y=rsintesi.foglio1)) stop("Attenzione ordine differente dei due dataframe")
	if(!verificaOrdine(x=dharvest,y=rsintesi.foglio1)) stop("Attenzione ordine differente dei due dataframe")
	if(!verificaOrdine(x=dfire,y=rsintesi.foglio1)) stop("Attenzione ordine differente dei due dataframe")

	#se dsurface ha stesso ordine possiamo togliere colonne dei fattori
	dsurface %>% dplyr::select(-c(1,2,3))->dsurface
	dharvest %>% dplyr::select(-c(1,2,3))->dharvest
	dfire %>% dplyr::select(-c(1,2,3))->dfire
	dtotal.growing.stock %>% dplyr::select(-c(1,2,3)) ->dtotal.growing.stock
	
	#calcolo di natural mortality
	if(!verificaOrdine(x=naturalMortalityp,y=rsintesi.foglio1)) stop("Attenzione ordine differente dei due dataframe")
	dtotal.growing.stock/(1-(naturalMortalityp$percent.natural.mortality/100))*
	(naturalMortalityp$percent.natural.mortality/100)->dnatural.mortality

	#calcolo drainGrazing
	if(!verificaOrdine(x=drainGrazingp,y=rsintesi.foglio1)) stop("Attenzione ordine differente dei due dataframe")

	dtotal.growing.stock/(1-(drainGrazingp$percent.drain.grazing/100))*
	(drainGrazingp$percent.drain.grazing/100)->ddrain.grazing

	if(!verificaOrdine(x=abovep,y=rsintesi.foglio1)) stop("Attenzione ordine differente dei due dataframe")
	(rsintesi.foglio1[,4:ncol(rsintesi.foglio1)]/1000)->rsintesi.foglio1
	#prodotto.bef
	if(!verificaOrdine(x=abovep,y=belowp)) stop("Attenzione ordine differente dei due dataframe")
	abovep$bef*belowp$bef->prodotto.bef

	#alla fine
	ncol(rsintesi.foglio1)->numero.colonne
	rsintesi.foglio2<-(rsintesi.foglio1/abovep$bef)*belowp$bef
	rsintesi.foglio3<-rsintesi.foglio1+rsintesi.foglio2

	lapply(1:ncol(rsintesi.foglio3),FUN=function(colonna,x=rsintesi.foglio3,y=dsurface){
		(x[,c(colonna)]/y[,c(colonna+1)])*y[,c(colonna)]
	})->rsintesi.foglio4.1

	as.data.frame(rsintesi.foglio4.1)->rsintesi.foglio4.1
	names(rsintesi.foglio4.1)<-names(rsintesi.foglio3)
	rsintesi.foglio4.2<-rsintesi.foglio3-rsintesi.foglio4.1

	parziale<-dharvest+dfire+ddrain.grazing+dnatural.mortality
	(parziale*abovep$bef*abovep$basic.density*CC2DM/1000)->rsintesi.foglio5
	(parziale*belowp$bef*belowp$basic.density*CC2DM/1000)->rsintesi.foglio6
	rsintesi.foglio7<- -(rsintesi.foglio5+rsintesi.foglio6)

	#per omogeneizzare le operazioni con quanto fatto prima togliamo prima colonna (1985) a rsintesi.foglio7
	rsintesi.foglio7<-rsintesi.foglio7[,-c(1)]

	lapply(1:ncol(rsintesi.foglio7),FUN=function(colonna,x=rsintesi.foglio7,y=dsurface){
		(x[,c(colonna)]/y[,c(colonna+1)])*y[,c(colonna)] 
	})->rsintesi.foglio8.1
	as.data.frame(rsintesi.foglio8.1)->rsintesi.foglio8.1
	names(rsintesi.foglio8.1)<-names(rsintesi.foglio3)
	rsintesi.foglio8.2<-rsintesi.foglio7-rsintesi.foglio8.1

	rsintesi.foglio9.1<-rsintesi.foglio4.1+rsintesi.foglio8.1
	rsintesi.foglio9.2<-rsintesi.foglio4.2+rsintesi.foglio8.2
	
	objects(pattern="rsintesi.+")->lista.rsintesi

	nomi.fogli<-c("AbovegroundCarbonCI","BelowgroundCarbonCI","Increase","LandRemainingForestIncrease",
	"LandConvertingForestIncrease","AbovegroundCarbonLoss","BelowgroundCarbonLoss","Decrease",
	"LandRemainingForestDecrease","LandConvertingForestDecrease","LandRemainingForestNetChange","LandConvertingForestNetChange") 	

	nome.file<-paste0("Sintesi_Finale_",nome.regione,".xlsx")
	wb <- createWorkbook("Guido")

  lapply(lista.rsintesi,FUN=function(nome.foglio){get(nome.foglio)})->lista.da.scrivere

	scriviExcel(my.wb=wb,as.list(nomi.fogli),lista.da.scrivere,prime.tre.colonne=colonne.fattori)->finale.wb
	#etichette
	lista.etichette<-list("Aboveground Current Increment of C","Belowground Current Increment of C",
	                      "Increase","Increase per Forest Remaining Forest","Increase for Land Converting to Forest",
	                      "Aboveground Loss of Carbon","Belowground Loss of Carbon","Decrease","Decrease per Forest Remaining Forest",
	                      "Decrease per Land Converting to Forest","Net Change per Forest Remaining Forest","Net Change for Land Converting to Forest")
	
  mapply(FUN=function(foglio,etichetta){
    writeData(finale.wb,foglio,x=paste0(etichetta,", kt=Gg C"),startCol = 1,startRow = 1)
    stile<-fgFRF.style
    if(length(grep("converting",tolower(etichetta)))) stile<-fgLCF.style
    addStyle(finale.wb,foglio,style=stile,rows=1:1,cols=1:1)
    
    #i fogli con Converting e Remaining vanno con gli anni dal 1986 al 1989 in grigio
    if(length(grep("converting",tolower(etichetta))) | length(grep("remaining",tolower(etichetta)))){
      addStyle(finale.wb,foglio,style=fgGRAY.style,rows=3:33,cols=4:7,stack=TRUE,gridExpand = TRUE)
    } #fine if per grigio
    
  },foglio=nomi.fogli,etichetta=lista.etichette)
	
  saveWorkbook(finale.wb,file=paste0("./",nome.regione,"/",nome.file),overwrite=TRUE)	
  
	#aggiungiamo le prime tre colonne
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio1,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio1
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio2,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio2
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio3,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio3
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio4.1,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio4.1
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio4.2,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio4.2
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio5,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio5
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio6,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio6
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio7,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio7
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio8.1,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio8.1
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio8.2,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio8.2
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio9.1,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio9.1
	unisciFattoriDati(fattori=colonne.fattori,x=rsintesi.foglio9.2,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->rsintesi.foglio9.2

	#oggetto unico da esportare
	list(rsintesi.foglio1,rsintesi.foglio2,rsintesi.foglio3,rsintesi.foglio4.1,rsintesi.foglio4.2,rsintesi.foglio5,rsintesi.foglio6,rsintesi.foglio7,rsintesi.foglio8.1,rsintesi.foglio8.2,rsintesi.foglio9.1,rsintesi.foglio9.2)->lista.fogli.rsintesi
	names(lista.fogli.rsintesi)<-nomi.fogli
 	
	return(lista.fogli.rsintesi)	
	
}#fine sintesiFinale 				


