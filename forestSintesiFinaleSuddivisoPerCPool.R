sintesiFinaleSuddivisoPerCPool<-function(foglioSintesi,nome.regione=nome.regione,dsurface=NULL,foglioSurface=NULL){
  
  #se nome.regione è una regione (non Italia) foglioSintesi è una lista di liste. 
  #In questa funzione ci serve sia la prima lista che la seconda. La seconda ci serve solo nel caso delle regioni
  if(length(grep(nome.regione,"Italia"))!=1) { foglioSintesi[[2]]->moreFoglioSintesi; foglioSintesi[[1]]->foglioSintesi} 
  
  #se nome.regione==Italia foglioSintesi è già una lista semplice e non la devo maneggiare

  #estraiamo variabili da dati.aboveground che ci servono in diverse parti della funzione
  lista.variabili.above<-c("fire","harvest","total.growing.stock","bef","basic.density")
  
  lapply(lista.variabili.above,FUN=function(nome.variabile){
    
    estraiDati(x=dati.aboveground,nome.variabile=nome.variabile)
    
  })->lista.out.above.loss
  
  names(lista.out.above.loss)<-lista.variabili.above
  length(lista.out.above.loss)->nlista
  
  lista.out.above.loss[[nlista+1]]<-dsurface
  names(lista.out.above.loss)[nlista+1]<-"surface"

  
  
  #estraiamo variabili da dati.belowground che ci servono in diverse parti della funzione
  lista.variabili.below<-c("bef","basic.density")
  lapply(lista.variabili.below,FUN=function(nome.variabile){
    
    estraiDati(x=dati.belowground,nome.variabile=nome.variabile)
    
  })->lista.out.below.loss
  names(lista.out.below.loss)<-lista.variabili.below  
  
  #calcolo di drain and grazing e di natural mortality, servono per above e below carbon loss
  ((drainGrazingp$percent.drain.grazing/100)/(1-(drainGrazingp$percent.drain.grazing/100) ))->new.dg
  ((naturalMortalityp$percent.natural.mortality/100)/(1-(naturalMortalityp$percent.natural.mortality/100) ))->new.nm
  
  #calcolo rapporto tra superfici
  lapply((annoI+1):annoF,FUN=rapportoColonne,x=lista.out.above.loss[["surface"]],reverse=TRUE)->rapp.surface
  names(rapp.surface)<-((annoI+1):annoF)
  #colonne dal 90 in poi
  as_data_frame(rapp.surface) %>% dplyr::select(-starts_with("198"))->rapp.surface  
  
  #foglio aboveground carbon ci
  estraiDati(x=dati.ci,nome.variabile="total.current.increment.carbon")->dtotal.current.increment.curban
  dtotal.current.increment.curban %>% dplyr::select(1,2,3)->prime.tre.colonne
  dtotal.current.increment.curban %>% dplyr::select(-c(1,2,3)) %>% dplyr::select(-starts_with("198")) %>% mutate_each(funs(./1000))->foglio.above.ci
  
  #foglio belowground carbon ci
  lapply(1990:annoF,FUN=function(anno){
    
    as.character(anno)->yyy
    foglio.above.ci[,c(yyy)]/lista.out.above.loss[["bef"]][,c("1996")] * lista.out.below.loss[["bef"]][,c(yyy)]
    
  })->foglio.below.ci
  names(foglio.below.ci)<-seq(1990,annoF)
  foglio.below.ci %<>% as_data_frame()
  
  #foglio increase per land converted to forest aboveground
  lapply(1990:annoF,FUN=function(yyy){
    
    domMultiply(x=foglio.above.ci,y=rapp.surface[[as.character(yyy)]],var.x=as.character(yyy))
  })->increase.frf.above  

  names(increase.frf.above)<-seq(1990,annoF)
  increase.frf.above %<>% as_data_frame()  
  
  #increase for land converting to forest aboveground
  tbl_df(foglio.above.ci-increase.frf.above) -> increase.lcf.above
  
  #sempre foglio increase per land converted to forest
  lapply(1990:annoF,FUN=function(yyy){
    
    domMultiply(x=foglio.below.ci,y=rapp.surface[[as.character(yyy)]],var.x=as.character(yyy))
  })->increase.frf.below 
  
  names(increase.frf.below)<-seq(1990,annoF)
  increase.frf.below %<>% as_data_frame()  
  
  #increase for land converting to forest belowground
  tbl_df(foglio.below.ci-increase.frf.below) -> increase.lcf.below  


  #questa parte riguarda solo i file regionali:parti evidenziate in giallo
	if(length(grep("italia",tolower(nome.regione)))!=1){

		which(names(foglio.above.ci) %in% "2008")->colonna2008
		if(!length(colonna2008)) stop("errore colonna 2008") #colonna non trovata
		foglio.above.ci %>% dplyr::select(colonna2008:ncol(foglio.above.ci))->sub.foglio.above.ci

		#
		which(names(lista.out.above.loss[["surface"]]) %in% "2008")->colonna.ds.2008
		if(!length(colonna.ds.2008)) stop("errore colonna 2008") #colonna non trovata
		lista.out.above.loss[["surface"]] %>% dplyr::select(colonna.ds.2008:ncol(lista.out.above.loss[["surface"]]))->sub.surface

		#parte in giallo di Increase per Land Converted to Forest: in alto a sinistra
		(sub.foglio.above.ci/sub.surface * lista.out.above.loss[["surface"]]$"1989")->giallo1

		#parte in giallo in basso a sinistra
		(sub.foglio.above.ci-giallo1)->giallo2

		#parte in giallo in alto a destra		
		which(names(foglio.below.ci) %in% "2008")->colonna2008
		if(!length(colonna2008)) stop("errore colonna 2008") #colonna non trovata
		foglio.below.ci %>% dplyr::select(colonna2008:ncol(foglio.below.ci))->sub.foglio.below.ci

		(sub.foglio.below.ci/sub.surface * lista.out.above.loss[["surface"]]$"1989")->giallo3
			
		#parte in giallo in basso a destra					
		(sub.foglio.below.ci-giallo3)->giallo4


	}#fine if dedicato alle regioni
  ####################################	

  #foglio aboveground carbon loss
  mapply(FUN=function(anno){

    as.character(anno)->yyy  

    (lista.out.above.loss[["fire"]][,yyy]+lista.out.above.loss[["harvest"]][,yyy]+(lista.out.above.loss[["total.growing.stock"]][,yyy]*new.dg)+(lista.out.above.loss[["total.growing.stock"]][,yyy]*new.nm)) *
      lista.out.above.loss[["bef"]][,yyy]*lista.out.above.loss[["basic.density"]][,yyy]*CC2DM/1000
    
  },anno=1990:annoF,SIMPLIFY =FALSE)->foglio.above.loss
  names(foglio.above.loss)<-seq(1990,annoF)
  foglio.above.loss %<>% as_data_frame()

  #foglio below
  mapply(FUN=function(anno){
    
    as.character(anno)->yyy  
    
    (lista.out.above.loss[["fire"]][,yyy]+lista.out.above.loss[["harvest"]][,yyy]+(lista.out.above.loss[["total.growing.stock"]][,yyy]*new.dg)+(lista.out.above.loss[["total.growing.stock"]][,yyy]*new.nm)) *
      lista.out.below.loss[["bef"]][,yyy]*lista.out.below.loss[["basic.density"]][,yyy]*CC2DM/1000
    
  },anno=1990:annoF,SIMPLIFY =FALSE)->foglio.below.loss  

  names(foglio.below.loss)<-seq(1990,annoF)
  foglio.below.loss %<>% as_data_frame()


  #questa parte riguarda solo i file regionali:parti evidenziate in giallo
	if(length(grep("italia",tolower(nome.regione)))!=1){

		#############################
		#parte in giallo di Decrease per Land Converted to Forest: in alto a sinistra

		which(names(foglio.above.loss) %in% "2008")->colonna2008
		if(!length(colonna2008)) stop("errore colonna 2008") #colonna non trovata
		foglio.above.loss %>% dplyr::select(colonna2008:ncol(foglio.above.loss))->sub.foglio.above.loss

		#
		which(names(lista.out.above.loss[["surface"]]) %in% "2008")->colonna.ds.2008
		if(!length(colonna.ds.2008)) stop("errore colonna 2008") #colonna non trovata
		lista.out.above.loss[["surface"]] %>% dplyr::select(colonna.ds.2008:ncol(lista.out.above.loss[["surface"]]))->sub.surface

		#parte in giallo di Increase per Land Converted to Forest: in alto a sinistra
		(sub.foglio.above.loss/sub.surface * lista.out.above.loss[["surface"]]$"1989")->giallo5

		#parte in giallo in basso a sinistra
		(sub.foglio.above.loss-giallo5)->giallo6

		#parte in giallo in alto a destra		
		which(names(foglio.below.loss) %in% "2008")->colonna2008
		if(!length(colonna2008)) stop("errore colonna 2008") #colonna non trovata
		foglio.below.loss %>% dplyr::select(colonna2008:ncol(foglio.below.loss))->sub.foglio.below.loss

		(sub.foglio.below.loss/sub.surface * lista.out.above.loss[["surface"]]$"1989")->giallo7
			
		#parte in giallo in basso a destra					
		(sub.foglio.below.loss-giallo7)->giallo8

	}#fine if dedicato alle regioni
  ####################################	

  
  #foglio decrease per land converted to forest:   
  #sesto foglio, parte in alto
  lapply(1990:annoF,FUN=function(yyy){
    
    domMultiply(x=foglio.above.loss,y=rapp.surface[[as.character(yyy)]],var.x=as.character(yyy))
  })->decrease.frf.above
  
  names(decrease.frf.above)<-(1990:annoF)
  decrease.frf.above %<>% as_data_frame()  

  #sesto foglio parte in basso
  tbl_df(foglio.above.loss-decrease.frf.above)->decrease.lcf.above

  #sesto foglio parte in alto a destra 
  #sesto foglio, parte in alto
  lapply(1990:annoF,FUN=function(yyy){
    
    domMultiply(x=foglio.below.loss,y=rapp.surface[[as.character(yyy)]],var.x=as.character(yyy))
  })->decrease.frf.below
  
  names(decrease.frf.below)<-(1990:annoF)
  decrease.frf.below %<>% as_data_frame()  
  
  #sesto foglio parte in basso
  tbl_df(foglio.below.loss-decrease.frf.below)->decrease.lcf.below
  
  #settimo foglio: NET CHANGE
  tbl_df(increase.frf.above-decrease.frf.above)->net.change.frf.above
  tbl_df(increase.lcf.above-decrease.lcf.above)->net.change.lcf.above
  
  tbl_df(increase.frf.below-decrease.frf.below)->net.change.frf.below
  tbl_df(increase.lcf.below-decrease.lcf.below)->net.change.lcf.below
  
  #foglio dead organic matter last
  lapply(1990:annoF,FUN=function(yyy){
    domMultiply(x=foglioSintesi[[as.character(yyy)]],y=rapp.surface[[as.character(yyy)]],var.x="total.litter.carbon")
  })->litter.frf
  
  names(litter.frf)<-(1990:annoF)
  litter.frf %<>% as_data_frame()
  
  lapply(1990:annoF,FUN=function(yyy){
    foglioSintesi[[as.character(yyy)]] %>% extract2("total.litter.carbon") - litter.frf %>% extract2(as.character(yyy))
  })->litter.lcf  
  
  names(litter.lcf)<-(1990:annoF)
  litter.lcf %<>% as_data_frame()  
  
  lapply(1990:annoF,FUN=function(yyy){
    domMultiply(x=foglioSintesi[[as.character(yyy)]],y=rapp.surface[[as.character(yyy)]],var.x="total.dead.carbon")
  })->deadwood.frf  
  
  names(deadwood.frf)<-(1990:annoF)
  deadwood.frf %<>% as_data_frame()    
  
  lapply(1990:annoF,FUN=function(yyy){
    foglioSintesi[[as.character(yyy)]] %>% extract2("total.dead.carbon") - deadwood.frf %>% extract2(as.character(yyy))
  })->deadwood.lcf  
  
  names(deadwood.lcf)<-(1990:annoF)
  deadwood.lcf %<>% as_data_frame()    

  #Parte per regioni: foglio FM_DOM_soils. In questi calcoli utilizziamo parte dei dati
  #(litter.tC e soli.tC) forniti da foglioSurface
	if(length(grep("italia",tolower(nome.regione)))!=1){

		#startYear,finalYear: dal 2007 in poi il calcolo viene corretto facendo una media dei valori dal 2007 fino al
		#2013. Se si vogliono mantenere fissi il 2007 e il 2013, indipendentemente dal fatto che siano disponibili i dati (ad esempio)
		#del 2014, del 2015...porre finalYear=2013. Se invece si vuole utilizzare un valore medio che via via
		#si aggiorna tenendo conto dei nuovi dati diponibili porre finalYear=annoF (annoF è una variabile globale
		#dichiarata nel file parametri). Analogo discorso per startYear

		#z mi serve perche quando uso moreDataRegioni il totale colonna non è più quello di x come nel caso di 
		#forest remaining forest. z mi serve per passare il dataframe forest remaining forest e calcolarci
		#i totali colonna nel cado di land converting to forest. Nel caso che z non è nullo (lcf)
		#startYear e finalYear devono coincidere. Ovvero: per lcf non effettuo il calcolo della media
		#per riga a partire dal 2007 con cui correggo il risultato di questa funzione

		moreDataRegioni<-function(x,y,sottrarreAlNumeratore,z=NULL,startYear=2007,finalYear=annoF){

			#per evitare errori nel passaggio dei parametri della funzione aggiungiamo questo controllo
                        if(!is.null(z) & (startYear!=finalYear) ) stop("Per forest remianing forest z dovrebbe sempre essere null!")

			which(names(x) %in% as.character(startYear))->colStartYear
			which(names(x) %in% as.character(finalYear))->colFinalYear

			if(!length(colStartYear) | !length(colFinalYear)) stop("Errore: non ho trovato gli anni per il calcolo della media!Impossibile")

			if(is.null(z)) z<-x #se non passiamo z (nel caso forest remaining forest) i totali colonna
					    #vanno calcolati utilizzando il data.frame x. Nel caso land converting to forest
					    #i totali colonna sono ancora una volta i totali calcolati mediante forest remaining forest.
					    #Quindi non posso più utilizzare il data.frame x.		
	
			#correggiamo i dati con la media degli ultimi anni
			if(startYear!=finalYear){

				#calcolo della media per riga
				#dal 1990 al 2007 (colStartYear-1) faccio la somma delle colonne
				#dal 2007 faccio la somma della media calcolata tra startYear e finalYear. Nel caso
				#(che non succederà) startYear==1990 colStartYear-1 sarebbe 0..per evitare utilizziamo l'if
				#che segue
				ifelse(colStartYear-1,colStartYear-1,1)->colonnaDoveFinire

				#qui z coinciderà con x
				z %>% dplyr::select(seq(1,colonnaDoveFinire)) %>% colSums(.,na.rm=TRUE)->totaliColonnaDenominatore
				z %>% dplyr::select(seq(colStartYear,colFinalYear)) %>% rowMeans(.,na.rm=TRUE)->mediaRighe
				(colFinalYear-colStartYear+1)->quanteVolte
				totaliColonnaDenominatore<-c(totaliColonnaDenominatore,rep(sum(mediaRighe,na.rm=TRUE),quanteVolte))
				
				#ora aggiustiamo x: dal 2007 (startYear) in poi prendiamo la media per riga invece dei singoli valori
				for(yyy in startYear:finalYear){
				  eval(parse(text=paste0("x$'",yyy,"'<-mediaRighe")))
				}#fine ciclo for
				
			}else{
				z %>% colSums(.,na.rm=TRUE)->totaliColonnaDenominatore
			}#fine if su startYear e finalYear

			#y contiene i dati dal 1985, x invece già filtrato dal 1990
			y %>% dplyr::select(-matches("198")) %>% colSums(.,na.rm=TRUE)->totaliColonnaNumeratore

			#a totaliColonnaNUmeratore ora dobbiamo sottrarre "sottrarreAlNumeratore" (valori che provengono
			#da foglioSurface: secondo dataframe nella lista, colonne litter.tC e deadmass.tC)
			if( length(sottrarreAlNumeratore)!=length(totaliColonnaNumeratore) ) stop("Errore lunghezze diverse 0")

			totaliColonnaNumeratore<-totaliColonnaNumeratore-sottrarreAlNumeratore
			
			if( length(totaliColonnaDenominatore)!=length(totaliColonnaNumeratore) ) stop("Errore lunghezze diverse 1")
			if( ncol(x)!=length(totaliColonnaNumeratore) ) stop("Errore lunghezze diverse 2")

      #fondamentale togliere i nomi generati da colSumns per evitare errori
      #nella scrittura dei file excel: use.names=FALSE
      #unname(totaliColonnaDenominatore)->totaliColonnaDenominatore
      #unname(totaliColonnaNumeratore)->totaliColonnaNumeratore
      
			lapply(1:length(totaliColonnaNumeratore),FUN=function(ii){
				unlist(totaliColonnaNumeratore[ii]*x[,ii]/totaliColonnaDenominatore[ii],use.names=FALSE)
			})->risultato

			names(risultato)<-seq(1990,annoF)

			as_data_frame(risultato)->df.risultato			

			#calcoliamo la differenza tra colonne
			lapply(1991:annoF,differenzeColonne,x=df.risultato)->risultato2
			names(risultato2)<-1991:annoF
			as_data_frame(risultato2)->df.risultato2

			list(df.risultato,df.risultato2)

		}#fine moreDataRegioni

		#passando due anni differenti (startYear e finalYear) la funzione calcola la media tra startYear e finalYear
		#per correggere le stime dal startYear in poi. Di default startYear è 2007.
	
		#moreLitter.frf contiene due fogli (due liste). La seconda rappresenta la differenza tar le colonne
		#(foglio excel: deadorganicmatterlast di Sintesi per C pool, terzo foglio in alto a destra per litter)
		foglioSurface[[2]] %>% filter(year>=1990) ->foglioSurface1990
		foglioSurface1990 %>% dplyr::select(litter.tC) %>% extract2(1)->sottraiLitter

		cumsum(sottraiLitter)->sottraiLitter
		c(0,sottraiLitter[1:(length(sottraiLitter)-1)])->sottraiLitter
		moreDataRegioni(x=litter.frf,y=moreFoglioSintesi$litter,sottrarreAlNumeratore=sottraiLitter,
		                z=NULL,startYear=2007,finalYear=annoF)->moreLitter.frf

		#attenzione: qui (lcf) il calcolo della media non va fatto. A questo scopo dobbiamo passare alla funzione
		#i parametri startYear e finalYear == annoF.		
		moreDataRegioni(x=litter.lcf,y=moreFoglioSintesi$litter,sottrarreAlNumeratore=sottraiLitter,
		                z=litter.frf,startYear=annoF,finalYear=annoF)->moreLitter.lcf

		#analogalmente per deadwood
		foglioSurface1990 %>% dplyr::select(deadmass.tC) %>% extract2(1) ->sottraiDead
		cumsum(sottraiDead)->sottraiDead
		c(0,sottraiDead[1:(length(sottraiDead)-1)])->sottraiDead
		moreDataRegioni(x=deadwood.frf,y=moreFoglioSintesi$deadwood,sottrarreAlNumeratore=sottraiDead,
		                z=NULL,startYear=2007,finalYear=annoF)->moreDeadwood.frf
		#attenzione: qui il calcolo della media non va fatto. A questo scopo dobbiamo passare alla funzione
		#i parametri startYear e finalYear == annoF.		
		moreDataRegioni(x=deadwood.lcf,y=moreFoglioSintesi$deadwood,sottrarreAlNumeratore=sottraiDead,
		                z=deadwood.frf,startYear=annoF,finalYear=annoF)->moreDeadwood.lcf

	}#fine if dedicato alle regioni
  ####################################	


  
  #### SCRITTURA FILE EXCEL
  
  nome.file<-paste0("Sintesi_Finale_Suddiviso_per_C_pool_",nome.regione,".xlsx")
  wb <- createWorkbook("Guido")
  
  lista.sheet.name<-list("Aboveground.Carbon.CI","Belowground.Carbon.CI","Aboveground.Carbon.Loss","Belowground.Carbon.Loss")
  lista.dati<-list(foglio.above.ci,foglio.below.ci,foglio.above.loss,foglio.below.loss)
  
  scriviExcel(my.wb=wb,lista.sheet.name,lista.dati,prime.tre.colonne=prime.tre.colonne)->final.wb
  #scrittura etichette
  mapply(FUN=function(sheet,etichetta){
    writeData(final.wb,sheet,paste0(etichetta,", kt=Gg C"),startCol=1,startRow=1)
    addStyle(final.wb,sheet,style=fgFRF.style,rows=1:1,cols=1:1)
  },sheet=lista.sheet.name,
  etichetta=list("aboveground current increment of C","belowground current increment of C","aboveground loss of carbon","belowground loss of carbon"))
  
  #funzione per scrivere quattro fogli insieme
  lista.sheet.name<-list("LandConvertedForestIncrease","LandConvertedForestDecrease","Net.Change","Dead.Organic.Matter.Last")
  lista.dati<-list(
    list(increase.frf.above,increase.lcf.above,increase.frf.below,increase.lcf.below),
    list(decrease.frf.above,decrease.lcf.above,decrease.frf.below,decrease.lcf.below),
    list(net.change.frf.above,net.change.lcf.above,net.change.frf.below,net.change.lcf.below),
    list(litter.frf,litter.lcf,deadwood.frf,deadwood.lcf)
  )#fine lista
  
  scriviExcel(my.wb=final.wb,lista.sheet.name,lista.dati,prime.tre.colonne=prime.tre.colonne,riducicIniziale=3)->final.wb
  #etichette
  etichetta1<-"Increase (kt=Gg C) per Forest Remaining Forest: aboveground (sinistra) e belowground (destra)"
  etichetta2<-"Increase (kt=Gg C) per Land Converting to Forest: aboveground (sinistra) e belowground (destra)"
  etichetta3<-"Decrease (kt=Gg C) per Forest Remaining Forest: aboveground (sinistra) e belowground (destra)"
  etichetta4<-"Decrease (kt=Gg C) per Land Converting to Forest: aboveground (sinistra) e belowground (destra)"
  etichetta5<-"Net Change (kt=Gg C) Forest: aboveground (sinistra) e belowground (destra)"
  etichetta6<-"Net Change (kt=Gg C) per Land Converting to Forest: aboveground (sinistra) e belowground (destra)"
  etichetta7<-"Forest Remaining Forest: litter (sinistra) e deadwood (destra)"
  etichetta8<-"Land Converting to Forest: litter (sinistra) e deadwood (destra)"  
  mapply(FUN=function(nomeFoglio,etichetta){
    
    writeData(final.wb,nomeFoglio,etichetta[[1]],startCol=1,startRow=1)
    writeData(final.wb,nomeFoglio,etichetta[[2]],startCol=1,startRow=35)
    
    addStyle(final.wb,nomeFoglio,style=fgFRF.style,rows=1:1,cols=1:1)
    addStyle(final.wb,nomeFoglio,style=fgLCF.style,rows=35:35,cols=1:1)
    
  },nomeFoglio=lista.sheet.name,
  etichetta=list(list(etichetta1,etichetta2),list(etichetta3,etichetta4),list(etichetta5,etichetta6),list(etichetta7,etichetta8)))
  
  
  #parte per regioni: scrittura delle parti evidenziate in giallo
  if(length(grep("italia",tolower(nome.regione)))!=1){	
  	lista.sheet.name<-list("FM_LandConvertedForestIncrease","FM_LandConvertedForestDecrease")

  	#applica etichette FM
  	lapply(list(giallo1,giallo2,giallo3,giallo4),FUN=aggiungiFM)->lista.giallo1
  	lapply(list(giallo5,giallo6,giallo7,giallo8),FUN=aggiungiFM)->lista.giallo2
  	
  	lista.dati<-list(lista.giallo1,lista.giallo2)

  	scriviExcel(my.wb=final.wb,lista.sheet.name,lista.dati,prime.tre.colonne=prime.tre.colonne,riducicIniziale=3)->final.wb
  	
    #etichette
  	lista.etichette<-list(
  	  list("Increase (kt=Gg C) per Forest Remaining Forest: aboveground (sinistra) e belowground (destra)","Increase (kt=Gg C) per Land Converting to Forest: aboveground (sinistra) e belowground (destra)"),
  	  list("Decrease (kt=Gg C) per Forest Remaining Forest: aboveground (sinistra) e belowground (destra)","Decrease (kt=Gg C) per Land Converting to Forest: aboveground (sinistra) e belowground (destra)")
  	)
  	
  	lista.stili<-list(list(fgFRF.style,fgLCF.style),list(fgFRF.style,fgLCF.style))
  	
  	mapply(FUN=function(foglio,etichetta,stile){
  	  writeData(final.wb,foglio,x=etichetta[[1]],startCol=1,startRow=1)  	  
  	  writeData(final.wb,foglio,x=etichetta[[2]],startCol=1,startRow=35)
  	  addStyle(final.wb,foglio,style=stile[[1]],rows=1:1,cols=1:1)
  	  addStyle(final.wb,foglio,style=stile[[2]],rows=35:35,cols=1:1)  	  
  	},foglio=lista.sheet.name,etichetta=lista.etichette,stile=lista.stili)
  	
 	
	  #ancora dati per litter e deadwood nel caso delle regioni
  	lista.sheet.name<-list("DOM_Litter")
	  lista.dati<-list(list(moreLitter.frf[[1]],moreLitter.lcf[[1]],moreLitter.frf[[2]],moreLitter.lcf[[2]]))
  	scriviExcel(my.wb=final.wb,lista.sheet.name,lista.dati,prime.tre.colonne=prime.tre.colonne,riducicIniziale=3)->final.wb

 	  lista.sheet.name<-list("DOM_Deadwood")
	  lista.dati<-list(list(moreDeadwood.frf[[1]],moreDeadwood.lcf[[1]],moreDeadwood.frf[[2]],moreDeadwood.lcf[[2]]))

	  scriviExcel(my.wb=final.wb,lista.sheet.name,lista.dati,prime.tre.colonne=prime.tre.colonne,riducicIniziale=3)->final.wb

	  #########################################################
	  #etichette
	  lista.etichette<-list(
	    list("Litter for Forest Remaining Forest","Litter for Land Converting to Forest"),
	    list("Deadwood for Forest Remaining Forest","Deadwood for Land Converting to Forest")
	  )

	  lista.stili<-list(list(fgFRF.style,fgLCF.style),list(fgFRF.style,fgLCF.style))
	  
	  mapply(FUN=function(foglio,etichetta,stile){
	    writeData(final.wb,foglio,x=paste0(etichetta[[1]],", kt=Gg C"),startCol=1,startRow=1)  	  
	    writeData(final.wb,foglio,x=paste0(etichetta[[2]],", kt=Gg C"),startCol=1,startRow=35)
	    addStyle(final.wb,foglio,style=stile[[1]],rows=1:1,cols=1:1)
	    addStyle(final.wb,foglio,style=stile[[2]],rows=35:35,cols=1:1)  	  
	  },foglio=list("DOM_Litter","DOM_Deadwood"),etichetta=lista.etichette,stile=lista.stili)
	  
	  #########################################################
	  #fine etichette	  
	  
  }#fine parte per solo regioni

  saveWorkbook(final.wb,paste0("./",nome.regione,"/",nome.file),overwrite=TRUE)

  #return
  unisciFattoriDati(fattori=prime.tre.colonne,x=increase.frf.above,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->increase.frf.above
  unisciFattoriDati(fattori=prime.tre.colonne,x=increase.frf.below,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->increase.frf.below
  unisciFattoriDati(fattori=prime.tre.colonne,x=increase.lcf.above,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->increase.lcf.above
  unisciFattoriDati(fattori=prime.tre.colonne,x=increase.lcf.below,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->increase.lcf.below
  
  unisciFattoriDati(fattori=prime.tre.colonne,x=decrease.frf.above,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->decrease.frf.above
  unisciFattoriDati(fattori=prime.tre.colonne,x=decrease.frf.below,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->decrease.frf.below
  unisciFattoriDati(fattori=prime.tre.colonne,x=decrease.lcf.above,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->decrease.lcf.above
  unisciFattoriDati(fattori=prime.tre.colonne,x=decrease.lcf.below,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->decrease.lcf.below
  
  unisciFattoriDati(fattori=prime.tre.colonne,x=litter.frf,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->litter.frf
  unisciFattoriDati(fattori=prime.tre.colonne,x=deadwood.frf,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->deadwood.frf
  unisciFattoriDati(fattori=prime.tre.colonne,x=litter.lcf,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->litter.lcf
  unisciFattoriDati(fattori=prime.tre.colonne,x=deadwood.lcf,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->deadwood.lcf
  
    
  list(increase.frf.above,increase.frf.below,increase.lcf.above,increase.lcf.below,
       decrease.frf.above,decrease.frf.below,decrease.lcf.above,decrease.lcf.below,
       litter.frf,deadwood.frf,litter.lcf,deadwood.lcf)->lista.da.restituire
  names(lista.da.restituire)<-c("increase.frf.above","increase.frf.below","increase.lcf.above","increase.lcf.below",
                                "decrease.frf.above","decrease.frf.below","decrease.lcf.above","decrease.lcf.below",
                                "litter.frf","deadwood.frf","litter.lcf","deadwood.lcf")
  
  #per le regioni restituiamo anche le parti in giallo e le parti netchange
  if(length(grep("italia",tolower(nome.regione)))!=1){	
    
    
    creaFogliPerFM<-function(temp.frf,temp.lcf){
      
      lapply(1:(ncol(temp.lcf)-1),FUN=function(colonna){
        
        temp.lcf %>% dplyr::select(seq(1,colonna,by=1)) %>% rowSums(.,na.rm=TRUE)-> sommaColonna
        
      })->lista.sommeColonne
      names(lista.sommeColonne)<-names(temp.lcf)[2:ncol(temp.lcf)]
      as_data_frame(lista.sommeColonne)->df.somme
      data.frame("1990"=rep(0,nrow(df.somme)),check.names=FALSE)->col90
      bind_cols(col90,df.somme)->dlcf.somme
      
      #increase meno valori cumulati di lcf
      temp.frf-dlcf.somme->foglio.fm
      
      unisciFattoriDati(fattori=prime.tre.colonne,x=foglio.fm,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)
      
    }#fine funzione
    
    increase.lcf.above %>% dplyr::select(matches("^[[:digit:]]+$")) -> temp.lcf
    increase.frf.above %>% dplyr::select(matches("^[[:digit:]]+$")) -> temp.frf
    
    #questo è il primo foglio (increase) nel file FM: aboveground
    creaFogliPerFM(temp.frf=temp.frf,temp.lcf=temp.lcf)->lista.da.restituire[["fm.increase.above"]]
    
    #questo è il primo foglio (increase) nel file FM: belowground
    increase.lcf.below %>% dplyr::select(matches("^[[:digit:]]+$")) -> temp.lcf
    increase.frf.below %>% dplyr::select(matches("^[[:digit:]]+$")) -> temp.frf
    
    creaFogliPerFM(temp.frf=temp.frf,temp.lcf=temp.lcf)->lista.da.restituire[["fm.increase.below"]]
    
    #questo è il secondo foglio (decrease) nel file FM: aboveground
    decrease.lcf.above %>% dplyr::select(matches("^[[:digit:]]+$")) -> temp.lcf
    decrease.frf.above %>% dplyr::select(matches("^[[:digit:]]+$")) -> temp.frf
    
    creaFogliPerFM(temp.frf=temp.frf,temp.lcf=temp.lcf)->lista.da.restituire[["fm.decrease.above"]]
    
    #questo è il secondo foglio (decrease) nel file FM: belowground
    decrease.lcf.below %>% dplyr::select(matches("^[[:digit:]]+$")) -> temp.lcf
    decrease.frf.below %>% dplyr::select(matches("^[[:digit:]]+$")) -> temp.frf
    
    creaFogliPerFM(temp.frf=temp.frf,temp.lcf=temp.lcf)->lista.da.restituire[["fm.decrease.below"]]
    
    unisciFattoriDati(fattori=prime.tre.colonne,x=net.change.frf.above,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->net.change.frf.above
    unisciFattoriDati(fattori=prime.tre.colonne,x=net.change.frf.below,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->net.change.frf.below
    unisciFattoriDati(fattori=prime.tre.colonne,x=net.change.lcf.above,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->net.change.lcf.above
    unisciFattoriDati(fattori=prime.tre.colonne,x=net.change.lcf.below,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->net.change.lcf.below

    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo1,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallo.increase.frf.above
    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo2,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallo.increase.lcf.above    
    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo3,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallo.increase.frf.below    
    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo4,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallo.increase.lcf.below    

    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo5,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallo.decrease.frf.above
    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo6,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallo.decrease.lcf.above    
    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo7,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallo.decrease.frf.below    
    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo8,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallo.decrease.lcf.below    

    giallo1 - giallo5-> giallo1.net
    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo1.net,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallonet.decrease.frf.above
    
    giallo2 - giallo6-> giallo2.net
    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo2.net,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallonet.decrease.lcf.above
    
    giallo3 - giallo7-> giallo3.net
    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo3.net,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallonet.decrease.frf.below
    
    giallo4 - giallo8-> giallo4.net
    unisciFattoriDati(fattori=prime.tre.colonne,x=giallo4.net,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->giallonet.decrease.lcf.below
    
    #questi servono per il foglio FM-DOm-SOIL
    unisciFattoriDati(fattori=prime.tre.colonne,x=moreLitter.frf[[1]],aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->ml.frf1
    unisciFattoriDati(fattori=prime.tre.colonne,x=moreLitter.frf[[2]],aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->ml.frf2
    unisciFattoriDati(fattori=prime.tre.colonne,x=moreLitter.lcf[[1]],aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->ml.lcf1
    unisciFattoriDati(fattori=prime.tre.colonne,x=moreLitter.lcf[[2]],aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->ml.lcf2
    
    unisciFattoriDati(fattori=prime.tre.colonne,x=moreDeadwood.frf[[1]],aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->md.frf1
    unisciFattoriDati(fattori=prime.tre.colonne,x=moreDeadwood.frf[[2]],aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->md.frf2
    unisciFattoriDati(fattori=prime.tre.colonne,x=moreDeadwood.lcf[[1]],aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->md.lcf1
    unisciFattoriDati(fattori=prime.tre.colonne,x=moreDeadwood.lcf[[2]],aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=TRUE)->md.lcf2
    
        
    length(lista.da.restituire)->len.lista.da.restituire
    if(!len.lista.da.restituire) stop("Lunghezza lista.da.restituire nulla: impossibile!!")
    
    lista.da.restituire[["net.change.frf.above"]]<-net.change.frf.above
    lista.da.restituire[["net.change.lcf.above"]]<-net.change.lcf.above
    lista.da.restituire[["net.change.frf.below"]]<-net.change.frf.below
    lista.da.restituire[["net.change.lcf.below"]]<-net.change.lcf.below
    
    lista.da.restituire[["giallo.increase.frf.above"]]<-giallo.increase.frf.above
    lista.da.restituire[["giallo.increase.lcf.above"]]<-giallo.increase.lcf.above
    lista.da.restituire[["giallo.increase.frf.below"]]<-giallo.increase.frf.below
    lista.da.restituire[["giallo.increase.lcf.below"]]<-giallo.increase.lcf.below

    lista.da.restituire[["giallo.decrease.frf.above"]]<-giallo.decrease.frf.above
    lista.da.restituire[["giallo.decrease.lcf.above"]]<-giallo.decrease.lcf.above
    lista.da.restituire[["giallo.decrease.frf.below"]]<-giallo.decrease.frf.below
    lista.da.restituire[["giallo.decrease.lcf.below"]]<-giallo.decrease.lcf.below   
    
    lista.da.restituire[["giallonet.decrease.frf.above"]]<-giallonet.decrease.frf.above
    lista.da.restituire[["giallonet.decrease.lcf.above"]]<-giallonet.decrease.lcf.above    
    lista.da.restituire[["giallonet.decrease.frf.below"]]<-giallonet.decrease.frf.below
    lista.da.restituire[["giallonet.decrease.lcf.below"]]<-giallonet.decrease.lcf.below

    lista.da.restituire[["moreLitter.frf1"]]<-ml.frf1
    lista.da.restituire[["moreLitter.frf2"]]<-ml.frf2    
    lista.da.restituire[["moreLitter.lcf1"]]<-ml.lcf1
    lista.da.restituire[["moreLitter.lcf2"]]<-ml.lcf2
    
    lista.da.restituire[["moreDeadwood.frf1"]]<-md.frf1
    lista.da.restituire[["moreDeadwood.frf2"]]<-md.frf2    
    lista.da.restituire[["moreDeadwood.lcf1"]]<-md.lcf1
    lista.da.restituire[["moreDeadwood.lcf2"]]<-md.lcf2
    
        
  }#fine parti per regioni  
  
  
  return(lista.da.restituire)
  
}#fine funzione sintesiFinaleSuddivisoPerCPool


