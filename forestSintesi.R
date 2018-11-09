#crea file Sintesi per Italia e per Regioni

sintesi.ItaliaRegioni<-function(nome.regione,dsurface){
  
  #################### 
  #Creazione dati per file Sintesi: Sintesi Italia, Sintesi Abruzzo etc etc...
  #foglio aboveground 
  dati.aboveground %>% estraiDati(x=.,nome.variabile="total.carbon")->datotal.carbon
  #foglio belowground
  dati.belowground %>% estraiDati(x=.,nome.variabile="total.carbon")->dbtotal.carbon
  #foglio dead
  dati.dead %>% estraiDati(x=.,nome.variabile="total.carbon")->ddtotal.carbon
  #foglio litter
  dati.litter %>% estraiDati(x=.,nome.variabile="total.carbon")->dltotal.carbon	
  #oglio soil
  dati.soil %>% estraiDati(x=.,nome.variabile="total.carbon")->dstotal.carbon	
  
  #verifica che sia stesso ordine delle categorie
  lapply(c("da","db","dd","dl","ds"),FUN=function(prefix){
    
    if(!verificaOrdine(x=dsurface,y=get(paste0(prefix,"total.carbon")) )) stop("Attenzione ordine differente dei due dataframe")
    
  })#fine lapply  
  
  #se dsurface ha stesso ordine possiamo togliere colonne dei fattori
  dsurface %<>% dplyr::select(-c(1,2,3)) # dsurface[,4:ncol(dsurface)]->dsurface
  
  #estraiamo le sole colonne numeriche e le riassegnamo alle stesse variabili datotal.carbon etc..
  lapply(c("da","db","dd","dl","ds"),FUN=function(prefix){
    
    get(paste0(prefix,"total.carbon")) %>% dplyr::select(-c(1,2,3)) ->temp 
    assign(paste0(prefix,"total.carbon"),temp,inherits=TRUE)
    
  })#fine lapply 	
  
  lapply((annoI+1):annoF,FUN=differenzeColonne,x=dsurface)->diff.surface
  lapply((annoI+1):annoF,FUN=differenzeColonne,x=datotal.carbon)->diff.datotal.carbon
  lapply((annoI+1):annoF,FUN=differenzeColonne,x=dbtotal.carbon)->diff.dbtotal.carbon
  lapply((annoI+1):annoF,FUN=differenzeColonne,x=ddtotal.carbon)->diff.ddtotal.carbon
  lapply((annoI+1):annoF,FUN=differenzeColonne,x=dltotal.carbon)->diff.dltotal.carbon
  lapply((annoI+1):annoF,FUN=differenzeColonne,x=dstotal.carbon)->diff.dstotal.carbon
  
  #gli oggetti sopra creati
  objects(pattern="^diff\\..+[en]$")->lista.differenze
  
  lapply(lista.differenze,FUN=function(oggetto){
    
    get(oggetto)->temp
    names(temp)<-((annoI+1):annoF)
    data.frame(temp,check.names=FALSE)->dtemp
    assign(oggetto,dtemp,inherits=TRUE)
    
  })#fine lapply
  
  #creazione del dataframe finale
  lapply((annoI+1):annoF,FUN=function(yy){
    
    finale<-colonne.fattori
    finale$surface<-eval(parse(text=paste0("diff.surface$","'", yy,"'")))
    finale$total.aboveground.carbon<-eval(parse(text=paste0("diff.datotal.carbon$","'", yy,"'")))
    finale$total.belowground.carbon<-eval(parse(text=paste0("diff.dbtotal.carbon$","'", yy,"'")))
    finale$living.biomass<-finale$total.aboveground.carbon+finale$total.belowground.carbon
    finale$total.dead.carbon<-eval(parse(text=paste0("diff.ddtotal.carbon$","'", yy,"'")))
    finale$total.litter.carbon<-eval(parse(text=paste0("diff.dltotal.carbon$","'", yy,"'")))
    finale$dead.organic.matter<-finale$total.dead.carbon+finale$total.litter.carbon
    finale$total.soil.carbon<-eval(parse(text=paste0("diff.dstotal.carbon$","'", yy,"'")))
    
    finale
  })->out.sintesi #fine lapply
  
  names(out.sintesi)<-seq(annoI+1,annoF,by=1)
  
  nome.file<-paste0("Sintesi_",nome.regione,".xlsx")
  wb <- createWorkbook("Guido")
  
  lista.sheet.name<-as.list(paste0("gs",as.character(seq(annoI+1,annoF))))

  scriviExcel(my.wb=wb,lista.sheet.name,out.sintesi,prime.tre.colonne=NULL)->final.wb
  #scrittura etichetta
  lapply(lista.sheet.name,FUN=function(ss){
    writeData(final.wb,ss,x="Carbon Stock Change. Variabile surface: kha. Total Carbon variables: kt=Gg C",startCol=1,startRow=1)
    addStyle(final.wb,ss,style=fgFRF.style,rows=1:1,cols=1:1)
  })  #fine lapply  
  
  #nel caso delle regioni aggiiungiamo altri tre fogli
  if(length(grep(nome.regione,"Italia"))!=1){
    more.sheet.name<-list("soil","deadwood","litter")
    more.data.list<-list(dstotal.carbon,ddtotal.carbon,dltotal.carbon)
    names(more.data.list)<-c("soil","deadwood","litter")	
    out.sintesi[[1]] %>% dplyr::select(1,2,3)->prime.tre.colonne
    scriviExcel(my.wb=final.wb,more.sheet.name,more.data.list,prime.tre.colonne=prime.tre.colonne)->final.wb
    #scrittura etichetta
    lapply(more.sheet.name,FUN=function(ss){
      writeData(final.wb,ss,x=paste("Total",toupper(ss),"Carbon, kt=Gg C",sep=" "),startCol=1,startRow=1)
      addStyle(final.wb,ss,style=fgFRF.style,rows=1:1,cols=1:1)
    })  #fine lapply
    
  }#fine if su Italia  

  #scrittura del foglio sintesi Italia o regioni
  saveWorkbook(final.wb,paste0("./",nome.regione,"/",nome.file),overwrite=TRUE)

  #Fine parte file Sintesi...
  #######################################################################
  
  #devo distinguere tra Italia e Regioni
  if(length(grep(nome.regione,"Italia"))!=1){
  	return(list(out.sintesi,more.data.list)) #more data list mi serve per Sintesi per C Pool, foglio dead orgamic matter last
  }else{ 		
  	return(out.sintesi)
  }
  
}#fine sintesi.ItaliaRegioni
