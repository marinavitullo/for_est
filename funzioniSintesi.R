#funzioni per il calcolo dei fogli sintesi

#########################################################
#unisci i data.frame con soli valori al data.frame che contiene management
#var3 e inventory.tipology. La funzione fa affidamento che i dati seguano
#l'ordine standard (quello nei file di Marina) di inventory.tipology.

#aggiungiTotaliColonna aggiunge la riga dei totali (utile al momento della creazione dei file xlsx)
unisciFattoriDati<-function(fattori,x,aggiungiTotaliColonna=FALSE,totaliKT=FALSE,fill.gaps.0=FALSE){
  
  if(missing(x) | missing(fattori)) stop(paste0("Funzione ",match.call()[[1]],": parametro mancante"))
  
  if(nrow(fattori)!=nrow(x)) stop(paste0("Funzione ",match.call()[[1]],": data.frame con diversi numeri di riga"))
  
  #riempi con zeri
  if(fill.gaps.0){
    x[nrow(x),]<-0
  }
  
  
  if(aggiungiTotaliColonna){
    
    as.data.frame(rbind(fattori,c("","","total")),stringsAsFactors=FALSE)->fattori
    apply(x,2,sum,na.rm=TRUE)->somma.colonne
    
    as.data.frame(rbind(x,somma.colonne),stringsAsFactors=FALSE)->x			
    
    if(totaliKT){
      
      as.data.frame(rbind(fattori,c("","","kt (Gg) of CO2 equivalent")),stringsAsFactors=FALSE)->fattori
      (somma.colonne*CD2C)->total.kt
      as.data.frame(rbind(x,total.kt),stringsAsFactors=FALSE)->x			
      
    }#fine totaliKT
    
  }#su aggiungiTotaliColonna
  
  
  return(as.data.frame(cbind(fattori,x),stringsAsFactors=FALSE))
  
}

#########################################################
#x è un data frame prodotto mediante il comando "melt", nome.variabile è il nome della 
#variabile che si vuole estrarre (ad esempio surface). L'output è un dataframe
#della variabile "nome.variabile" dove ogni colonna corrisponde a un anno
estraiDati<-function(x,nome.variabile){
  
  if(missing(x)| missing(nome.variabile)) stop(paste0("Funzione ",match.call()[[1]],": parametro mancante"))	
  which(x$variable==nome.variabile)->index
  
  if(!length(index)) stop("Nessun dato trovato")
  x[index,]->out
  dcast(out,management+var3+inventory.tipology~yy,value.var="value")->dout
  riordina(x=dout,y=colonne.fattori)->dout2
  return(dout2)
  
}#estrai dati


#########################################################
#riordina i risultati sulla base di  invTipology 
riordina<-function(x=NA,y=NA){
  
  if(missing(x) | missing(y)) stop(paste0("Funzione ",match.call()[[1]],": parametro mancante"))
  
  merge(y,x,by=c("management","var3","inventory.tipology"),all.x=TRUE,sort=FALSE)->out
  as.character(out$inventory.tipology)->out$inventory.tipology
  
  #controllo importante
  #se ho un numero differente di righe vuol dire che il merge ha fallito
  if(nrow(out)!=nrow(y)) stop("merge non è andato a buon fine")
  
  out
  
}#fine riordina


#########################################################
#verifica ordine dei due data.frame rispetto a inventory.tipology 
#serve per essere sicuri (controllo paranoico) di aver mantenuto l'ordine dei dati
#in base a inventory tipology
verificaOrdine<-function(x=NA,y=NA){
  
  risultato<-TRUE
  if(missing(x) | missing(y)) stop(paste0("Funzione ",match.call()[[1]],": parametro mancante"))
  
  if(!all(x$inventory.tipology==y$inventory.tipology)) risultato<-FALSE
  
  risultato
  
}#fine verificaOrdine


##############################################################
#fa la differenza tra le colonne tra un anno e l'altro. Ad esempio nel file Sintesi compaiono
#le differenze tra le superfici del 1990 e quelle del 1989 etc etc
differenzeColonne<-function(var,x){
  
  if(missing(x) | missing(var)) stop(paste0("Funzione ",match.call()[[1]],": parametro mancante"))
  
  
  x %>% extract2(as.character(var))->colonna1
  x %>% extract2(as.character(var-1))->colonna2
  
  ((colonna1-colonna2)/1000)->differenza
  
  return(differenza)
  
}#fine differenze colonne	

##############################################################
#fa il rapporto tra le colonne tra un anno e l'altro. Ad esempio nei fogli DOM (Dead Organic Matter)
#le differenze tra le superfici del 1990 e quelle del 1989 etc etc
rapportoColonne<-function(var,x,reverse=FALSE){
  
  if(missing(x) | missing(var)) stop(paste0("Funzione ",match.call()[[1]],": parametro mancante"))
  
  x %>% extract2(as.character(var))->colonna1
  x %>% extract2(as.character(var-1))->colonna2
  
  if(!reverse){
    (colonna1/colonna2)->rapporto
  }else{
    (colonna2/colonna1)->rapporto
  }
  
  return(rapporto)
  
}#fine rapporto colonne	



#################################################################
#DOM multiply: utile per calcolare DOM (Dead organic matter) per il rapporto tra superfici
#oppure (nel caso delle regioni) Litter * rapporto tra superfici etc etc
#x data frame con diverse variabili. Var.x la variabile di x che si vuole moltiplicare per y
domMultiply<-function(x,y,var.x){
  
  #if(missing(x) | missing(var) | missing(var.x) ) stop(paste0("Funzione ",match.call()[[1]],": parametro mancante"))
  
    x %>% extract2(var.x) ->dati1

    return(dati1*y)

}#fine domMultiply  


#x data frame con diverse variabili. Var.x la variabile di x che si vuole moltiplicare per y
domDifferenza<-function(x,y,var.x){
  
  #if(missing(x) | missing(var) | missing(var.x) ) stop(paste0("Funzione ",match.call()[[1]],": parametro mancante"))
  
    x %>% extract2(var.x) ->dati1

    return(dati1-y)
  
}#fine domDifferenza 

#########################################################
#sommaPerVar3
#Funzione utilizzata in sintediDelF/SintesiDelFPerPool
#########################################################
#deforestaztion viene utilizzato per calcolare i valori necessari a litter e deadwood
#in SIntesi per C pool a livello regionale
sommaPerVar3<-function(my.data,deforestation=NULL){
  
  #plantations+others va sommato con stands: quindi riconvertiamo plantations in stands
  which(my.data$var3=="plantations" & my.data$inventory.tipology=="others")->index
  
  stopifnot(length(index)==1)
  my.data[index,c("var3")]<-"stands"
  #da "var3" togliamo "other" quando "inventory.tipology" è uguale a shrublands: in questo modo
  #sommando other considereremo solo rupicolous fores e riparian forest
  which(my.data$var3=="other" & my.data$inventory.tipology=="shrublands")->index
  stopifnot(length(index)==1)
  my.data[-index,]->my.data
  
  #summarize
  my.data %>% group_by(var3) %>% dplyr::select(-c(1,2,3)) %>% summarise_each(funs(sum(.,na.rm=TRUE)))->my.data2
  
  #deforestation si usa solo ne caso delle regioni e per il foglio Sintesi per C pool ( o meglio
  #nel foglio surface)
  if(!is.null(deforestation)){
  
    if((ncol(my.data2)-1)!=length(deforestation))stop("Funzione sommaPerVar3, lunghezza deforestation differisce da my.data2")
    which(my.data2$var3=="coppices")->riga
    if(length(riga)!=1) stop("sommaPerVar3 Problemi con Coppices??")
    
    my.data2 %>% slice(riga) %>% dplyr::select(seq(2,ncol(my.data2))) %>% t ->vettore.coppices
    
    vettore.coppices-deforestation -> new.vettore.coppices #coppices meno deforestation  
    my.data2[riga,2:ncol(my.data2)]<-new.vettore.coppices
    
  }#fine su deforestation
  
  return(my.data2)
  
}#fine funzione sommaPerVar3


#########################################################
#moltipilicaPerCoef
#Funzione utilizzata in sintesiDelF/sintesiDelFPerPool
#########################################################
moltipilicaPerCoef<-function(x,iii,ccc){
  
  mapply(FUN=function(dati=x,indici,my.coef){
    
    dati[indici,]*my.coef->ris
    
    unlist(ris)
    
  },indici=iii,my.coef=ccc)->zz #fine mapply
  
  #questo sarà l ordine dei file di output
  data.frame("var3"=c("stands","coppices","rupicolous and riparian forests","plantations"),t(zz),check.names=FALSE)
  
}#fine funzione moltiplicaPerCoef  


#aggiunge il suffisso FM alle etichette (anni di colonna)
aggiungiFM<-function(x){
  
  x %>% dplyr::select(matches("^[[:digit:]]+$"))->dati
  x %>% dplyr::select(-matches("^[[:digit:]]+$"))->nondati
  paste("FM",names(dati),sep="_")->names(dati)  
  
  bind_cols(nondati,dati)
  
}#fine aggiungiFM
