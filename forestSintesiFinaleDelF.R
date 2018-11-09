#sintesi finale del F
sintesiFinaleDelF1<-function(foglioSintesi,nome.regione){
  
  #estraiamo le colonne dal 1990 in poi
  sommaPerVar3(my.data=foglioSintesi[["LandRemainingForestIncrease"]]) ->increase.frf

  #identifichiamo stands coppices other plantations
  which(increase.frf$var3=="stands")->index.stands
  which(increase.frf$var3=="coppices")->index.coppices
  which(increase.frf$var3=="plantations")->index.plantations
  which(increase.frf$var3=="other")->index.other #nell'output: rupicolulous and riparian
  #unstocked lo tralasciamo volutamente
  
  if(length(index.stands)!=1 | length(index.coppices)!=1 | length(index.plantations)!=1 | length(index.other)!=1 ) stop("forestSintesiFinaledelF: Fermati!")
  
  #prima colonna variabile var
  increase.frf%>% dplyr::select(1)->variabile.var
    
  #ora estriamo solo le colonne dal 1990 in poi
  grep("^198.$",names(increase.frf))->indice.grep
  if(length(indice.grep)){
    increase.frf%<>% dplyr::select(-starts_with("198"))
  }#fine if
  
  sommaPerVar3(my.data=foglioSintesi[["LandConvertingForestIncrease"]])->increase.lcf
  grep("^198.$",names(increase.lcf))->indice.grep
  if(length(indice.grep)){
    increase.lcf%<>% dplyr::select(-starts_with("198"))
  }#fine if    
  
  '+'(increase.frf %>% dplyr::select(-c(1)), increase.lcf %>% dplyr::select(-c(1))) ->total.forest.land.increase
  
  sommaPerVar3(my.data=foglioSintesi[["LandRemainingForestDecrease"]]) %>% dplyr::select(-starts_with("198")) ->decrease.frf
  sommaPerVar3(my.data=foglioSintesi[["LandConvertingForestDecrease"]]) %>% dplyr::select(-starts_with("198")) ->decrease.lcf
  
  '+'(decrease.frf %>% dplyr::select(-c(1)), decrease.lcf %>% dplyr::select(-c(1))) ->total.forest.land.decrease  

  #coeff. per plantations
  rep(coefIncrease.plantations1,(annoF-1990+1))->cincrease.plantations1
  rep(coefIncrease.plantations2,(annoF-1990+1))->cincrease.plantations2

  rep(coefDecrease.plantations1,(annoF-1990+1))->cdecrease.plantations1
  rep(coefDecrease.plantations2,(annoF-1990+1))->cdecrease.plantations2
  
  
  #indici: mi danno la posizione di stands, coppices, plantations e other all'interno del data.frame
  list(index.stands,index.coppices,index.other,index.plantations)->indici
  #coeff. da moltiplicare: coef di stands, coppices, other sono uguali, variano quelli di plantations
  list(coefIncrease.sintesiFinale1,coefIncrease.sintesiFinale1,coefIncrease.sintesiFinale1,cincrease.plantations1)->my.coef

  moltipilicaPerCoef(x=total.forest.land.increase,indici,my.coef)->finale.increase.frf

  #indici non variano
  #coef
  list(coefIncrease.sintesiFinale2,coefIncrease.sintesiFinale2,coefIncrease.sintesiFinale2,coefIncrease.plantations2)->my.coef
  moltipilicaPerCoef(x=total.forest.land.increase,indici,my.coef)->finale.increase.lcf
  
  #indici non variano
  #coeff. da moltiplicare: coef di stands, coppices, other sono uguali, variano quelli di plantations
  list(coefDecrease.sintesiFinale1,coefDecrease.sintesiFinale1,coefDecrease.sintesiFinale1,coefDecrease.plantations1)->my.coef
  moltipilicaPerCoef(x=total.forest.land.decrease,indici,my.coef)->finale.decrease.frf
  
  #indici non variano
  #coeff. da moltiplicare: coef di stands, coppices, other sono uguali, variano quelli di plantations
  list(coefDecrease.sintesiFinale2,coefDecrease.sintesiFinale2,coefDecrease.sintesiFinale2,coefDecrease.plantations2)->my.coef
  moltipilicaPerCoef(x=total.forest.land.decrease,indici,my.coef)->finale.decrease.lcf

  #ordine delle variabili
  increase.frf[c(index.stands,index.coppices,index.other,index.plantations),]->increase.frf
  increase.lcf[c(index.stands,index.coppices,index.other,index.plantations),]->increase.lcf
  decrease.frf[c(index.stands,index.coppices,index.other,index.plantations),]->decrease.frf
  decrease.lcf[c(index.stands,index.coppices,index.other,index.plantations),]->decrease.lcf

  #ai data.frame total vanno aggiunte le prima colonna della variabile var3 e riassegnato un ordine
  data.frame("var3"=c("stands","coppices","rupicolous and riparian forests","plantations"),
             total.forest.land.increase[c(index.stands,index.coppices,index.other,index.plantations),],check.names=FALSE)->total.forest.land.increase  
  data.frame("var3"=c("stands","coppices","rupicolous and riparian forests","plantations"),
             total.forest.land.decrease[c(index.stands,index.coppices,index.other,index.plantations),],check.names=FALSE)->total.forest.land.decrease
  

  #la funzione restituisce questa lista
  list(increase.frf,increase.lcf,total.forest.land.increase,finale.increase.frf,finale.increase.lcf
      ,decrease.frf,decrease.lcf,total.forest.land.decrease,finale.decrease.frf,finale.decrease.lcf)
  
}#fine sintesi finale del F1



#altra funzione: questa scrive DOM dal file C_Land_converted to forest
sintesiFinaleDelF2<-function(foglioSintesi,nome.regione,dsurface){

  #se nome.regione è una regione (non Italia) foglioSintesi è una lista di liste. 
  #In questa funzione ci serve solo la prima lista.
  if(length(grep(nome.regione,"Italia"))!=1) foglioSintesi[[1]]->foglioSintesi

  #################### 
  #Creazione dati per file Sintesi: Sintesi Italia, Sintesi Abruzzo etc etc...
  #foglio aboveground 
  #dati.aboveground %>% estraiDati(x=.,nome.variabile="surface")->dsurface
  ###guido: cambiare qui
  lapply((annoI+1):annoF,FUN=rapportoColonne,x=dsurface,reverse=TRUE)->rapp.surface
  
  names(rapp.surface)<-((annoI+1):annoF)
  data.frame(rapp.surface,check.names=FALSE)->rapp.surface
  
  #colonne dal 1990 in poi
  rapp.surface %<>% dplyr::select(-starts_with("198"))

  lapply(1990:annoF,FUN=function(yyy){

    foglioSintesi[[as.character(yyy)]] ->my.data

    domMultiply(x=my.data,y=rapp.surface[[as.character(yyy)]],var.x="dead.organic.matter")
  })->zz


  names(zz)<-(1990:annoF)
  data.frame(zz,check.names=FALSE)->cfromland.domLast1

 lapply(1990:annoF,FUN=function(yyy){
    
    foglioSintesi[[as.character(yyy)]] ->my.data
   cfromland.domLast1 %>% extract2(as.character(yyy))->y
    domDifferenza(x=my.data,y=y,var.x="dead.organic.matter")
    
  })->zz

 #aggiungiamo le colonne.fattori
 data.frame(cbind(colonne.fattori,cfromland.domLast1),check.names=FALSE)->cfromland.domLast1
  
  names(zz)<-(1990:annoF)
  data.frame(cbind(colonne.fattori,zz),check.names=FALSE)->cfromland.domLast2 
  return(list(cfromland.domLast1,cfromland.domLast2))
  
}#fine funzione


#############################################
#scrittura foglio sintesi finale del f. STACCO: QUANTE RIGHE VUOTE TRA UN DATA FRAME E L'ALTRO?
scriviFoglioSintesiFinaleDelF<-function(foglio1,foglio2,nome.regione){
  
  #scrive i primi due fogli
  scriviIncreaseDecreseDom<-function(wb,lista.fogli,nome.foglio){
    
    addWorksheet(wb,sheetName=nome.foglio)
    
    singoloDataFrame<-function(singolo.foglio,stringa,riga0){
      singolo.foglio %>% mutate(type=paste0(stringa,"_kt=Gg C")) %>% dplyr::select(type,everything())->my.data
      writeData(wb,nome.foglio,x=my.data,startRow=riga0,startCol=1,colNames=TRUE,rowNames=FALSE)
      #una riga va all'intestazione del data frame
      (1+nrow(my.data))->numero.righe
      t(colSums(my.data[,3:ncol(my.data)],na.rm=TRUE))->totali
      writeData(wb,nome.foglio,x="Total",startRow=riga0+numero.righe,startCol=2,colNames=FALSE,rowNames=FALSE)
      writeData(wb,nome.foglio,x=totali,startRow=riga0+numero.righe,startCol=3,colNames=FALSE,rowNames=FALSE)
      writeData(wb,nome.foglio,"kt (Gg) of CO2 equivalent",startCol=2,startRow=riga0+numero.righe+1,colNames=FALSE,rowNames=FALSE)
      writeData(wb,nome.foglio,x=(totali*44/12),startCol=3,startRow=riga0+numero.righe+1,colNames=FALSE,rowNames=FALSE)  

      #applica stili
      #font  
      addStyle(wb,sheet=nome.foglio,cols=1:ncol(my.data),rows=riga0:(riga0+nrow(my.data)+2),style=font.style,gridExpand=TRUE,stack=TRUE)
      #center
      addStyle(wb,sheet=nome.foglio,cols=1:ncol(my.data),rows=riga0:(riga0+nrow(my.data)+2),style=centre.style,gridExpand=TRUE,stack=TRUE)
      #rounding
      addStyle(wb,sheet=nome.foglio,cols=1:ncol(my.data),rows=riga0:(riga0+nrow(my.data)+2),style=rounding.style,gridExpand=TRUE,stack=TRUE)
      #intestazione bold
      addStyle(wb,sheet=nome.foglio,cols=1:ncol(my.data),rows=riga0:riga0,style=bold.style,gridExpand=TRUE,stack=TRUE)  
      #bold delle prime due colonne
      addStyle(wb,sheet=nome.foglio,cols=1:2,rows=riga0:(riga0+nrow(my.data)+2),style=bold.style,gridExpand=TRUE,stack=TRUE)  
      #border dell'intestazione
      addStyle(wb,sheet=nome.foglio,cols=1:ncol(my.data),rows=riga0:riga0,style=border.style,gridExpand=TRUE,stack=TRUE)          
      #foreground ell'header
      addStyle(wb,sheet=nome.foglio,cols=1:ncol(my.data),rows=riga0:riga0,style=fgheader.style,gridExpand=TRUE,stack=TRUE)  
      #foreground della prima colonna: dipende dal contenuto della stringa. Rosso o purple      
      if(length(grep("remaining",tolower(stringa)))){
       addStyle(wb,sheet=nome.foglio,cols=1:1,rows=riga0:(riga0+nrow(my.data)+2),style=fgFRF.style,gridExpand=TRUE,stack=TRUE)  
      }else if(length(grep("converting",tolower(stringa)))){
        addStyle(wb,sheet=nome.foglio,cols=1:1,rows=riga0:(riga0+nrow(my.data)+2),style=fgLCF.style,gridExpand=TRUE,stack=TRUE)  
      }else{
        addStyle(wb,sheet=nome.foglio,cols=1:1,rows=riga0:(riga0+nrow(my.data)+2),style=fgcell.style,gridExpand=TRUE,stack=TRUE)        
      } # se non trova ne ramaining ne converting resterà bianco (total + Land) 
      
      #foreground della seconda colonna      
      addStyle(wb,sheet=nome.foglio,cols=2:2,rows=riga0:(riga0+nrow(my.data)+2),style=fgheader.style,gridExpand=TRUE,stack=TRUE)  
      #riga dei totali: rosso
      addStyle(wb,sheet=nome.foglio,cols=2:ncol(my.data),rows=(riga0+nrow(my.data)+1),style=total.style,gridExpand=TRUE,stack=TRUE)  
      #dimensioni celle:
      setColWidths(wb,nome.foglio, cols=1:ncol(my.data), widths="auto")
      
    }#fine singoloDatFrame
    
    stringhe<-list("Increase for Forest Remaining Forest","Increase for Land Converting to Forest","Total Forest + Land","Increase for Forest Remaining Forest","Increase for Land Converting to Forest")
    righe<-list(1,10,19,28,37) #righe dove posizionare i dati nel foglio excel
    mapply(FUN=singoloDataFrame,lista.fogli[1:5],stringhe[1:5],righe[1:5])

  }#fine funzione scrivi IncreaseDecrease
  
  #foglio2 non contiene i dati di DOM, ma i diati di CfromLandConvertedToForest
  #da cui si ricavano poi i dati di DOM
  
  nome.file<-paste0("Sintesi_Finale_del_F_",nome.regione,".xlsx")
  createWorkbook("guido")->wb.sintesiFinaleDelF
  #i primi cinque fogli sono del tab Increase
  scriviIncreaseDecreseDom(wb=wb.sintesiFinaleDelF,lista.fogli =foglio1[1:5],nome.foglio="Increase")
  #foglio decrease
  scriviIncreaseDecreseDom(wb=wb.sintesiFinaleDelF,lista.fogli =foglio1[6:10],nome.foglio="Decrease")
  #foglio DOM
  scriviIncreaseDecreseDom(wb=wb.sintesiFinaleDelF,lista.fogli =foglio2,nome.foglio="DOM_f")
  saveWorkbook(wb.sintesiFinaleDelF,paste0("./",nome.regione,"/",nome.file),overwrite=TRUE)
  
}#fine funzione scriviFoglioSintesiFinaleDelF


#questa funzione prende i dati in CFromLandConvertingToForest e produce i dati di SintesiFinaleDelF
#per il Tab DOM. Le operazioni sono simili a quelle di sintesi FinaleDelF1 ma leggermente diverse
sintesiFinaleDelF1.variant<-function(foglioSintesi,nome.regione){
   
  sommaPerVar3<-function(my.data){

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
    my.data %>% group_by(var3) %>% dplyr::select(-c(1,2,3)) %>% summarise_each(funs(sum))
    
  }#fine funzione sommaPerVar3

  #estraiamo le colonne dal 1990 in poi
  sommaPerVar3(my.data=foglioSintesi[[1]]) ->dom.frf #dom forest remaining forest
  
  #identifichiamo stands coppices other plantations
  which(dom.frf$var3=="stands")->index.stands
  which(dom.frf$var3=="coppices")->index.coppices
  which(dom.frf$var3=="plantations")->index.plantations
  which(dom.frf$var3=="other")->index.other #nell'output: rupicolulous and riparian
  #unstocked lo tralasciamo volutamente
  
  if(length(index.stands)!=1 | length(index.coppices)!=1 | length(index.plantations)!=1 | length(index.other)!=1 ) stop("forestSintesiFinaledelF: Fermati!")
  
  #prima colonna variabile var
  dom.frf%>% dplyr::select(1)->variabile.var

  
  #ora estriamo solo le colonne dal 1990 in poi
  sommaPerVar3(my.data=foglioSintesi[[2]]) ->dom.lcf
  
  '+'(dom.frf %>% dplyr::select(-c(1)), dom.lcf %>% dplyr::select(-c(1))) ->total.dom
  
  
  #coeff. per plantations
  rep(coefIncrease.plantations1,(annoF-1990+1))->cincrease.plantations1
  rep(coefIncrease.plantations2,(annoF-1990+1))->cincrease.plantations2  
  
  #questa funzione moltiplica i totali di stands coppices plantations (foglio Sintesi finale del f) per i corrispoettivi coef
  moltipilicaPerCoef<-function(x,iii,ccc){
    
    mapply(FUN=function(dati=x,indici,my.coef){
      
      dati[indici,]*my.coef->ris
      
      unlist(ris)
      
    },indici=iii,my.coef=ccc)->zz #fine mapply
    #questo sarà l ordine dei file di output
    data.frame("var3"=c("stands","coppices","rupicolous and riparian forests","plantations"),t(zz),check.names=FALSE)
    
  }#fine funzione moltiplicaPerCoef  
  
  #indici: mi danno la posizione di stands, coppices, plantations e other all'interno del data.frame
  list(index.stands,index.coppices,index.other,index.plantations)->indici
  #coeff. da moltiplicare: coef di stands, coppices, other sono uguali, variano quelli di plantations
  list(coefDOM.frf,coefDOM.frf,coefDOM.frf,coefIncrease.plantations1)->my.coef
  moltipilicaPerCoef(x=total.dom,indici,my.coef)->finale.dom.frf
  
  #indici non variano
  #coef
  list(coefDOM.lcf,coefDOM.lcf,coefDOM.lcf,coefIncrease.plantations2)->my.coef
  moltipilicaPerCoef(x=total.dom,indici,my.coef)->finale.dom.lcf
  
  #ordine delle variabili
  dom.frf[c(index.stands,index.coppices,index.other,index.plantations),]->dom.frf
  dom.lcf[c(index.stands,index.coppices,index.other,index.plantations),]->dom.lcf
  
  #ai data.frame total vanno aggiunte le prima colonna della variabile var3 e riassegnato un ordine
  data.frame("var3"=c("stands","coppices","rupicolous and riparian forests","plantations"),
             total.dom[c(index.stands,index.coppices,index.other,index.plantations),],check.names=FALSE)->total.dom  
 
  
  #la funzione restituisce questa lista
  list(dom.frf,dom.lcf,total.dom,finale.dom.frf,finale.dom.lcf)
  
}#fine sintesi finale del F1

