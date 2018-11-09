sintesiFinaleDelFPerCPool<-function(foglio,nome.regione){

  #se si tratta di un foglio FM (ovvero le parti in giallo scritte per le regioni)
  #una parte del foglio non va scritta. Nel caso di FM==TRUE agli anni (nomi delle colonne)
  #viene aggiunto il suffisso FM_
  
  scriviTab<-function(foglioSintesi,wb,nomeTab,FM=FALSE){  
        #estraiamo le colonne dal 1990 in poi
        sommaPerVar3(my.data=foglioSintesi[[1]]) ->increase.frf.above
      
        #identifichiamo stands coppices other plantations
        which(increase.frf.above$var3=="stands")->index.stands
        which(increase.frf.above$var3=="coppices")->index.coppices
        which(increase.frf.above$var3=="plantations")->index.plantations
        which(increase.frf.above$var3=="other")->index.other #nell'output: rupicolulous and riparian
        #unstocked lo tralasciamo volutamente
        
        if(length(index.stands)!=1 | length(index.coppices)!=1 | length(index.plantations)!=1 | length(index.other)!=1 ) stop("forestSintesiFinaledelF: Fermati!")
       
        #prima colonna variabile var
        increase.frf.above%>% dplyr::select(1)->variabile.var
        
        #ora estriamo solo le colonne dal 1990 in poi, solo se esistono colonne antecedenti al 1990
        grep("^198.$",names(increase.frf.above))->indice.grep
        if(length(indice.grep)){
          increase.frf.above%<>% dplyr::select(-starts_with("198"))
        }#fine if
        
        sommaPerVar3(my.data=foglioSintesi[[2]])->increase.frf.below
        grep("^198.$",names(increase.frf.below))->indice.grep
        if(length(indice.grep)){
          increase.frf.below%<>% dplyr::select(-starts_with("198"))
        }#fine if  
      
        sommaPerVar3(my.data=foglioSintesi[[3]])->increase.lcf.above
        grep("^198.$",names(increase.lcf.above))->indice.grep
        if(length(indice.grep)){
          increase.lcf.above%<>% dplyr::select(-starts_with("198"))
        }#fine if  
        
        sommaPerVar3(my.data=foglioSintesi[[4]])->increase.lcf.below
        grep("^198.$",names(increase.lcf.below))->indice.grep
        if(length(indice.grep)){
          increase.lcf.below%<>% dplyr::select(-starts_with("198"))
        }#fine if    
      
        '+'(increase.frf.above %>% dplyr::select(-c(1)),increase.lcf.above %>% dplyr::select(-c(1)))->total.fli.above
        '+'(increase.frf.below %>% dplyr::select(-c(1)),increase.lcf.below %>% dplyr::select(-c(1)))->total.fli.below
        
        '+'(total.fli.above,total.fli.below) ->total.forest.land.increase
        
        #indici: mi danno la posizione di stands, coppices, plantations e other all'interno del data.frame
        list(index.stands,index.coppices,index.other,index.plantations)->indici
        #coeff. da moltiplicare: coef di stands, coppices, other sono uguali. A differenza di
        #foglioSintesidelF  i coeff. di plantations sono uguali agli altri
        list(coefIncrease.sintesiFinale1,coefIncrease.sintesiFinale1,coefIncrease.sintesiFinale1,coefIncrease.sintesiFinale1)->my.coef
        
        #questo è: increase for forest remaning forest aboveground
        moltipilicaPerCoef(x=total.fli.above,indici,my.coef)->finale.increase.frf.above
        moltipilicaPerCoef(x=total.fli.below,indici,my.coef)->finale.increase.frf.below  

        #questo è: increase for land converting to forest belowground (ultima sezione del foglio excel)
        list(coefIncrease.sintesiFinale2,coefIncrease.sintesiFinale2,coefIncrease.sintesiFinale2,coefIncrease.sintesiFinale2)->my.coef
        moltipilicaPerCoef(x=total.fli.above,indici,my.coef)->finale.increase.lcf.above
        moltipilicaPerCoef(x=total.fli.below,indici,my.coef)->finale.increase.lcf.below  
        
        #scrittura excel: foglio Increase
        increase.frf.above[c(index.stands,index.coppices,index.other,index.plantations),]->increase.frf.above
        increase.frf.below[c(index.stands,index.coppices,index.other,index.plantations),]->increase.frf.below
        
        increase.frf.above$var3[increase.frf.above$var3=="other"]<-"rupicolous and riparian forests"
        increase.frf.below$var3[increase.frf.below$var3=="other"]<-"rupicolous and riparian forests"
        
        #nel caso delle regioni abbiamo in più la parte in giallo FM. Le etichette degli anni 
        #le modifichiamo con il suffisso FM_
        if(FM){
          aggiungiFM(x=increase.frf.above)->increase.frf.above
          aggiungiFM(x=increase.frf.below)->increase.frf.below
          aggiungiFM(x=increase.lcf.above)->increase.lcf.above
          aggiungiFM(x=increase.lcf.below)->increase.lcf.below            
        }#su FM
        
        scriviExcel(my.wb=wb,list(nomeTab),lista.dati=list(list(increase.frf.above,increase.frf.below)),prime.tre.colonne =NULL,shift.prime.tre.colonne=0,mtg=FALSE,convertiTotali=FALSE)->new.wb
        #scrittura etichetta e stile
        mapply(FUN=function(etichetta,stile,riga){
          writeData(new.wb,nomeTab,x=paste(nomeTab,etichetta,sep=" "),startCol=1,startRow=riga)    
          addStyle(new.wb,nomeTab,style=stile,rows=riga:riga,cols=1:1,gridExpand=TRUE,stack=TRUE)
        },etichetta=list("per Forest Remaining Forest - aboveground, kt=Gg C"," per Forest Remaining Forest - belowground, kt=Gg C"),stile=list(fgFRF.style,fgFRF.style),riga=list(1,10))
        
        increase.lcf.above[c(index.stands,index.coppices,index.other,index.plantations),]->increase.lcf.above
        increase.lcf.below[c(index.stands,index.coppices,index.other,index.plantations),]->increase.lcf.below
        
        increase.lcf.above$var3[increase.lcf.above$var3=="other"]<-"rupicolous and riparian forests"
        increase.lcf.below$var3[increase.lcf.below$var3=="other"]<-"rupicolous and riparian forests"
        
        #parte land converting forest
        scriviExcel(my.wb=new.wb,list(nomeTab),lista.dati=list(list(increase.lcf.above,increase.lcf.below)),prime.tre.colonne =NULL,primaRiga=20,shift.prime.tre.colonne=0,mtg=FALSE,convertiTotali=FALSE)->new.wb  
        #scrittura etichetta e stile
        mapply(FUN=function(etichetta,stile,riga){
          writeData(new.wb,nomeTab,x=paste(nomeTab,etichetta,sep=" "),startCol=1,startRow=riga)    
          addStyle(new.wb,nomeTab,style=stile,rows=riga:riga,cols=1:1,gridExpand=TRUE,stack=TRUE)
        },etichetta=list("per Land Converted to Forest - aboveground, kt=Gg C","per Land Converted to Forest - belowground, kt=Gg C"),stile=list(fgLCF.style,fgLCF.style),riga=list(19,28))
        
        #riga 41
        total.forest.land.increase<-bind_cols(variabile.var,total.forest.land.increase) %>% slice(c(index.stands,index.coppices,index.other,index.plantations))
        total.forest.land.increase$var3[total.forest.land.increase$var3=="other"]<-"rupicolous and riparian forests"
        
        #solo nel caso Italia questa parte va scritta
        if(!FM){
          scriviExcel(my.wb=new.wb,list(nomeTab),lista.dati=list(total.forest.land.increase),prime.tre.colonne =NULL,primaRiga=38,shift.prime.tre.colonne=0,mtg=FALSE,convertiTotali=FALSE)->new.wb  
          writeData(new.wb,nomeTab,x="TOTAL FL",startCol=1,startRow=37)    
          addStyle(new.wb,nomeTab,style=fgcell.style,rows=37:37,cols=1:1,gridExpand=TRUE,stack=TRUE)
        }
          
        #riga 49
        total.fli.above<-bind_cols(variabile.var,total.fli.above) %>% slice(c(index.stands,index.coppices,index.other,index.plantations))
        total.fli.above$var3[total.fli.above$var3=="other"]<-"rupicolous and riparian forests"
      
        total.fli.below<-bind_cols(variabile.var,total.fli.below) %>% slice(c(index.stands,index.coppices,index.other,index.plantations))
        total.fli.below$var3[total.fli.below$var3=="other"]<-"rupicolous and riparian forests"
      
        if(!FM){
          scriviExcel(my.wb=new.wb,list(nomeTab),lista.dati=list(list(total.fli.above,total.fli.below)),prime.tre.colonne =NULL,primaRiga=47,shift.prime.tre.colonne=0,mtg=FALSE,convertiTotali=FALSE)->new.wb  
          #scrittura etichetta e stile
          etichetta1<-"TOTAL FL - aboveground, kt=Gg C"
          etichetta2<-"TOTAL FL - belowground, kt=Gg C"
          
          #nel caso DOM-f cambiano le etichette
          if(length(grep("dom",tolower(nomeTab)))){etichetta1<-"TOTAL FL - litter, kt=Gg C";etichetta2<-"TOTAL FL - deadwood, kt=Gg C"}
          
          mapply(FUN=function(etichetta,stile,riga){
            writeData(new.wb,nomeTab,x=etichetta,startCol=1,startRow=riga)    
            addStyle(new.wb,nomeTab,style=stile,rows=riga:riga,cols=1:1,gridExpand=TRUE,stack=TRUE)
          },etichetta=list(etichetta1,etichetta2),stile=list(fgcell.style,fgcell.style),riga=list(46,55))
        }#fine if
        
        if(!FM){
          scriviExcel(my.wb=new.wb,list(nomeTab),lista.dati=list(list(finale.increase.frf.above,finale.increase.frf.below)),prime.tre.colonne =NULL,primaRiga=65,shift.prime.tre.colonne=0,mtg=FALSE,convertiTotali=FALSE)->new.wb  
          #scrittura etichetta e stile
          mapply(FUN=function(etichetta,stile,riga){
            writeData(new.wb,nomeTab,x=paste(nomeTab,etichetta,sep=" "),startCol=1,startRow=riga)    
            addStyle(new.wb,nomeTab,style=stile,rows=riga:riga,cols=1:1,gridExpand=TRUE,stack=TRUE)
          },etichetta=list("per Forest Remaining Forest - aboveground, kt=Gg C","per Forest Remaining Forest - belowground, kt=Gg C"),stile=list(fgFRF.style,fgFRF.style),riga=list(64,73))
        }#fine if    
          
        if(!FM){
          scriviExcel(my.wb=new.wb,list(nomeTab),lista.dati=list(list(finale.increase.lcf.above,finale.increase.lcf.below)),prime.tre.colonne =NULL,primaRiga=83,shift.prime.tre.colonne=0,mtg=FALSE,convertiTotali=FALSE)->new.wb  
          #scrittura etichetta e stile
          mapply(FUN=function(etichetta,stile,riga){
            writeData(new.wb,nomeTab,x=paste(nomeTab,etichetta,sep=" "),startCol=1,startRow=riga)    
            addStyle(new.wb,nomeTab,style=stile,rows=riga:riga,cols=1:1,gridExpand=TRUE,stack=TRUE)
          },etichetta=list("per Land Converted to Forest - aboveground, kt=Gg C","per Land Converted to Forest - belowground, kt=Gg C"),stile=list(fgLCF.style,fgLCF.style),riga=list(82,91))
        }#fine if  
        
        return(new.wb)
        
  }#fine nomeTab        
        
  createWorkbook("guido")->wb
  scriviTab(foglioSintesi=foglio[1:4],wb=wb,nomeTab="Increase")->new.wb
  scriviTab(foglioSintesi=foglio[5:8],wb=wb,nomeTab="Decrease")->new.wb  
  scriviTab(foglioSintesi=foglio[9:12],wb=wb,nomeTab="DOM_f")->new.wb
  
  #parte per regioni: sono le parti evidenziate in giallo!
  if(length(grep("italia",tolower(nome.regione)))!=1){  

    foglio[c("net.change.frf.above","net.change.frf.below","net.change.lcf.above","net.change.lcf.below")]->foglioNetChange
    scriviTab(foglioSintesi=foglioNetChange,wb=wb,nomeTab="NetChange",FM=FALSE)->new.wb
        
    foglio[c("giallo.increase.frf.above","giallo.increase.frf.below","giallo.increase.lcf.above","giallo.increase.lcf.below")]->foglioGialloIncrease
    scriviTab(foglioSintesi=foglioGialloIncrease,wb=wb,nomeTab="FM_Increase",FM=TRUE)->new.wb
    
    foglio[c("giallo.decrease.frf.above","giallo.decrease.frf.below","giallo.decrease.lcf.above","giallo.decrease.lcf.below")]->foglioGialloDecrease
    scriviTab(foglioSintesi=foglioGialloDecrease,wb=wb,nomeTab="FM_Decrease",FM=TRUE)->new.wb    

    foglio[c("giallonet.decrease.frf.above","giallonet.decrease.frf.below","giallonet.decrease.lcf.above","giallonet.decrease.lcf.below")]->foglioGialloNet
    scriviTab(foglioSintesi=foglioGialloNet,wb=wb,nomeTab="FM_NetChange",FM=TRUE)->new.wb    
    
    
  }#fine parte regioni  

  nome.file<-paste0("Sintesi_Finale_Del_F_per_C_pool_",nome.regione,".xlsx")    
  saveWorkbook(new.wb,paste0("./",nome.regione,"/",nome.file),overwrite = TRUE)

}#fine sintesiFinaleDelFPerCPool
