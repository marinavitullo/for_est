#stili per fogli excel
library("openxlsx")

#stili per i fogli Excel
#fontsize
font.style<-createStyle(fontSize=9)
#testo bold
bold.style <-createStyle(textDecoration = "bold")
#header in grassetto
centre.style <-createStyle(halign="center")
#header con bordo nero
border.style <-createStyle(border="TopBottomLeftRight",borderColour="#000000",borderStyle="thin")
#righe dei totali in rosso e in grassetto
total.style <-createStyle(fontColour="#DE564B",textDecoration = "bold",halign="center",fontSize=9,numFmt="0.00")
total.style.black <-createStyle(fontColour="#000000",textDecoration = "bold",halign="center",fontSize=9,numFmt="0.00")
#gaps style: colore verde come fogli Marina
gaps.style <-createStyle(fontColour="#8FB98B",)
#rounding
rounding.style <-createStyle(numFmt="0.00")
#fgcell: colore foreground
fgcell.style<-createStyle(fgFill="#FFFFFF")
#fgheader
fgheader.style<-createStyle(fgFill="#FFE9D8")
#sfondo Rosso
fgFRF.style<-createStyle(fgFill="#FF0000",fontColour="#FFFFFF",fontSize=10)
#sfondo viola
fgLCF.style<-createStyle(fgFill="#666699",fontColour="#FFFFFF",fontSize=10)
#scritta grigia
fgGRAY.style<-createStyle(fgFill="#FFFFFF",fontColour="#AAAAAA",fontSize=10)



#applica gli stili di base al foglio excel
#more.than.gaps. Ci sono altre righe dopo gaps? Nei fogli sintesi si, quindi gaps si trova a righe-2
#altrimenti gaps si trova a "righe"

#rowShift e colShifr vengono usati per scrivere pià dataframe nello stesso foglio Excel
applicaStili<-function(foglio=NA,sheet.name=NA,righe=NA,colonne=NA,more.than.gaps=FALSE,colShift=1,rowShift=1,shift.prime.tre.colonne=2){

		if(missing(foglio)| missing(sheet.name) | missing(righe) | missing(colonne)) 
			stop(paste0("Funzione ",match.call()[[1]],": parametro mancante"))	

		#dimensione del testo
		addStyle(foglio,sheet.name,style=font.style,rows=(1+rowShift-1):(righe+rowShift-1),cols=(1+colShift-1):(colonne+colShift-1),gridExpand=TRUE,stack=TRUE)	
		#bold columns: le prime tre
		addStyle(foglio,sheet=sheet.name,cols=(1+colShift-1):((1+shift.prime.tre.colonne)+colShift-1),rows=(1+rowShift-1):(righe+rowShift-1),style=bold.style,gridExpand=TRUE,stack=TRUE)
		#bold header
		addStyle(foglio,sheet=sheet.name,cols=(1+colShift-1):(colonne+colShift-1),rows=(1+rowShift-1):(1+rowShift-1),style=bold.style,gridExpand=TRUE,stack=TRUE)
		#bordo per header
		addStyle(foglio,sheet=sheet.name,cols=(1+colShift-1):(colonne+colShift-1),rows=1+rowShift-1,style=border.style,gridExpand=TRUE,stack=TRUE)

		#gaps in verde
		if(!more.than.gaps){
			addStyle(foglio,sheet=sheet.name,cols=(2+colShift-1):(colonne+colShift-1),rows=(righe+rowShift-1),style=gaps.style,gridExpand=TRUE,stack=TRUE)
		}else{
			addStyle(foglio,sheet=sheet.name,cols=(2+colShift-1):(colonne+colShift-1),rows=(righe-2+rowShift-1),style=gaps.style,gridExpand=TRUE,stack=TRUE)
		}
	
		#centra il testo
		addStyle(foglio,sheet.name,style=centre.style,rows=(1+rowShift-1):(righe+rowShift-1),cols=(1+colShift-1):(colonne+colShift-1),gridExpand=TRUE,stack=TRUE)	
		#rounding
		addStyle(foglio,sheet.name,style=rounding.style,rows=(2+rowShift-1):(righe+rowShift-1),cols=((2+shift.prime.tre.colonne)+colShift-1):(colonne+colShift-1),gridExpand=TRUE,stack=TRUE)	
		#fgcell
		addStyle(foglio,sheet.name,style=fgcell.style,rows=(2+rowShift-1):(righe+rowShift-2),cols=((2+shift.prime.tre.colonne)+colShift-1):(colonne+colShift-1),gridExpand=TRUE,stack=TRUE)	
		#fgheader
		addStyle(foglio,sheet.name,style=fgheader.style,rows=(1+rowShift-1),cols=((2+shift.prime.tre.colonne)+colShift-1):(colonne+colShift-1),gridExpand=TRUE,stack=TRUE)
		#fgheader per prime tre colonne
		addStyle(foglio,sheet.name,style=fgheader.style,rows=(1+rowShift-1):(righe+rowShift-2),cols=(1+colShift-1):((1+shift.prime.tre.colonne)+colShift-1),gridExpand=TRUE,stack=TRUE)
		#dimensione celle
		setColWidths(foglio, sheet.name, cols=(1+colShift-1):(colonne+colShift-1), widths="auto")

		return(foglio)

}#fine applica stili


applicaStiliOutcomes<-function(foglio=NA,sheet.name=NA,righe=NA,colonne=NA){

		if(missing(foglio)| missing(sheet.name) | missing(righe) | missing(colonne)) 
			stop(paste0("Funzione ",match.call()[[1]],": parametro mancante"))	

		#dimensione del testo
		addStyle(foglio,sheet.name,style=font.style,rows=1:righe,cols=1:colonne,gridExpand=TRUE,stack=TRUE)	
		#bold columns: le prime tre
		addStyle(foglio,sheet=sheet.name,cols=1,rows=1:righe,style=bold.style,gridExpand=TRUE,stack=TRUE)
		#bold header
		addStyle(foglio,sheet=sheet.name,cols=1:colonne,rows=1:1,style=bold.style,gridExpand=TRUE,stack=TRUE)
		#bordo per header
		addStyle(foglio,sheet=sheet.name,cols=1:colonne,rows=1,style=border.style,gridExpand=TRUE,stack=TRUE)
	
		#centra il testo
		addStyle(foglio,sheet.name,style=centre.style,rows=1:righe,cols=1:colonne,gridExpand=TRUE,stack=TRUE)	
		#rounding
		addStyle(foglio,sheet.name,style=rounding.style,rows=2:righe,cols=2:colonne,gridExpand=TRUE,stack=TRUE)	
		#fgcell
		addStyle(foglio,sheet.name,style=fgcell.style,rows=2:righe,cols=2:colonne,gridExpand=TRUE,stack=TRUE)	
		#fgheader
		addStyle(foglio,sheet.name,style=fgheader.style,rows=1,cols=2:colonne,gridExpand=TRUE,stack=TRUE)
		#fgheader per prima colonna
		addStyle(foglio,sheet.name,style=fgheader.style,rows=1:righe,cols=1,gridExpand=TRUE,stack=TRUE)
		#dimensione celle
		setColWidths(foglio, sheet.name, cols=1:colonne, widths="auto")
		return(foglio)

}#fine applica stili


#scrittura dei file dati
#my.wb: workbook Excel. Il workbook va a perto e chiuso fuori dalla funzione
#lista.sheet.name: lista dei nomi dei fogli (i tab Excel)
#lista.dati: lista di dati. Ogni elemento della lista può essere 
#un dataframe, o una lista di dataframe (al massimo 4..funziona con 3?)

#primaRiga: all'interno del foglio excel dove metto la prima riga?
#primaColonna: all'interno del foglio excel dove metto la prima colonna?

#shift.prime.tre.colonne: nel caso in cui le variabili descrittivi non sono tre ma una (ad esempio foglioSintesiFinaledelF) va gestito
#dove mettere il grassetto e dove inserire la scritta Total e quindi i successivi total. shift.prime.tre.colonne ha questo scopo

#mtg: applicare gli stili per i gap? (riga verde)-> opzione more.than.gaps

#cIniziale viene calcolato dal programma (dove metto la prima colonna quando ho più data frame nello stesso foglio excel?)
#Se non si è soddisfatti (dati troppo distanti) passare un valore intero a riducicIniziale

#togliNAN: sostituire NAN con 0?

scriviExcel<-function(my.wb,lista.sheet.name,lista.dati,
                      prime.tre.colonne=NULL,
                      primaRiga=2,
                      primaColonna=1,
                      shift.prime.tre.colonne=2,
                      mtg=TRUE,scriviTotali=TRUE,
                      convertiTotali=TRUE,
                      ANNOI=annoI,
                      ANNOF=annoF,
                      riducicIniziale=0,
                      togliNAN=TRUE){
  
  #se prime tre colonne non è nullo, allora la posizione di total e del bold la decido in base al numero di
  #colonne passate come "prime tre.colonne"
  if(!is.null(prime.tre.colonne)) (ncol(prime.tre.colonne)-1)->shift.prime.tre.colonne

  
  mapply(FUN=function(sheet.name,my.data){
    
    tryCatch({
      addWorksheet(my.wb,sheetName=sheet.name,tabColour ="white")
    },error=function(e){
      print("foglio già esistente, aggiungo i dati al foglio")
    })
    
    #distanza che separa i dati all'interno del foglio Excel
    if(scriviTotali & convertiTotali){
      distanza.dati<-7
    }else if(scriviTotali & !convertiTotali){
      distanza.dati<-6
#    }else if(!scriviTotali & !convertiTotali){
    }else{  
      distanza.dati<-5      
    }#fine if 
      
    #righe e colonne iniziali all'interno del foglio
    if(!is.data.frame(my.data)){                
      rIniziale<-list(primaRiga,(primaRiga-1)+nrow(my.data[[1]])+distanza.dati,primaRiga,(primaRiga-1)+nrow(my.data[[1]])+distanza.dati)
    }else{
      rIniziale<-list(primaRiga,(primaRiga-1)+nrow(my.data)+distanza.dati,primaRiga,(primaRiga-1)+nrow(my.data)+distanza.dati)      
    } 
    
    cIniziale<-list(primaColonna,primaColonna,(primaColonna-1)+(ANNOF-ANNOI+1+(1+shift.prime.tre.colonne)+3)-riducicIniziale,(primaColonna-1)+(ANNOF-ANNOI+1+(1+shift.prime.tre.colonne)+3-riducicIniziale))

      
    if(!is.data.frame(my.data)){                        
      length(my.data)->len.my.data
    }else{
      len.my.data<-1
    } 
    
    lista.indice<-as.list(c(1:len.my.data))

    mapply(FUN=function(indice){

      if(!is.null(prime.tre.colonne)){
        
        if(!is.data.frame(my.data)){                
          bind_cols(prime.tre.colonne,my.data[[indice]])->my.data2
        }else{
          bind_cols(prime.tre.colonne,my.data)->my.data2
        }
        
      }else{
        
        if(!is.data.frame(my.data)){                
          my.data[[indice]]->my.data2  
        }else{
          my.data->my.data2            
        }
        
      }#fine if

      ncol(my.data2)->colonne
      nrow(my.data2)->righe
  
      cIniziale[[indice]]->cini
      rIniziale[[indice]]->rini
      
      #metti zeri: sostituisci NAN con 0
      if(togliNAN) my.data2[sapply(my.data2,is.nan)]<-0
      
      writeData(my.wb,sheet.name,my.data2,startCol=cini,startRow=rini,keepNA=FALSE)
      if(scriviTotali){            
      #aggiungiamo i totali di riga
          t(colSums(my.data2[,(2+shift.prime.tre.colonne):ncol(my.data2)],na.rm=TRUE))->totali
          writeData(my.wb,sheet.name,"TOTAL",startCol=(primaColonna+shift.prime.tre.colonne)+cini-1,startRow=righe+2+rini,colNames=FALSE,rowNames=FALSE)
          writeData(my.wb,sheet.name,totali,startCol=(primaColonna+(shift.prime.tre.colonne+1))+cini-1,startRow=righe+2+rini,colNames=FALSE,rowNames=FALSE)
          if(convertiTotali){
            totali*(44/12)->totali
            writeData(my.wb,sheet.name,"kt (Gg) of CO2 equivalent",startCol=(primaColonna+shift.prime.tre.colonne)+cini-1,startRow=righe+3+rini,colNames=FALSE,rowNames=FALSE)
            writeData(my.wb,sheet.name,totali,startCol=(primaColonna+shift.prime.tre.colonne+1)+cini-1,startRow=righe+3+rini,colNames=FALSE,rowNames=FALSE)
          }#fine convertiTotali
      }#fine su scriviTotali
      
      #applica gli stili
      applicaStili(foglio=my.wb,sheet.name=sheet.name,righe=righe+3,colonne=colonne,more.than.gaps=mtg,colShift=cini,rowShift=rini,
                  shift.prime.tre.colonne = shift.prime.tre.colonne)->my.wb
      if(scriviTotali){
        #stile per i totali
        addStyle(my.wb,sheet=sheet.name,cols=(1+cini):(colonne+cini),rows=righe+2+rini,style=total.style,gridExpand=TRUE,stack=TRUE)
        if(convertiTotali) addStyle(my.wb,sheet=sheet.name,cols=(1+cini):(colonne+cini),rows=righe+3+rini,style=total.style.black,gridExpand=TRUE,stack=TRUE)                  
      }
    },indice=lista.indice)
    
  },sheet.name=lista.sheet.name,my.data=lista.dati) 
  
  return(my.wb)
  
}#fine scriviExcel



