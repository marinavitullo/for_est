growing_stock_per_ha<-function(total=NA,surface=NA){

	if(is.na(total)|| is.na(surface)) stop(paste0(match.call()[[1]],": parametri mancanti"))

	total/surface->ris
  ris[is.nan(ris)]<-0
	return(round(ris,DIGITS))

}#fine growing



####################################################
####################################################



#cc2dm conversion factor: carbon content/dry matter
#cd2c conversion factor: carbon dioxide/carbonr

#region=TRUE --> viene applicato il modello secondo lo schema regionale
#region=FALSE --> si deve utilizzare quando si procede all'utilizzo del modello per l'Italia. In questo caso
#diverse colonne (variabili) sono date dall'aggregazione dei valori delle singole regioni e non vanno calcolati
#secondo le formule dei fogli delle singole regioni. Quando region=FALSE i valori calcolati come aggregati
#regionali debbono già essere presenti in "x". Quando invece region=TRUE, in x debbono essere presenti solo le voci
#necessarie per i calcoli del modello Forest.
forest1<-function(x=NA,pools=NA,cc2dm,cd2c,region=TRUE){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(pools)) stop(paste0(match.call()[[1]],": parametro pools mancante"))

	as.character(pools)->pools

	if(!(pools %in% POOLS)) stop(paste0(match.call()[[1]],": parametro pools errato"))


	#solo per aboveground biomass
	if(pools==POOLS[1]){ #aboveground

		x$growing.stock.per.ha<-growing_stock_per_ha(surface=x$surface,total=x$total.growing.stock)

		#per singola regione va calcolato, per l'Italia + una somam dei valori Italiani.
		#Deve essere già presente in x
		
		#total_aboveground_biomass
		if(region) x$total.biomass<-round(x$bef * x$total.growing.stock,DIGITS)

		if(region){
			x$biomass.per.ha<-round(x$growing.stock.per.ha * x$bef,DIGITS)
		}else{
			x$biomass.per.ha<-x$total.biomass/x$surface
			x$biomass.per.ha[is.nan(x$biomass.per.ha)]<-0

		}#if region

	}else if(pools==POOLS[2]){ #belowground

		if(region){
			x$biomass.per.ha<-round(x$growing.stock.per.ha * x$bef,DIGITS)
			x$total.biomass<-(x$biomass.per.ha*x$surface)
		}else{
		  
			x$biomass.per.ha<-x$total.biomass/x$surface
			x$biomass.per.ha[is.nan(x$biomass.per.ha)]<-0

		}#fine region

	}


	if(pools==POOLS[1] || pools==POOLS[2]){

		if(region){ 
			x$total.dry.matter<- round(x$basic.density * x$total.biomass,DIGITS)
			x$dry.matter.per.ha<-round(x$biomass.per.ha * x$basic.density,DIGITS)	
		}else{	

			x$dry.matter.per.ha<-x$total.dry.matter/x$surface
			x$dry.matter.per.ha[is.nan(x$dry.matter.per.ha)]<-0
			
		}#fine region

	}else if(pools==POOLS[3]){ #DEAD

		if(region){
			x$dry.matter.per.ha<-x$peso
			x$total.dry.matter<-(x$surface*x$dry.matter.per.ha)
		}else{

			x$dry.matter.per.ha<-x$total.dry.matter/x$surface
			x$dry.matter.per.ha[is.nan(x$dry.matter.per.ha)]<-0
			
		}

	}#su pools


	if(pools==POOLS[1] || pools==POOLS[2] || pools==POOLS[3]){# not above below dead

		if(region){
			x$total.carbon<-(x$total.dry.matter*cc2dm)
			x$carbon.per.ha<-(x$dry.matter.per.ha*cc2dm)
		}else{

			x$carbon.per.ha<-x$total.carbon/x$surface
			x$carbon.per.ha[is.nan(x$carbon.per.ha)]<-0
			
		}#fine region

	}else{

		if(pools==POOLS[4]){#litter
			
			if(region){
				x$carbon.per.ha<-x$carbonio.organico.lettiera
			}else{
				x$carbon.per.ha<-rep(0,nrow(x))
				which(x$surface>0)->index0
				x[index0,]$carbon.per.ha<-x[index0,]$total.carbon/x[index0,]$surface
			}

		}else if(pools==POOLS[5]){#soil

			if(region){
				x$da.cancellare<-x$carbon.per.ha
				#a e b parametri letti nel file soil.csv
				temp<-(x$carbon.per.ha*x$b)+x$a
				x$carbon.per.ha<-temp
			}else{
				x$carbon.per.ha<-rep(0,nrow(x))
				which(x$surface>0)->index0
				x[index0,]$carbon.per.ha<-x[index0,]$total.carbon/x[index0,]$surface
			}#fine region

		}else{
			stop("ERRORE CALCOLO PARAMETRO")
		}

		if(region) x$total.carbon<-(x$carbon.per.ha*x$surface) # nel caso di soil e litter

	}#fine su pools

	#total.carbon.dioxide:non cambia il calcolo sia che region sia TRUE che sia FALSE
    	x$total.carbon.dioxide<-(x$total.carbon*cd2c)

	if(region){
		x$carbon.dioxide.per.ha<-(x$carbon.per.ha*cd2c)
	}else{

		x$carbon.dioxide.per.ha<-rep(0,nrow(x))
		which(x$surface>0)->index0
		x[index0,]$carbon.dioxide.per.ha<-x[index0,]$total.carbon.dioxide/x[index0,]$surface

	}


	if(pools!=POOLS[1]){
		x$total.growing.stock<-NA #questo non va annullato perchè per calcolare i totali colonna per pool belowground
		#in ciascuna regione abbiamo bisogno del total.growing.stock presente in aboveground. Dovremo annullarlo al
		#momento della scrittura su file
		x$growing.stock.per.ha<-NA	
	}


	if(pools!=POOLS[1] & pools!=POOLS[2]){

		x$total.biomass<-NA
		x$biomass.per.ha<-NA
	}


	if(pools!=POOLS[1] & pools!=POOLS[2] & pools!=POOLS[3]){

		x$total.dry.matter<-NA
		x$dry.matter.per.ha<-NA

	}

	
	#riscrittura colonna pools
 	if(pools==POOLS[1]){
		x$pools<-"aboveground biomass"
	}else if(pools==POOLS[2]){
		x$pools<-"belowground biomass"
	}else if(pools==POOLS[3]){
		x$pools<-"dead mass"
	
	}else{
			x$pools<-pools		
	}#fine 

	return(x)

}#fine.forest1

####################################################
####################################################


#Questa funzione viene utilizzata per annullare le voci all'interno dei risultati
#restituisce una lista di indici a cui corrispondono le voci da annullare
azzera.fun<-function(x=NA,voci=NA){

	indici<-c()
	stopifnot((length(voci)%%2)==0)
	#length(voci)->len
	
	for(jj in seq(2,length(voci),by=2)){

		which(x$var3==voci[jj-1] & x$inventory.tipology==voci[jj])->index.azzera
		stopifnot(length(index.azzera)!=0)
		indici<-c(indici,index.azzera)		

	}#fine ciclo for

	indici

}#fine azzera.fun

####################################################
####################################################



#azzera: le voci da annullare. Se NA nessuna voce viene annullata
#ad esempio: poplar stands viene annullato
colXfire<-function(x=NA,sub.fires=NA,anno=NA,azzera=NA){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
  if(missing(sub.fires)) stop(paste0(match.call()[[1]],": parametro sub.fires mancante"))
	if(missing(anno)) stop(paste0(match.call()[[1]],": parametro anno mancante"))

	#ris di default è 0
	ris<-rep(0,nrow(x))

	#il valore del parametro fuoco da utilizzare 
	sub.fires[sub.fires$yy==(anno),]$stands->valore.fuoco.stands
	sub.fires[sub.fires$yy==(anno),]$coppices->valore.fuoco.coppices

	denominatore.coppices<-NA
	denominatore.stands<-NA

	lapply(nomi.var3,FUN=function(vvv){

		#questo controllo non può mai fallire
		which(x$var3==vvv)->index.vvv0
		stopifnot(length(index.vvv0)!=0)

		#questo invece potrebbe fallire
		which(x$var3==vvv & x$surface!=0)->index.vvv

		#se index.vvv ha length > 0 allora calcola, altrimenti ris è 0
		if(length(index.vvv)){
			if(vvv=="coppices"){

				which( 
					(x$var3==vvv) | 
					(x$inventory.tipology==plantations[1] | x$inventory.tipology==plantations[2]) |
					(x$var3=="other" & x$inventory.tipology==other[3])
				)->indice.per.denominatore
			

				denominatore.coppices<<-sum(x[indice.per.denominatore,]$surface)
				denominatore<-denominatore.coppices
				valore.fuoco.coppices->valore.fuoco
			}else{
				#in tutti gli altri casi facciamo finta che siano stands, poi le correzioni delle voci particolari vengono
				#fatte una per una fuori da lapply
				which( 
					(x$var3=="stands") | 
				(x$inventory.tipology==plantations[4] | x$inventory.tipology==plantations[5] |  x$inventory.tipology==plantations[6]) |
					((x$var3=="other") & (x$inventory.tipology==other[1] | x$inventory.tipology==other[2]))
				)->indice.per.denominatore

				denominatore.stands<<-sum(x[indice.per.denominatore,]$surface)
				denominatore<-denominatore.stands
				valore.fuoco.stands->valore.fuoco

			}#vvv


		if(denominatore==0) stop(sprintf("ERRORE FUNZIONE %s: denominatore nullo",match.call()[[1]] ))
		#moltiplichiamo per il parametro di stands o coppices

		ris[index.vvv]<<-(valore.fuoco * x[index.vvv,]$surface/denominatore)*x[index.vvv,]$growing.stock.per.ha

		
		}#su if length(index.vvv)


	})#fine lapply

	#di default abbiamo calcolato tutto  quello che non era "coppices" come "stands". Ora dobbiamo 
	#correggere alcune voci di plantations e other che hanno un parametro valore.fuoco di volta in volta differente	

	#voci che correggiamo:
	#plantations: 
	which(
		(x$inventory.tipology==plantations[1] | x$inventory.tipology==plantations[2] | x$inventory.tipology==plantations[3]) & 
		(x$var3=="plantations")
	)->index.vvv

		ris[index.vvv]<-(valore.fuoco.coppices * x[index.vvv,]$surface/denominatore.coppices)*x[index.vvv,]$growing.stock.per.ha
	
	#other
	which(
		(x$inventory.tipology==other[3]) & (x$var3=="other")
	)->index.vvv

	ris[index.vvv]<-(valore.fuoco.coppices * x[index.vvv,]$surface/denominatore.coppices)*x[index.vvv,]$growing.stock.per.ha

	#azzera è un vettore di stringhe a coppie. Ogni coppia: la prima stringa rappresenta la voce in var3 e la seconda stringa
	#la voce in inventory.tipology
	if(length(azzera)){
		azzera.fun(x,voci=azzera)->hhh
		ris[hhh]<-VALORE.AZZERA
	}

	data.frame("var3"=x$var3,"inventory.tipology"=x$inventory.tipology,"fire"=round(ris,20)) ##<---questo nn lo toccare

}#fine fuoco

####################################################
####################################################
#colAD ridistribuisce l'harvest (stands, coppices, plantations) per per tutte le tipologie forestali sulla base del peso percentuale 
#della tipologia sul totale stands/coppices/plantations (in termini di growing stock)
#l'harvest assegnato alle plantation verr? poi ridistribuito alle categorie stands e coppices (colonne AJ, AK, AL, AM)
 
 colAD<-function(x=NA, sub.harvests=NA,anno=NA,azzera=NA){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(anno)) stop(paste0(match.call()[[1]],": parametro anno mancante"))
  if(missing(sub.harvests)) stop(paste0(match.call()[[1]],": parametro sub.harvests mancante"))
  

	ris<-rep(0,nrow(x))

	lapply(nomi.var3,FUN=function(vvv){
	
		#due controlli distinti: il primo non pu? mai fallire, perch? le voci 
		#di var3 devono tutte comparire nel file di input
		which(x$var3==vvv)->index.vvv0
		stopifnot(length(index.vvv0)!=0)

		#il secondo controllo potrebbe fallire, come ad esempio nel caso della Liguria che nel 1985
		#ha tutti i total.growing.stock per plantations pari a 0. Il controllo qui sotto poteva essere
		#scritto senza ritestare la variabile var3, per? per non modificare il codice che segue
		#non lo modifichiamo
		which(x$var3==vvv & x$total.growing.stock!=0)->index.vvv

		#se tutti i growing stock sono nulli, non tocchiamo "ris" che gi? contiene 0		

		if(length(index.vvv)){


			if(vvv!="other"){

				paste0("sub.harvests[sub.harvests$yy==(anno),]$",vvv,"->valore.raccolto")->stringa
				eval(parse(text=stringa))

				ris[index.vvv]<<-(valore.raccolto * x[index.vvv,]$total.growing.stock)/sum(x[index.vvv,]$total.growing.stock)

			}else{
				ris[index.vvv]<-0	
			}

		}#if su length index.vvv	

		invisible()

	})#fine su lapply


	#azzera è un vettore di stringhe a coppie. 
	#Ogni coppia: la prima stringa rappresenta la voce in var3 e la seconda stringa
	#la voce in inventory.tipology
	if(length(azzera)){
		azzera.fun(x,voci=azzera)->hhh
		ris[hhh]<-VALORE.AZZERA
	}

	data.frame("var3"=x$var3,"inventory.tipology"=x$inventory.tipology,"stands.coppices.plantations"=round(ris,DIGITS))

}#colAD function

####################################################
####################################################
#colAH calcola 1/3 del growing stock (anno n-1) per tutte le tipologie forestali


colAH<-function(x=NA,azzera=NA){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))

	(x$total.growing.stock/3)->ris

	#azzera è un vettore di stringhe a coppie. 
	#Ogni coppia: la prima stringa rappresenta la voce in var3 e la seconda stringa
	#la voce in inventory.tipology
	if(length(azzera)){
		azzera.fun(x,voci=azzera)->hhh
		ris[hhh]<-VALORE.AZZERA
	}


	data.frame("var3"=x$var3,"inventory.tipology"=x$inventory.tipology,"one.third.gs"=round(ris,DIGITS))

}#fine colonnaAH

####################################################
####################################################

## si puo' eliminare, era per la categoria "not assigned" ora non c'e' pi?

colAE<-function(x=NA,sub.harvests=NA,anno=NA,azzera=NA){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(anno)) stop(paste0(match.call()[[1]],": parametro anno mancante"))
  if(missing(sub.harvests)) stop(paste0(match.call()[[1]],": parametro sub.harvests mancante"))
  
	ris<-rep(0,nrow(x))

	sum(x$total.growing.stock,na.rm=TRUE)->fattore1

	#non può fallire
	which(x$var3=="other")->index.vvv
	stopifnot(length(index.vvv)!=0)

	sum(x[index.vvv,]$total.growing.stock,na.rm=TRUE)->fattore2

	differenza<-fattore1-fattore2

	if(differenza!=0){

		sub.harvests[sub.harvests$yy==anno,]$not.assigned->not.assigned
		ris<-(not.assigned*x$total.growing.stock/differenza)	


	}#fine if su differenza

	#azzera è un vettore di stringhe a coppie. 
	#Ogni coppia: la prima stringa rappresenta la voce in var3 e la seconda stringa
	#la voce in inventory.tipology
	if(length(azzera)){
		azzera.fun(x,voci=azzera)->hhh
		ris[hhh]<-VALORE.AZZERA
	}

	data.frame("var3"=x$var3,"inventory.tipology"=x$inventory.tipology,"not.assigned"=round(ris,DIGITS))

}#fine colAE

####################################################
####################################################
## if "no stands, coppices, plantations"... si pu? eliminare

colAF<-function(x=NA,sub.harvests=NA,anno=NA,azzera=NA){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(anno)) stop(paste0(match.call()[[1]],": parametro anno mancante"))
  if(missing(sub.harvests)) stop(paste0(match.call()[[1]],": parametro sub.harvests mancante"))
  
	ris<-rep(0,nrow(x))

	sum(x[x$var3=="plantations",]$total.growing.stock,na.rm=TRUE)->total.plantations
	sum(x[x$var3=="stands",]$total.growing.stock,na.rm=TRUE)->total.stands
	sum(x[x$var3=="coppices",]$total.growing.stock,na.rm=TRUE)->total.coppices

	if((total.stands+total.plantations)!=0){

		if(total.coppices==0){

			parametro<-sub.harvests[sub.harvests$yy==anno,]$coppices

			ris<-(parametro*x$total.growing.stock/(total.stands+total.plantations))


		}#if su total coppices


	}#fine if


	#azzera ? un vettore di stringhe a coppie. 
	#Ogni coppia: la prima stringa rappresenta la voce in var3 e la seconda stringa
	#la voce in inventory.tipology
	if(length(azzera)){
		azzera.fun(x,voci=azzera)->hhh
		ris[hhh]<-VALORE.AZZERA
	}


  data.frame("var3"=x$var3,"inventory.tipology"=x$inventory.tipology,"no.coppices"=round(ris,DIGITS))

}#fine colAF

####################################################
####################################################
## if "no stands, coppices, plantations"... si pu? eliminare

colAG<-function(x=NA,sub.harvests=NA,anno=NA,azzera=NA){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(anno)) stop(paste0(match.call()[[1]],": parametro anno mancante"))
  if(missing(sub.harvests)) stop(paste0(match.call()[[1]],": parametro sub.harvests mancante"))
  
	ris<-rep(0,nrow(x))

	sum(x[x$var3=="plantations",]$total.growing.stock,na.rm=TRUE)->total.plantations
	sum(x[x$var3=="stands",]$total.growing.stock,na.rm=TRUE)->total.stands
	sum(x[x$var3=="coppices",]$total.growing.stock,na.rm=TRUE)->total.coppices

	if((total.stands+total.coppices)!=0){

		if(total.plantations==0){

			parametro<-sub.harvests[sub.harvests$yy==anno,]$plantations

			ris<-(parametro*x$total.growing.stock/(total.stands+total.coppices))


		}#if su total coppices


	}#fine if


	#azzera è un vettore di stringhe a coppie. 
	#Ogni coppia: la prima stringa rappresenta la voce in var3 e la seconda stringa
	#la voce in inventory.tipology
	if(length(azzera)){
		azzera.fun(x,voci=azzera)->hhh
		ris[hhh]<-VALORE.AZZERA
	}


	data.frame("var3"=x$var3,"inventory.tipology"=x$inventory.tipology,"no.plantations"=round(ris,DIGITS))

}#fine colAG

####################################################
####################################################

#x ? il data.frame che contiene inventory.tipology, var3 e la variabile a cui applicare le percentuali
#percentuali contiene : inventory.tipology, var3  e le percentuali da applicare (deve contenere la parola percent)
#variabile: ? il nome della variabile in x a cui applicare le percentuali

applicaPercentuali<-function(x=NA,percentuali=NA,variabile=NA){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(percentuali)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(variabile)) stop(paste0(match.call()[[1]],": parametro x mancante"))

####### 21_11 marina############################################################
####### si blocca qui. probabimente perche' abbiamo bypassato la procedura per 
####### il calcolo dell'harvests a partire dal 2010 in poi 
####### lo calcoliamo con i fattori del file harvests_input
###############################################################################
  
  	merge(x,percentuali,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	stopifnot(nrow(newx)==28)

	#trova colonna che riporta la voce percent
	grep("percent",names(newx))->colonna.perc
	grep(variabile,names(newx))->colonna.var	

	#colonna mi dice dove sta la colonna delle percentuali da applicare in newx. 
	stopifnot(length(colonna.perc)==1)	
	stopifnot(length(colonna.var)==1)

	ris<-rep(0,nrow(percentuali))	

	which(newx[,c(colonna.perc)]!=0)->index
	stopifnot(length(index)!=0)

	ris[index]<-newx[index,c(colonna.var)]/(1-(newx[index,c(colonna.perc)]/100))*(newx[index,c(colonna.perc)]/100)	

  data.frame("var3"=newx$var3,"inventory.tipology"=newx$inventory.tipology,"total"=round(ris,DIGITS))
	

}#fine applica percentuali

####################################################
####################################################
#colAI calcola il totale dei growing stock (AS, AE, AF, AG), lo confronta con colAH (1/3 growing stock), e se minore 
#colAI non ha "azzera" perch? ? la somma di voci gi? annullate

colAItotal<-function(col.x=NA,col.ad=NA,col.ae=NA,col.af=NA,col.ag=NA,col.ah=NA){

	if(missing(col.x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(col.ad)) stop(paste0(match.call()[[1]],": parametro ad mancante"))
	if(missing(col.ae)) stop(paste0(match.call()[[1]],": parametro ae mancante"))
	if(missing(col.af)) stop(paste0(match.call()[[1]],": parametro af mancante"))
	if(missing(col.ag)) stop(paste0(match.call()[[1]],": parametro ag mancante"))
	if(missing(col.ah)) stop(paste0(match.call()[[1]],": parametro ah mancante"))

	merge(col.x,col.ad,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	merge(newx,col.ah,sort=FALSE,by=c("var3","inventory.tipology"))->newx5
	merge(newx5,col.ae,sort=FALSE,by=c("var3","inventory.tipology"))->newx3
	merge(newx3,col.af,sort=FALSE,by=c("var3","inventory.tipology"))->newx4
	merge(newx4,col.ag,sort=FALSE,by=c("var3","inventory.tipology"))->newx2
	stopifnot(nrow(newx2)==28)

	(newx2$fire)+(newx2$stands.coppices.plantations)+(newx2$not.assigned)+(newx2$no.plantations)+(newx2$no.coppices)->ris

	which(newx2$one.third.gs <= ris )->index

	if(length(index)) ris[index]<- newx2[index,]$one.third.gs

  data.frame("var3"=newx$var3,"inventory.tipology"=newx2$inventory.tipology,"total"=round(ris,DIGITS))


}#fine colonna AI

####################################################
####################################################
#colAJ calcola, per le plantations, le differenze tra il totale (colAI) la colAD (ridistribuzione harvest per categoria sulla base 
#del peso percentuale della tipologia inventariale nella categoria) 

#colAJ non ha "azzera" perchè è la somma di voci già annullate

colAJ<-function(col.x=NA,col.ad=NA,col.ae=NA,col.af=NA,col.ag=NA,col.ah=NA,col.ai=NA,azzera=NA){

	if(missing(col.x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(col.ad)) stop(paste0(match.call()[[1]],": parametro ad mancante"))
	if(missing(col.ae)) stop(paste0(match.call()[[1]],": parametro ae mancante"))
	if(missing(col.af)) stop(paste0(match.call()[[1]],": parametro af mancante"))
	if(missing(col.ag)) stop(paste0(match.call()[[1]],": parametro ag mancante"))
	if(missing(col.ah)) stop(paste0(match.call()[[1]],": parametro ah mancante"))
	if(missing(col.ai)) stop(paste0(match.call()[[1]],": parametro ah mancante"))

	merge(col.x,col.ad,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	merge(newx,col.ah,sort=FALSE,by=c("var3","inventory.tipology"))->newx6
	merge(newx6,col.ae,sort=FALSE,by=c("var3","inventory.tipology"))->newx3
	merge(newx3,col.af,sort=FALSE,by=c("var3","inventory.tipology"))->newx4
	merge(newx4,col.ag,sort=FALSE,by=c("var3","inventory.tipology"))->newx5
	merge(newx5,col.ai,sort=FALSE,by=c("var3","inventory.tipology"))->newx2

	stopifnot(nrow(newx2)==28)

	(newx2$fire)+(newx2$stands.coppices.plantations)+
	(newx2$not.assigned)+(newx2$no.plantations)+(newx2$no.coppices)-(newx2$total)->ris

	#azzera è un vettore di stringhe a coppie. 
	#Ogni coppia: la prima stringa rappresenta la voce in var3 e la seconda stringa
	#la voce in inventory.tipology
	if(length(azzera)){
		azzera.fun(newx2,voci=azzera)->hhh
		ris[hhh]<-VALORE.AZZERA
	}

	data.frame("var3"=newx$var3,"inventory.tipology"=newx2$inventory.tipology,"deviation"=round(ris,DIGITS))

}#fine colonna AJ

####################################################
####################################################
#colAk ridistribuisce l'harvest sulla base delle differenze (ColAJ)
# si potrebbe eliminare, visto che ? pari a 0

#colAk non ha "azzera" perch? ? la somma di voci gi? annullate
colAK<-function(col.aj=NA,col.ah=NA,col.ai=NA){

	if(missing(col.aj)) stop(paste0(match.call()[[1]],": parametro ag mancante"))
	if(missing(col.ah)) stop(paste0(match.call()[[1]],": parametro ah mancante"))
	if(missing(col.ai)) stop(paste0(match.call()[[1]],": parametro ah mancante"))

	merge(col.ah,col.ai,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	merge(newx,col.aj,sort=FALSE,by=c("var3","inventory.tipology"))->newx2

	stopifnot(nrow(newx2)==28)

	newx2$ris<-rep(0,nrow(newx2))

	which(newx2$one.third.gs!=newx2$total)->index0

	if(length(index0)){

		total.plantations<-sum(newx2[newx2$var3=="plantations",]$total)
		total.coppices<-sum(newx2[newx2$var3=="coppices",]$total)
		total.stands<-sum(newx2[newx2$var3=="stands",]$total)

		one.third.plantations<-sum(newx2[newx2$var3=="plantations",]$one.third.gs)
		one.third.coppices<-sum(newx2[newx2$var3=="coppices",]$one.third.gs)
		one.third.stands<-sum(newx2[newx2$var3=="stands",]$one.third.gs)

		deviation.plantations<-sum(newx2[newx2$var3=="plantations",]$deviation)
		deviation.coppices<-sum(newx2[newx2$var3=="coppices",]$deviation)
		deviation.stands<-sum(newx2[newx2$var3=="stands",]$deviation)
	
		calcola<-function(vvv=NA,fattore.deviation=NA,denominatore=NA){

			if(missing(vvv)) stop()
			if(missing(fattore.deviation)) stop()
			if(missing(denominatore)) stop()

			fattore1<-(
				fattore.deviation*
				(newx2[index0,][newx2[index0,]$var3==vvv,]$total-
				newx2[index0,][newx2[index0,]$var3==vvv,]$one.third.gs)/denominatore
			)


			fattore2<-(
				newx2[index0,][newx2[index0,]$var3==vvv,]$one.third.gs-
				newx2[index0,][newx2[index0,]$var3==vvv,]$total
			)


			fattore1-fattore2->differenza
			which(differenza<0)->index.diff

			if(length(index.diff)){
					
				newx2[index0,][newx2[index0,]$var3==vvv,]$ris[index.diff]<<-fattore1[index.diff]

			}


			which(differenza>=0)->index.diff

			if(length(index.diff)){

				newx2[index0,][newx2[index0,]$var3==vvv,]$ris[index.diff]<<-fattore2[index.diff]

			}



		}#fine calcola


		if(total.stands!=one.third.stands){ 
			calcola(vvv="stands",fattore.deviation=deviation.stands,
			denominatore=(total.stands-one.third.stands))}	
		if(total.coppices!=one.third.coppices){
			calcola(vvv="coppices",fattore.deviation=deviation.coppices,
			denominatore=(total.coppices-one.third.coppices))}	
		if(total.plantations!=one.third.plantations){
			calcola(vvv="plantations",fattore.deviation=deviation.plantations,
			denominatore=(total.plantations-one.third.plantations))}		


	}#fine su index0


	data.frame("var3"=newx$var3,"inventory.tipology"=newx2$inventory.tipology,"redistribution.per.category"=round(newx2$ris,DIGITS))


}#fine colonna AK

####################################################
####################################################
#colAL calcola le differenze tra la colAJ  e colAK. sempre solo plantations

colAL<-function(col.aj=NA,col.ak=NA){

	if(missing(col.aj)) stop(paste0(match.call()[[1]],": parametro ad mancante"))
	if(missing(col.ak)) stop(paste0(match.call()[[1]],": parametro ae mancante"))

	merge(col.aj,col.ak,sort=FALSE,by=c("var3","inventory.tipology"))->newx

	stopifnot(nrow(newx)==28)

	(newx$deviation)-(newx$redistribution.per.category)->ris


  data.frame("var3"=newx$var3,"inventory.tipology"=newx$inventory.tipology,"deviation2"=round(ris,DIGITS))


}#fine colonna AL

####################################################
####################################################
#colAM ridistribuisce le variazioni (colAL per plantations) su tutte le tipologie incluse in stands e coppices 

colAM<-function(col.ai=NA,col.ak=NA,col.ah=NA,col.al=NA){

	if(missing(col.al)) stop(paste0(match.call()[[1]],": parametro ad mancante"))
	if(missing(col.ai)) stop(paste0(match.call()[[1]],": parametro ad mancante"))
	if(missing(col.ak)) stop(paste0(match.call()[[1]],": parametro ae mancante"))
	if(missing(col.ah)) stop(paste0(match.call()[[1]],": parametro ad mancante"))

	merge(col.ah,col.ai,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	merge(newx,col.ak,sort=FALSE,by=c("var3","inventory.tipology"))->newx3
	merge(newx3,col.al,sort=FALSE,by=c("var3","inventory.tipology"))->newx2

	stopifnot(nrow(newx2)==28)

	newx2$ris<-rep(0,nrow(newx2))

	sum(col.ai$total,na.rm=TRUE)+sum(col.ak$redistribution.per.category,na.rm=TRUE)-
	sum(col.ah$one.third.gs,na.rm=TRUE)->denominatore

	sum(col.al$deviation2,na.rm=TRUE)->total.al

	if(denominatore!=0){

		(total.al*(col.ai$total+col.ak$redistribution.per.category-col.ah$one.third.gs)/denominatore)->fattore1
		(col.ah$one.third.gs-col.ai$total-col.ak$redistribution.per.category)->fattore2
				
		which(fattore1<fattore2)->index0
		if(length(index0)) newx2[index0,]$ris<-fattore1[index0]

		which(fattore1>=fattore2)->index0
		if(length(index0)) newx2[index0,]$ris<-fattore2[index0]

	}#fine if su sum	


	data.frame("var3"=newx2$var3,"inventory.tipology"=newx2$inventory.tipology,"overall.redistribution"=round(newx2$ris,DIGITS))


}#fine colonna AM

####################################################
####################################################
#colAO: controlla se la somma ? pari a 0 (se ho ridistribuito tutto l'harvest prima inputato alle plantations)

colAO<-function(col.ah=NA,col.am=NA,col.al=NA){

	if(missing(col.al)) stop(paste0(match.call()[[1]],": parametro ad mancante"))
	if(missing(col.am)) stop(paste0(match.call()[[1]],": parametro ad mancante"))
	if(missing(col.ah)) stop(paste0(match.call()[[1]],": parametro ad mancante"))

	ris<-rep(0,nrow(col.ah))

	totale.colonna.an<-(sum(col.am[col.am$var2!="other",]$overall.redistribution,na.rm=TRUE)-
			    sum(col.al[col.al$var2!="other",]$deviation2,na.rm=TRUE))

	if(totale.colonna.an!=0) stop("TOTATLE COLONNA AN NON E' UGUALE A 0")


	which(col.ah$one.third.gs==max(col.ah$one.third.gs,na.rm=TRUE))->index0

	if(length(index0)) ris[index0]<-totale.colonna.an

	data.frame("var3"=col.ah$var3,"inventory.tipology"=col.ah$inventory.tipology,"deviation3"=round(ris,DIGITS))

}#fine colonna AO


####################################################
####################################################
#colW: calcola l'harvest per singola tipologia, sulla base della ridistribuzioni fatta nelle colonne precendenti

colW<-function(col.ai=NA,col.ak=NA,col.am=NA,col.ao=NA,col.x=NA,azzera=NA){

	if(missing(col.ai)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(col.ak)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(col.am)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(col.ao)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(col.x)) stop(paste0(match.call()[[1]],": parametro x mancante"))

	merge(col.ai,col.x,sort=FALSE,by=c("var3","inventory.tipology"))->newx2
	merge(newx2,col.ak,sort=FALSE,by=c("var3","inventory.tipology"))->newx3
	merge(newx3,col.am,sort=FALSE,by=c("var3","inventory.tipology"))->newx4
	merge(newx4,col.ao,sort=FALSE,by=c("var3","inventory.tipology"))->newx

	stopifnot(nrow(newx)==28)

	newx$total+newx$redistribution.per.category+newx$overall.redistribution+newx$deviation3-newx$fire->ris


	#azzera ? un vettore di stringhe a coppie. 
	#Ogni coppia: la prima stringa rappresenta la voce in var3 e la seconda stringa
	#la voce in inventory.tipology
	if(length(azzera)){
		azzera.fun(newx,voci=azzera)->hhh
		ris[hhh]<-VALORE.AZZERA
	}

  data.frame("var3"=newx$var3,"inventory.tipology"=newx$inventory.tipology,"harvest"=round(ris,DIGITS))

}#fine colonnaw

####################################################
####################################################

updateSurface<-function(x=NA,sub.surfaces=NA,anno=NA){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(anno)) stop(paste0(match.call()[[1]],": parametro anno mancante"))
  if(missing(sub.surfaces)) stop(paste0(match.call()[[1]],": parametro sub.surfaces mancante"))
  
	parametro<-sub.surfaces[sub.surfaces$yy==anno,]$total

	stopifnot(parametro!=0)

	ris<-rep(0,nrow(x))

	which(x$surface>0)->index0

	if(!length(index0)) return(ris)


	ris<-x$surface/sum(x$surface)*parametro

  data.frame("var3"=x$var3,"inventory.tipology"=x$inventory.tipology,"surface"=round(ris,DIGITS))

			
}#fine funzione incrementColE

####################################################
####################################################


updateTotalGrowingStock<-function(x=NA,col.d=NA,col.w=NA,col.x=NA,drainGrazingp=NA,mortalityp=NA){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(col.d)) stop(paste0(match.call()[[1]],": parametro col.d mancante"))
	if(missing(col.w)) stop(paste0(match.call()[[1]],": parametro col.w mancante"))
	if(missing(col.x)) stop(paste0(match.call()[[1]],": parametro col.x mancante"))

	merge(x,col.d,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	merge(newx,col.w,sort=FALSE,by=c("var3","inventory.tipology"))->newx2
	merge(newx2,col.x,sort=FALSE,by=c("var3","inventory.tipology"))->newx3
	merge(newx3,drainGrazingp,sort=FALSE,by=c("var3","inventory.tipology"))->newx4
	merge(newx4,mortalityp,sort=FALSE,by=c("var3","inventory.tipology"))->newxx

	stopifnot(nrow(newxx)==28)

	ris<-rep(0,nrow(newxx))

	which(newxx$surface>0)->index0

	if(length(index0)){

		fattore1<-newxx[index0,]$total.growing.stock+newxx[index0,]$total.current.increment-
newxx[index0,]$fire-newxx[index0,]$harvest

		fattore2<-fattore1*(1-newxx[index0,]$percent.natural.mortality/100)
		ris[index0]<-fattore2*(1-newxx[index0,]$percent.drain.grazing/100)

	}#fine if su index0

	data.frame("var3"=newx3$var3,"inventory.tipology"=newx3$inventory.tipology,"total.growing.stock"=round(ris,DIGITS))

}#fine update function

####################################################
####################################################


currentIncrement.per.ha<-function(x=NA,col.w=NA,col.x=NA,drainGrazingp=NA,mortalityp=NA,sub.functionp=NA){

	if(missing(x)) stop(paste0(match.call()[[1]],": parametro x mancante"))
	if(missing(col.w)) stop(paste0(match.call()[[1]],": parametro col.w mancante"))
	if(missing(col.x)) stop(paste0(match.call()[[1]],": parametro col.x mancante"))
  if(missing(drainGrazingp)) stop(paste0(match.call()[[1]],": parametro drainGrazingp mancante"))
  if(missing(mortalityp)) stop(paste0(match.call()[[1]],": parametro mortalityp mancante"))
  if(missing(sub.functionp)) stop(paste0(match.call()[[1]],": parametro sub.functionp mancante"))


	merge(x,col.w,sort=FALSE,by=c("var3","inventory.tipology"),all.x=FALSE)->newx
	
	stopifnot(nrow(newx)==28)
	merge(newx,col.x,sort=FALSE,by=c("var3","inventory.tipology"),all.x=TRUE)->newx2
	stopifnot(nrow(newx2)==28)

	ris<-rep(0,nrow(x))
	which(newx2$surface>0)->index.surface.gt0

	if(length(index.surface.gt0)){


		#per essere sicuri di associare a inventory.tipology i corretti parametri, facciamo un merge
		merge(newx2,sub.functionp,sort=FALSE,by=c("var3","inventory.tipology"),all.x=FALSE)->newx3
		merge(newx3,mortalityp,sort=FALSE,by=c("var3","inventory.tipology"),all.x=FALSE)->newxx
		stopifnot(nrow(newxx)==28)

		fattore1<-rep(1,nrow(x))
	
		which(drainGrazingp$percent.drain.grazing>0)->index.grain
		(1-(drainGrazingp[index.grain,]$percent.drain.grazing/100))->fattore1[index.grain]

		newxx$total.growing.stock-newxx$harvest-newxx$fire->fattore2

		which(fattore2<0)->index0
		if(length(index0)) fattore2[index0]<-newxx$total.growing.stock[index0]

		(1-(newxx$percent.natural.mortality/100))->fattore3

		fattore0<-(fattore2*fattore3*fattore1/newxx$surface)

		(newxx$fun.k*fattore0/newxx$fun.n)->elemento0

		(elemento0-elemento0*((fattore0/newxx$fun.a)^newxx$fun.n)+newxx$fun.y0)->valori
		ris[index.surface.gt0]<-valori[index.surface.gt0]

	}#fine su if

	data.frame("var3"=newxx$var3,"inventory.tipology"=newxx$inventory.tipology,"current.increment.per.ha"=round(ris,DIGITS))

}#fine currentIncrement

####################################################
####################################################


totalCurrentIncrement<-function(surface=NA,col.e=NA){

	if(missing(surface)) stop(paste0(match.call()[[1]],": parametro surface mancante"))
	if(missing(col.e)) stop(paste0(match.call()[[1]],": parametro col.e mancante"))

	merge(surface,col.e,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	stopifnot(nrow(newx)==28)

	(newx$surface*newx$current.increment.per.ha)->ris

	data.frame("var3"=newx$var3,"inventory.tipology"=newx$inventory.tipology,"total.current.increment"=round(ris,DIGITS))
	
}#fine totalCurrentIncrement

####################################################
####################################################


totalCurrentIncrementBiomass<-function(col.d=NA,abovep=NA){

	if(missing(col.d)) stop(paste0(match.call()[[1]],": parametro col.d mancante"))
  if(missing(abovep)) stop(paste0(match.call()[[1]],": parametro abovep mancante"))
  
	merge(col.d,abovep,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	stopifnot(nrow(newx)==28)
	
	(newx$total.current.increment*newx$bef)->ris

	data.frame("var3"=newx$var3,"inventory.tipology"=newx$inventory.tipology,"total.current.increment.biomass"=round(ris,DIGITS))

}#fine totalCurrentIncrementBiomass

####################################################
####################################################


currentIncrementBiomass.per.ha<-function(col.e=NA,abovep=NA){

	if(missing(col.e)) stop(paste0(match.call()[[1]],": parametro col.e mancante"))
  if(missing(abovep)) stop(paste0(match.call()[[1]],": parametro abovep mancante"))
  
	merge(col.e,abovep,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	stopifnot(nrow(newx)==28)

	(newx$current.increment.per.ha*newx$bef)->ris

	data.frame("var3"=newx$var3,"inventory.tipology"=newx$inventory.tipology,"current.increment.biomass.per.ha"=round(ris,DIGITS))

}#fine currentIncrementBiomass.per.ha

####################################################
####################################################


totalCurrentIncrementDryMatter<-function(col.g=NA,abovep=NA){

	if(missing(col.g)) stop(paste0(match.call()[[1]],": parametro col.g mancante"))
  if(missing(abovep)) stop(paste0(match.call()[[1]],": parametro abovep mancante"))
  
	merge(col.g,abovep,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	stopifnot(nrow(newx)==28)

	(newx$total.current.increment.biomass)*(newx$basic.density)->ris

	data.frame("var3"=newx$var3,"inventory.tipology"=newx$inventory.tipology,"total.current.increment.dry.matter"=round(ris,DIGITS))


}#fine funzione

####################################################
####################################################


currentIncrementDryMatter.per.ha<-function(col.h=NA,abovep=NA){

	if(missing(col.h)) stop(paste0(match.call()[[1]],": parametro col.h mancante"))
  if(missing(abovep)) stop(paste0(match.call()[[1]],": parametro abovep mancante"))
  
	merge(col.h,abovep,sort=FALSE,by=c("var3","inventory.tipology"))->newx
	stopifnot(nrow(newx)==28)

	(newx$current.increment.biomass.per.ha)*(newx$basic.density)->ris

	data.frame("var3"=newx$var3,"inventory.tipology"=newx$inventory.tipology,"current.increment.dry.matter.per.ha"=round(ris,DIGITS))


}#fine funzione

####################################################
####################################################


totalCurrentIncrementCarbon<-function(col.j=NA,cc2dm=NA){

	if(missing(col.j)) stop(paste0(match.call()[[1]],": parametro col.j mancante"))
	if(missing(cc2dm)) stop(paste0(match.call()[[1]],": parametro cc2dm mancante"))

	col.j$total.current.increment.dry.matter*cc2dm->ris

	data.frame("var3"=col.j$var3,
	"inventory.tipology"=col.j$inventory.tipology,
	"total.current.increment.carbon"=round(ris,DIGITS))

}#fine funzione


currentIncrementCarbon.per.ha<-function(col.k=NA,cc2dm=NA){

	if(missing(col.k)) stop(paste0(match.call()[[1]],": parametro col.k mancante"))
	if(missing(cc2dm)) stop(paste0(match.call()[[1]],": parametro cc2dm mancante"))

	col.k$current.increment.dry.matter.per.ha*cc2dm->ris

	data.frame("var3"=col.k$var3,
	"inventory.tipology"=col.k$inventory.tipology,
  "current.increment.carbon.per.ha"=round(ris,DIGITS))

}#fine funzione

####################################################
####################################################

totalCurrentIncrementCarbonDioxide<-function(col.m=NA,cd2c=NA){

	if(missing(col.m)) stop(paste0(match.call()[[1]],": parametro col.m mancante"))
	if(missing(cd2c)) stop(paste0(match.call()[[1]],": parametro cd2c mancante"))

	col.m$total.current.increment.carbon*cd2c->ris

	data.frame("var3"=col.m$var3,
	"inventory.tipology"=col.m$inventory.tipology,
	"total.current.increment.carbon.dioxide"=round(ris,DIGITS))



}#fine funzione

####################################################
####################################################

currentIncrementCarbonDioxide.per.ha<-function(col.n=NA,cd2c=NA){


	if(missing(col.n)) stop(paste0(match.call()[[1]],": parametro col.n mancante"))
	if(missing(cd2c)) stop(paste0(match.call()[[1]],": parametro cd2c mancante"))

	col.n$current.increment.carbon.per.ha*cd2c->ris

	data.frame("var3"=col.n$var3,
	"inventory.tipology"=col.n$inventory.tipology,
	"current.increment.carbon.dioxide.per.ha"=round(ris,DIGITS))

}#fine funzione

####################################################
#Questa funzione NON può essere sostituita con scriviExcel (nel file forestExcel):
#forestExcel si occupa della formattazione dei file di output di sintesi. scriviXLS
#gestisce i file aboveground/belowground etc prodotto dal modello Forest,
####################################################
#
#questa funzione scrive i fogli xls, per ogni anno scrive un foglio. Le variabili vengono ordinate (le colonne)
#in base all'ordine dei fogli originali di MArina. Siccome le variabili tra i fogli gs e i fogli ci sono 
#differenti, il parametro prefixSheet viene utilizzato per i distinguo.

#lista: lista con i fogli che vanno nel file xls
#nome del file
#prefixSheet: gs o ci
scriviXLS<-function(lista=NA,nome=NA,prefixSheet=NA){

	if(missing(lista)) stop(paste0(match.call()[[1]],": parametro anno mancante")) 
	if(missing(nome)) stop(paste0(match.call()[[1]],": parametro anno mancante")) 

	APPEND<-FALSE
	
	names(lista)->anni

	wb <- createWorkbook("Guido")

	lapply(1:length(lista),FUN=function(ii){

		lista[[ii]]->single.gs
		
		#scriviamo i file xls in modo di avere output simili ai fogli xls di Marina
		single.gs[,c("pools","management","var3","inventory.tipology")]->newx

		if(prefixSheet=="gs"){

			#annulliamo alcune variabili per i pools diversi da aboveground
			if(length(grep(POOLS[1],single.gs$pools[1]))!=1){
				single.gs$surface<-NA 
				single.gs$total.growing.stock<-NA 				
			}#nei file di output queste due variabili sono diverse da NA solo per aboveground biomass
			#non le abbiamo potute annullare all'interno di "forest1" perchè queste variabili		
			#comunque ritornano nei calcoli dei totali di ciascuna regione e nei calcoli dei totali italia

			c("surface","total.growing.stock","growing.stock.per.ha","bef",
			"total.biomass","biomass.per.ha","basic.density","total.dry.matter","dry.matter.per.ha","total.carbon",
			"carbon.per.ha","total.carbon.dioxide","carbon.dioxide.per.ha")->nomi.variabili
			
			#per abovreground aggiungiamo alcune variabili nei file di output
			if(length(grep(POOLS[1],single.gs$pools[1]))==1){

				c(nomi.variabili,"harvest","fire","natural.mortality",
				"stands.coppices.plantations","not.assigned","no.coppices","one.third.gs",
				"total","deviation","redistribution.per.category","deviation2",
				"overall.redistribution","deviation3")->nomi.variabili	

			}#per aboveground biomass dobbiamo aggiungereanche le variabili fire harvest
			

		}else if(prefixSheet=="ci"){
			c("total.current.increment","current.increment.per.ha","bef",
			"total.current.increment.biomass","current.increment.biomass.per.ha",
			"total.current.increment.dry.matter","current.increment.dry.matter.per.ha",
			"total.current.increment.carbon","current.increment.carbon.per.ha",
			"total.current.increment.carbon.dioxide",
			"current.increment.carbon.dioxide.per.ha")->nomi.variabili
	
		}else{
			stop("FUNZIONE scriviXLS, prefixSheet non riconosciuto")
		}
				
		#in newx ordiniamo le variabili secondo l'ordine di nomi.variabili
		lapply(nomi.variabili,FUN=function(vvv){

				stringa<-paste0("newx$",vvv,"<<-single.gs$",vvv)
				eval(parse(text=stringa))
				invisible()

		})


		(nrow(newx)+1)->righe
		ncol(newx)->colonne

		addWorksheet(wb,sheetName=paste0(prefixSheet,anni[ii]))
		#funzione in forestExcel che applica stili a ogni foglio
		applicaStili(foglio=wb,sheet.name=paste0(prefixSheet,anni[ii]),righe=righe,colonne=colonne)->wb2
	        writeData(wb2,sheet=paste0(prefixSheet,anni[ii]),newx)

		wb2->>wb

#		write.xlsx(x=newx,file=nome,sheetName=paste0(prefixSheet,anni[ii]),
#		col.names=TRUE,row.names=FALSE,append=APPEND,showNA=TRUE)

#		APPEND<<-TRUE



	})#fine lapply

	saveWorkbook(wb,nome,overwrite=TRUE)

}#fine scriviXLS




