library("reshape2")
library("openxlsx")
source("forestFunctions.R")
source("sommaColonne.R")
source("forestExcel.R")
#######################################################
#Lettura dei parametri per above e belowground biomass
#######################################################
leggi.parametri<-function(dir,lista){

	lapply(lista,FUN=function(nome.file){
	  
			print(nome.file)
	    read_delim(paste(".",dir,nome.file,sep="/"),delim=";",col_names=TRUE)->parametri
	  
			if(length(grep("above",tolower(nome.file)))){
				assign("abovep",parametri,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("ABOVEGROUND BIOMASS: file parametri %s letto\n",nome.file))
			}else if(length(grep("below",tolower(nome.file)))){
				assign("belowp",parametri,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("BELOWGROUND BIOMASS: file parametri %s letto\n",nome.file))
			}else if(length(grep("dead",tolower(nome.file)))){
				assign("deadwoodp",parametri,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("DEADWOOD COEFFICIENT: file parametri %s letto\n",nome.file))
			}else if(length(grep("litter",tolower(nome.file)))){
				assign("litterp",parametri,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("LITTER COEFFICIENT: file parametri %s letto\n",nome.file))
			}else if(length(grep("soil",tolower(nome.file)))){
				assign("soilp",parametri,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("SOIL COEFFICIENT: file parametri %s letto\n",nome.file))
			}else if(length(grep("mortality",tolower(nome.file)))){
				assign("mortalityp",parametri,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("NATURAL MORTALITY COEFFICIENT: file parametri %s letto\n",nome.file))
			}else if(length(grep("drain",tolower(nome.file)))){
				assign("drainGrazingp",parametri,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("NATURAL MORTALITY COEFFICIENT: file parametri %s letto\n",nome.file))
			}else if(length(grep("function",tolower(nome.file)))){
				assign("functionp",parametri,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("NATURAL MORTALITY COEFFICIENT: file parametri %s letto\n",nome.file))
			}else{	
			  message("#########################################")				  
				message(sprintf("-->> IGNORO IL FILE PARAMETRI %s",nome.file))
				message(sprintf("Il file %s probabilmente verrà utilizzato nelle elaborazioni successive",nome.file))				
			}


	})#fine lapply

}#fine leggi.parametri


#######################################################
#Lettura delle serie storiche: harvest, fires, surfaces 
#######################################################

#tot.surfaces: il file csv contiene gi? i totali. Ricalcolarli al volo? Non implementato
leggi.serie<-function(dir,lista,tot.surfaces=FALSE){

	lapply(lista,FUN=function(nome.file){
	  
	    trasforma<-function(x){
	      
	      if(!is.data.frame(x)) stop("x non è un data frame!")

	      if("yy" %in% names(x)){
  
  	      x %>% 
	          gather(key="inventory.tipology",value="volumi",-regione,-yy) %>% mutate(volumi=as.double(volumi))->x
	        
	      }else{
	        
	        x %>% 
	          gather(key="inventory.tipology",value="fattori",-regione) %>% mutate(fattori=as.double(fattori))->x	        
	        
	      }
	        
    
	      x$var3<-"stands"
	      x[grepl("^.+ c$",x$inventory.tipology),]$var3<-"coppices"
	      x[grepl("^.+ p$",x$inventory.tipology),]$var3<-"plantations"
	      x[grepl("^.+ o$",x$inventory.tipology),]$var3<-"other"
	      x[grepl("^.+ u$",x$inventory.tipology),]$var3<-"unstocked"			
	      
	      stringr::str_replace(x$inventory.tipology," .$","")->x$inventory.tipology
	      
	      x
	      
	    }#fine trasforma
	  
	  
			print(nome.file)
			#read.csv(file=paste(".",dir,nome.file,sep="/"),sep=";",head=TRUE,check.names=FALSE,stringsAsFactors=FALSE)->parametri
			suppressWarnings(read_delim(paste(".",dir,nome.file,sep="/"),delim=";",col_names=TRUE)->parametri)
			if(length(grep("fires",tolower(nome.file)))){
				assign("fires",parametri,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("FIRES: file serie %s letto\n",nome.file))
			}else if(length(grep("^h.+_input",tolower(nome.file)))){
        trasforma(x=parametri)->hInput
			  assign("harvests_input",hInput,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("HARVEST: file parametri %s letto\n",nome.file))
			}else if(length(grep("^h.+_proj",tolower(nome.file)))){
			  trasforma(x=parametri)->hProj
			  assign("harvests_proj",hProj,env=.GlobalEnv)
			  message(sprintf("HARVEST: file parametri %s letto\n",nome.file))
			}else if(length(grep("surfaces",tolower(nome.file)))){
				assign("surfaces",parametri,env=.GlobalEnv)
				message("#########################################")	
				message(sprintf("SURFACES: file parametri %s letto\n",nome.file))
			}else{	
				stop(sprintf("ERRORE:impossibile leggere il file serie %s",nome.file))
			}

	})#fine lapply


}#fine leggi.serie

#######################################################
#Verifica Serie: verifica che le serie lette (fires,harvest e surfaces) coprono lo stesso periodo
#Se le serie iniziano o finiscono in anni diversi pongo come annoinizio il minimo tra i vari anni di inizio
#e come anno di fine serie il massimo tra gli anni in cui termina ciascuna serie
#######################################################
verifica_anni_serie<-function(annoI,annoF,fires,harvests,surfaces){

	if(annoI>annoF){
		message("ERRORE: anno inizio elaborazioni (annoI) maggiore dell'anno fine elaborazioni (annoF)")
		message("-->>verificare file parametri<<--")
		stop()
	}

	range(fires$yy)->range.fires
	range(surfaces$yy)->range.surfaces
	range(harvests$yy)->range.harvests

	#verifica anno inizio
	if(range.fires[1]!=range.surfaces[1] || range.surfaces[1]!=range.harvests[1] ) 
		stop("Le serie harvest/fire/surface differiscono per l'anno di inzio")	

	#se arrivo qui le serie iniziano tutte e tre nello stesso anno
	anno.inizio.serie<-range.fires[1]

	#controllo annoI
	if(annoI<anno.inizio.serie){
		message(sprintf("-->>ATTENZIONE: annoI %s inferiore a anno inizio delle serie harvests/fires/surfaces",annoI))
		message(sprintf("-->>ATTENZIONE: pongo annoI uguale a %s",anno.inizio.serie))
		
		annoI<<-anno.inizio.serie 

	} 

	#verifica anno fine
	if(range.fires[2]!=range.surfaces[2] || range.surfaces[2]!=range.harvests[2] ){
		message("-->>ATTENZIONE: le serie harvests/fires/surfaces differiscono per anno fine") 
		anno.fine.serie<-min(range.fires[2],range.harvests[2],range.surfaces[2])		
		message(sprintf("-->>ATTENZIONE: la serie più breve termina nell'anno %s",anno.fine.serie))
	}else{
		#le tre serie terminano nello stesso anno
		anno.fine.serie<-range.fires[2]		
	}


	#controllo annoF
	if(annoF>anno.fine.serie){
		message(sprintf("-->> ATTENZIONE: annoF %s maggiore di anno fine delle serie harvests/fires/surfaces",annoF))
		message(sprintf("-->> ATTENZIONE: pongo annoF uguale a %s",anno.fine.serie))
		
		annoF<<-anno.fine.serie 

	} 
	

}#fine verifica_serie

######################################################
######################################################

#lista.in contiene i risultati di una determinata regione nei vari anni (ciascun elemento di lista.in è un foglio gs o
#un foglio ci. unisci.lista mette insieme i risultati delle varie regioni. Ad esempio, dopo aver eseguito unisci.lista
#il primo elemento di lista.out conterrà tutti i risultati del 1985 delle regioni.

#In realtà con unisci.lista mettiamo in uno stesso data.frame solo i risultati che vanno aggregati per calcolare i
#risultati per l'italia
	
unisci.lista<-function(lista.in=NA,lista.out=NA,nomi.variabili=NA,regione=NA){

	if(missing(lista.in) | missing(lista.out) | missing(nomi.variabili) | missing(regione)) stop()

	lapply(1:length(lista.in),FUN=function(indice){

		lista.in[[indice]][,nomi.variabili]->single.gs

		single.gs$yy<-names(lista.in)[indice]
		single.gs$regione<-regione

		rbind(lista.out[[indice]],single.gs)->lista.out[[indice]]

		lista.out[[indice]]

	})#fine lapply su lista.aboveground.out2

}#fine unisci.lista

######################################################
######################################################

#aggrega a livello nazionale i risultati regionali. lista.in deve avere come input l'output di
#unisci.lista
aggrega.campi.lista<-function(lista.in=NA){

	if(missing(lista.in)) stop()

	lapply(lista.in,FUN=function(single.gs){


		if(single.gs$pools[1]!="ci"){

			#aboveground ha anche fire e harvest
			if(length(grep("aboveground",single.gs$pools[1]))){

			  single.gs %>% dplyr::group_by(pools,management,var3,inventory.tipology) %>%
			    dplyr::summarise(surface=sum(surface,na.rm=TRUE),
			                     total.growing.stock=sum(total.growing.stock,na.rm=TRUE),
			                     total.biomass=sum(total.biomass,na.rm=TRUE),
			                     total.dry.matter=sum(total.dry.matter,na.rm=TRUE),
			                     total.carbon=sum(total.carbon,na.rm=TRUE),
			                     fire=sum(fire,na.rm=TRUE),harvest=sum(harvest,na.rm=TRUE))->out

			}else{

			  single.gs %>% dplyr::group_by(pools,management,var3,inventory.tipology) %>%
			    dplyr::summarise(surface=sum(surface,na.rm=TRUE),
			                     total.growing.stock=sum(total.growing.stock,na.rm=TRUE),
			                     total.biomass=sum(total.biomass,na.rm=TRUE),
			                     total.dry.matter=sum(total.dry.matter,na.rm=TRUE),
			                     total.carbon=sum(total.carbon,na.rm=TRUE))->out
			  
			}#su aboveground


		}else{

		  single.gs %>% dplyr::group_by(pools,management,var3,inventory.tipology) %>%
		    dplyr::summarise(	surface=sum(surface,na.rm=TRUE),
		                      total.current.increment=sum(total.current.increment,na.rm=TRUE),
		                      total.current.increment.biomass=sum(total.current.increment.biomass,na.rm=TRUE),
		                      total.current.increment.dry.matter=sum(total.current.increment.dry.matter,na.rm=TRUE),
		                      total.current.increment.carbon=sum(total.current.increment.carbon,na.rm=TRUE))->out  

		}#su ci

		as.character(out$pools)->out$pools
		as.character(out$management)->out$management
		as.character(out$var3)->out$var3
		as.character(out$inventory.tipology)->out$inventory.tipology
		length(unlist(invTipology))->len.invTip
		#prendiamo ile prime 28 righe..sort=FALSE..
		merge(single.gs[1:len.invTip,c("pools","management","var3","inventory.tipology")],
		out,by=c("pools","management","var3","inventory.tipology"),sort=FALSE)

		##ddply riordina le stringhe trasformandole in fattori
		#ridiamo all'output l'ordine desiderato utilizzando un truccho

	}) #fine lapply

}#FINE AGGREGA.LISTA


######################################################
######################################################

#MAIN PART OF THE PROGRAM
elabora<-function(lista.nomi.regioni=NA,
                  listaDatiRegioni=NA,
                  annoI=NA,
                  annoF=NA,
                  abovep=NA,
                  belowp=NA,
                  deadwoodp=NA,
                  drainGrazingp=NA,
                  functionp=NA,
                  litterp=NA,
                  mortalityp=NA,
                  soilp=NA,
                  fires=NA,
                  harvests=NA,
                  surfaces=NA,                  
                  CC2DM=NA,
                  CD2C=NA){

	if(missing(lista.nomi.regioni)) stop()

	#sequenza anni e numero anni
	seq(annoI,annoF)->seq.anni
	length(seq.anni)->len.anni

	#lista.italia: qui vanno i dati delle regioni. Ogni elemento della lista è un data.frame
	#che rappresenta un anno. Dentro ciascun data.frame hai i dati di tutte le regioni che poi andranno
	#aggregati (solo alcune variabili) per ottenere i risultati per Italia

	#la lista ha la lunghezza dei POOLS, più uno perchè abbiamo anche i risultati dei fogli ci
	lista.italia.out2<-vector(mode="list",length=(length(POOLS)+1))
	names(lista.italia.out2)<-c(POOLS,"ci")

	lapply(lista.nomi.regioni,FUN=function(nome.regione){

		#nome regione: desunto dal nome file
		message(sprintf("\nELABORO DATI PER REGIONE: %s",nome.regione))
	
	  dati<-listaDatiRegioni[[nome.regione]]
	  
		#annoI e annoF sono definite dall'utente nel file forestParameters.R

		#subset di fires/surfaces/harvests/functionp etc in base a regione

		lapply(c("harvests","surfaces","fires","functionp","deadwoodp","litterp","soilp","harvests_input"),FUN=function(xxx){

			print(sprintf("CARICO DATI PER PARAMETRO %s",xxx))
      get(xxx,envir=parent.env(environment()))->yyy
			#verifica che vi sia la colonna "regione" in ciascun file
			#names(eval(parse(text=xxx)))->nomi.xxx
			#which(nomi.xxx %in% "regione")->index
		  which(names(yyy) %in% "regione")->index
			if(length(index)!=1) stop(sprintf("PARAMETRO %s: manca la colonna regione, non posso proseguire",xxx))

			#subset(eval(parse(text=xxx)),eval(parse(text=paste0(xxx,"$regione")))==nome.regione)->x
      yyy[yyy$regione==nome.regione,]->x 		  
			sprintf("Subset dei dati per regione: %s",nome.regione)
			stopifnot(nrow(x)!=0)
			#assegna a sub.fires etc etc
			#assign(paste0("sub.",xxx),x,env=.GlobalEnv)
			assign(paste0("sub.",xxx),x,env=parent.env(environment()))		
			
			invisible()

		})#fine lapply

		###ora esistono oggetti sub.surfaces/sub.fires/sub.harvests/sub.functionp etc..
		#ciascun oggetto contiene i parametri per la regione oggetto di analisi
		############################

		message(sprintf("++ INIZIO ELABORAZIONE DATI REGIONALI: %s",annoI))
		message(sprintf("++ FINE ELABORAZIONE DATI REGIONALI: %s",annoF))

		#questo serve solo per i calcoli
		lista.aboveground.out<-vector(mode="list",length=len.anni)
		#qui scriviamo i data.frame che scriviamo nei file di output. Ogni data.frame rappresenta
		#un foglio gs nei file di Marina. 
		lista.aboveground.out2<-vector(mode="list",length=len.anni)
		lista.belowground.out2<-vector(mode="list",length=len.anni)
		lista.dead.out2<-vector(mode="list",length=len.anni)
		lista.soil.out2<-vector(mode="list",length=len.anni)
		lista.litter.out2<-vector(mode="list",length=len.anni)
		#contiene i fogli ci: ha un foglio in meno perchè ci comincia dall'86
		lista.ci.out2<-vector(mode="list",length=len.anni-1)

		#calcola incremento
		lapply(annoI:annoF,FUN=function(anno){
			
			#indice identifica dove memorizzare i dati (foglio xls) all'interno
			#di un file xls	
			indice<-(anno-annoI)+1

			calcoliFogliGS<-function(index=NA,yy=NA){

				if(missing(yy) | missing(index)) stop("errore parametro calcoliFogliGS")

				#calcoli per primo anno
				colXfire(x=lista.aboveground.out[[index]],sub.fires=sub.fires,anno=yy,azzera=azzera.poplar)->colonna.x
				
  			# if(anno> 2016){ #novembre 9 2018
  			#   browser()
  			# 	colAD(x=lista.aboveground.out[[index]],sub.harvests=sub.harvests,anno=yy,azzera=azzera.other)->colonna.ad
  			# 	colAE(x=lista.aboveground.out[[index]],sub.harvests=sub.harvests,anno=yy,azzera=azzera.other)->colonna.ae
  			# 	colAF(x=lista.aboveground.out[[index]],sub.harvests=sub.harvests,anno=yy,azzera=azzera.other)->colonna.af
  			# 	colAG(x=lista.aboveground.out[[index]],sub.harvests=sub.harvests,anno=yy,azzera=azzera.other)->colonna.ag
  			# 	colAH(x=lista.aboveground.out[[index]],azzera=azzera.other)->colonna.ah
  			# 	colAItotal(col.x=colonna.x,col.ad=colonna.ad,col.ah=colonna.ah,col.ae=colonna.ae,
  			# 	col.af=colonna.af,col.ag=colonna.ag)->colonna.ai
  			# 	colAJ(col.x=colonna.x,col.ad=colonna.ad,col.ae=colonna.ae,
  			# 	col.af=colonna.af,col.ag=colonna.ag,col.ah=colonna.ah,col.ai=colonna.ai,
  			# 	azzera=azzera.other)->colonna.aj
  			# 	colAK(col.aj=colonna.aj,col.ah=colonna.ah,col.ai=colonna.ai)->colonna.ak
  			# 	colAL(col.aj=colonna.aj,col.ak=colonna.ak)->colonna.al
  			# 	colAM(col.ai=colonna.ai,col.ak=colonna.ak,col.ah=colonna.ah,col.al=colonna.al)->colonna.am
  			# 	colAO(col.ah=colonna.ah,col.am=colonna.am,col.al=colonna.al)->colonna.ao
  			# 	colW(col.ai=colonna.ai,col.ak=colonna.ak,col.am=colonna.am,
  			# 	col.ao=colonna.ao,col.x=colonna.x,azzera=azzera.other)->colonna.w

				colonna.ad<-NULL
				colonna.ae<-NULL
				colonna.af<-NULL
				colonna.ag<-NULL
				colonna.ah<-NULL
				colonna.ai<-NULL
				colonna.aj<-NULL
				colonna.ak<-NULL
				colonna.al<-NULL
				colonna.am<-NULL
				colonna.ao<-NULL
				
				if(anno>=annoIntermedio){
				  print("############################# ANNO INTERMEDIO ELABORO<--------------------")
				  colonna.w<-NULL
				
				}else{
				  
				  #colonna.w fino al 2016
				  sub.harvests %>% 
				    filter(!is.na(yy) & yy==anno & !is.na(volumi)) %>%
				    rename(harvest=volumi) %>%
				    select(var3,inventory.tipology,harvest)->colonna.w  

				}#novembre 9 2018

				list("colonna.x"=colonna.x,"colonna.ad"=colonna.ad,"colonna.ae"=colonna.ae,
				     "colonna.af"=colonna.af,"colonna.ag"=colonna.ag,"colonna.ah"=colonna.ah,
				     "colonna.ai"=colonna.ai,"colonna.ae"=colonna.ae,"colonna.aj"=colonna.aj,
				     "colonna.ak"=colonna.ak,"colonna.al"=colonna.al,"colonna.am"=colonna.am,
				     "colonna.ao"=colonna.ao,"colonna.w"=colonna.w)	

			}#fine funzione


			if(indice==1){
			
				dati->new.dati

			}else{

				calcoliFogliGS(index=indice-1,yy=anno)->colgs

			  if(is.null(colgs$colonna.w)){ #dal 2017 in poi ....

			    left_join(sub.harvests_input,lista.aboveground.out[[indice-1]],by=c("var3"="var3","inventory.tipology"="inventory.tipology"))->temporaneo
			    colgs[["colonna.w"]]<-data.frame(var3=temporaneo$var3,inventory.tipology=temporaneo$inventory.tipology,harvest=temporaneo$total.growing.stock*temporaneo$fattori)

			  }			  
			  			  
				currentIncrement.per.ha(x=lista.aboveground.out[[indice-1]],
				col.w=colgs[["colonna.w"]],col.x=colgs[["colonna.x"]],
				drainGrazingp=drainGrazingp,mortalityp=mortalityp,sub.functionp=sub.functionp)->ci.colonna.e

				totalCurrentIncrement(surface=lista.aboveground.out[[indice-1]],col.e=ci.colonna.e)->ci.colonna.d
				totalCurrentIncrementBiomass(col.d=ci.colonna.d,abovep=abovep)->ci.colonna.g
				currentIncrementBiomass.per.ha(col.e=ci.colonna.e,abovep=abovep)->ci.colonna.h	
				totalCurrentIncrementDryMatter(col.g=ci.colonna.g,abovep=abovep)->ci.colonna.j
				currentIncrementDryMatter.per.ha(col.h=ci.colonna.h,abovep=abovep)->ci.colonna.k
				totalCurrentIncrementCarbon(col.j=ci.colonna.j,cc2dm=CC2DM)->ci.colonna.m
				currentIncrementCarbon.per.ha(col.k=ci.colonna.k,cc2dm=CC2DM)->ci.colonna.n
				totalCurrentIncrementCarbonDioxide(col.m=ci.colonna.m,cd2c=CD2C)->ci.colonna.p
				currentIncrementCarbonDioxide.per.ha(col.n=ci.colonna.n,cd2c=CD2C)->ci.colonna.q

				#scrittura del foglio ci		
				data.frame("pools"="ci","management"=dati$management,
				"var3"=dati$var3,"inventory.tipology"=dati$inventory.tipology,
				"surface"=lista.aboveground.out[[indice-1]]$surface,
				"total.current.increment"=ci.colonna.d$total.current.increment,
				"current.increment.per.ha"=ci.colonna.e$current.increment.per.ha,
				"total.current.increment.biomass"=ci.colonna.g$total.current.increment.biomass,
				"current.increment.biomass.per.ha"=ci.colonna.h$current.increment.biomass.per.ha,
				"total.current.increment.dry.matter"=ci.colonna.j$total.current.increment.dry.matter,
				"current.increment.dry.matter.per.ha"=ci.colonna.k$current.increment.dry.matter.per.ha,
				"total.current.increment.carbon"=ci.colonna.m$total.current.increment.carbon,
				"current.increment.carbon.per.ha"=ci.colonna.n$current.increment.carbon.per.ha,
				"total.current.increment.carbon.dioxide"=ci.colonna.p$total.current.increment.carbon.dioxide,
				"current.increment.carbon.dioxide.per.ha"=
				ci.colonna.q$current.increment.carbon.dioxide.per.ha,		
				stringsAsFactors=FALSE)->new.dati.ci

				lista.ci.out2[[indice-1]]<<-new.dati.ci	

				#Aggiornamento
				updateSurface(x=lista.aboveground.out[[indice-1]],sub.surfaces=sub.surfaces,anno=anno)->surface
				
				updateTotalGrowingStock(x=lista.aboveground.out[[indice-1]],
				col.d=ci.colonna.d,
				col.w=colgs[["colonna.w"]],
				col.x=colgs[["colonna.x"]],
				drainGrazingp=drainGrazingp,
				mortalityp=mortalityp)->total.growing.stock

				#all.x=FALSE ci serve perchè altrimenti facendo il  
				#merge non risulterebbe nell'output la riga "gaps"
				#che non compare nei file dei parametri
				data.frame("inventory.tipology"=dati$inventory.tipology,"surface"=surface$surface,
				"total.growing.stock"=total.growing.stock$total.growing.stock,"pools"=dati$pools,
				"management"=dati$management,"var3"=dati$var3,stringsAsFactors=FALSE)->new.dati

			}#fine su indice==0

			merge(new.dati,abovep,sort=FALSE,by=c("inventory.tipology","var3","pools","management"),
			all.x=FALSE)->mdati.above

			forest1(x=mdati.above,pools="aboveground",cc2dm=CC2DM,cd2c=CD2C)->out.above
			lista.aboveground.out[[(indice)]]<<-out.above

			#per il 1985 i calcoli vanno fatti sui dati di partenza, senza ricorrere
			#a quelli dell'anno precedente (yy=annoI) per tutti gli altri anni i calcoli
			#vanno fatti con yy=anno dove anno è uguale ll'anno precedente a quello in corso
			#di elaborazione
			if(indice==1) calcoliFogliGS(index=indice,yy=annoI)->colgs

			#lista.above out contiene i dati per l'elaborazione, 
			#creiamo un oggetto per la scrittura dei dati 
			#su foglio excel
			lista.aboveground.out2[[(indice)]]<<-lista.aboveground.out[[(indice)]]
			lista.aboveground.out2[[(indice)]]$harvest<<-colgs[["colonna.w"]]$harvest
			lista.aboveground.out2[[(indice)]]$fire<<-colgs[["colonna.x"]]$fire
		  lista.aboveground.out2[[(indice)]]$stands.coppices.plantations<<-colgs[["colonna.ad"]]$stands.coppices.plantations
			lista.aboveground.out2[[(indice)]]$not.assigned<<-colgs[["colonna.ae"]]$not.assigned
			lista.aboveground.out2[[(indice)]]$no.coppices<<-colgs[["colonna.af"]]$no.coppices
			lista.aboveground.out2[[(indice)]]$no.plantations<<-colgs[["colonna.ag"]]$no.plantations
			lista.aboveground.out2[[(indice)]]$one.third.gs<<-colgs[["colonna.ah"]]$one.third.gs
			lista.aboveground.out2[[(indice)]]$total<<-colgs[["colonna.ai"]]$total
			lista.aboveground.out2[[(indice)]]$deviation<<-colgs[["colonna.aj"]]$deviation
		lista.aboveground.out2[[(indice)]]$redistribution.per.category<<-colgs[["colonna.ak"]]$redistribution.per.category
			lista.aboveground.out2[[(indice)]]$deviation2<<-colgs[["colonna.al"]]$deviation2			
			lista.aboveground.out2[[(indice)]]$overall.redistribution<<-colgs[["colonna.am"]]$overall.redistribution
			lista.aboveground.out2[[(indice)]]$deviation3<<-colgs[["colonna.ao"]]$deviation3

			#below ground: prendiamo da out.above le variabili che ci servono per belowground
			which(names(lista.aboveground.out[[(indice)]]) %in% c("inventory.tipology","surface","growing.stock.per.ha","var3"))->colonne			
			stopifnot(length(colonne)!=0)
			lista.aboveground.out[[(indice)]][,colonne]->dati.below

			#ora prendiamo bef e basic density per belowground biomass
			merge(dati.below,belowp,sort=FALSE,by=c("inventory.tipology","var3"),all.x=FALSE)->mdati.below
			stopifnot(nrow(mdati.below)==28)
			forest1(x=mdati.below,pools="belowground",cc2dm=CC2DM,cd2c=CD2C)->out.below

			lista.belowground.out2[[indice]]<<-out.below

			#deadmass: prendiamo da out.above le variabili che ci servono per deadmass e litter
			which(names(lista.aboveground.out[[(indice)]]) %in% c("pools","management","inventory.tipology","surface","var3"))->colonne
			stopifnot(length(colonne)!=0)
			lista.aboveground.out[[(indice)]][,colonne]->dati.dead

			merge(dati.dead,sub.deadwoodp,sort=FALSE,by=c("inventory.tipology","var3"),all.x=FALSE)->mdati.dead
			stopifnot(nrow(mdati.dead)==28)
			forest1(x=mdati.dead,pools="dead",cc2dm=CC2DM,cd2c=CD2C)->out.dead

			lista.dead.out2[[indice]]<<-out.dead	

			#litter: prendiamo da out.above le variabili che ci servono per litter: sono 
			#le stesse colonne individuate per deadmass
			lista.aboveground.out[[(indice)]][,colonne]->dati.litter

			merge(dati.litter,sub.litterp,sort=FALSE,by=c("inventory.tipology","var3"),all.x=FALSE)->mdati.litter
			stopifnot(nrow(mdati.litter)==28)
			forest1(x=mdati.litter,pools="litter",cc2dm=CC2DM,cd2c=CD2C)->out.litter

			lista.litter.out2[[(indice)]]<<-out.litter		

			#soil: prendiamo da out.above le variabili che ci servono per litter
			which(names(lista.aboveground.out[[(indice)]]) %in% c("pools","management","inventory.tipology","surface","carbon.per.ha","var3"))->colonne
			lista.aboveground.out[[(indice)]][,colonne]->dati.soil

			merge(dati.soil,sub.soilp,sort=FALSE,by=c("inventory.tipology","var3"),
			all.x=FALSE)->mdati.soil
			stopifnot(nrow(mdati.soil)==28)
			forest1(x=mdati.soil,pools="soil",cc2dm=CC2DM,cd2c=CD2C)->out.soil	
	
			lista.soil.out2[[(indice)]]<<-out.soil		
			lista.dead.out2[[(indice)]]<<-out.dead	

			invisible()		

		})#fine lapply su anni

		#fuori da lapply cancelliamo directory della regione
		if(SCRIVI.XLS){	
			if(file.exists(nome.regione)) unlink(nome.regione,recursive=TRUE,force=TRUE)
			dir.create(nome.regione)
			dir.create(paste0(nome.regione,"/totali"))
			dir.create(paste0(nome.regione,"/outcomes"))
		}	
		
		#qui comincia la scrittura dei file e la procedura per accorpare in un'unico foglio
		#i dati di un anno determinato di tutte le regioni, per poi fare i calcoli a livello
		#nazionale
		lapply(c(POOLS,"ci"),FUN=function(fff){
			
			#nome.lista contiene il mome della lista con i dati di aboveground/belowground etc
			#di una determinata regione
			
		  #paste("lista",fff,"out2",sep=".")->nome.lista.in
			get(paste("lista",fff,"out2",sep="."))->temp.variable
	
			if(fff!="ci"){
				names(temp.variable)<-seq.anni
				prefisso<-"gs"
				#nomi variabili: sono le variabili che vanno sommate 
				#per ottenere il valore a livello
				#nazionale
				c("pools","management","var3","inventory.tipology",
				"surface","total.growing.stock","total.biomass",
				"total.dry.matter","total.carbon")->nomi.variabili
				
				#aboveground ha due variabili inpiù che dobbiamo recuperare
				if(fff=="aboveground") c(nomi.variabili,"fire","harvest")->nomi.variabili

			}else{
				names(temp.variable)<-seq.anni[-1]
				prefisso<-"ci"
				#nomi variabili: sono le variabili che vanno sommate 
				#per ottenere il valore a livello
				#nazionale
				c("pools","management","var3","inventory.tipology","surface",
				"total.current.increment","total.current.increment.biomass",
				"total.current.increment.dry.matter",
				"total.current.increment.carbon")->nomi.variabili
			}

			if(SCRIVI.XLS){

				paste0(nome.regione,"/",nome.regione)->prefix.name
				paste0(prefix.name,"_",fff,".xlsx")->nome.file

				scriviXLS(lista=temp.variable,nome=nome.file,prefixSheet=prefisso)

			}#if su scrivi.xls

			#nome regione va in un campo regione, in modo di poter distinguere
			#i dati all'interno di uno stesso foglio (anno)

			lista.italia.out2[[fff]]->temp.variabile.out
			#temp.variabile è ad esempio la lista dal 1985 al 2013 con tutti i risultati di aboveground
			#oppure la lista con tutti i risultatidi una certa regione per soil...
		
			#temp.variabile.out è una variabile di appoggio per fare l'append dei dati delle
			#arie regioni

			#lista.italia.out: ogni elemnto di questa lista è una lista. Il primo elemento
			#contiene la lista 1985 2013 per aboveground
			#la seconda lista i risultati 1985 2013 per belowground...etc

			#i risultati della regione Basilicata per ciascun pool (e.g. aboveground)
			#li aggiungiamo (rbind) alla lista che in lista.italia.out2 rappresenta
			#i risultati dello stesso pool (aboveground). risultato.unisci
			#conterrà i risultati di tutte le regioni di aboveground finora elaborate	
			unisci.lista(lista.in=temp.variable,lista.out=temp.variabile.out,
			nomi.variabili=nomi.variabili,regione=nome.regione)->risultato.unisci

			#risultato unisci lo riassegnamo alla lista (lista di liste) lista.italia.out2
			#risultato unisci contiene la lista di un determinato pool (aboveground per esempio)
			#dopo aver fatto l'append (rbind) dei dati di una nuova regione)
			lista.italia.out2[[fff]]<<-risultato.unisci	
		})#FINE LAPPLY CON FFF


#########################CALCOLA TOTALI PER CIASCUNA COLONNA DI CIASCUN FOGLIO
		#qui memorizziamo i risultati su cui costruire il foglio outcomes del F
		outcomes.del.f<-data.frame()
		outcomes<-data.frame()

		lapply(c(POOLS,"ci"),FUN=function(pool){

			#le funzioni per sommare le colone stanno in sommaColonne.R
			#stringa.lista<-paste0("lista.",pool,".out2")
			get(paste0("lista.",pool,".out2"))->temp.lista

			if(pool!="ci"){
				names(temp.lista)<-seq.anni
			}else{
				names(temp.lista)<-seq.anni[-1]
			}

			#il calcolo di belowground, i totali colonna, ha bisogno di total.growing.stock, che però
			#ha questo punto è tutto NA. Per mettere una pezza che non faccia saltare il programma
			#utilizziamo queste due righe qui sotto

			if(pool=="belowground"){
						
				lapply(1:length(lista.aboveground.out2),FUN=function(iii){
							
				lista.aboveground.out2[[iii]]$total.growing.stock->>temp.lista[[iii]]$total.growing.stock	

				})#fine lapply toppa
			}

			
			#altra pezza: ci ha bisogno di bef che sta in aboveground
			if(pool=="ci"){

				lapply(1:length(temp.lista),FUN=function(iii){

				  abovep$basic.density->>temp.lista[[iii]]$basic.density
					abovep$bef->>temp.lista[[iii]]$bef	

				})#fine lapply toppa
			}


			#funzione somma è la funzione che verrà chiamata per effettuare i calcoli
			#per i totali di ciascuna colonna
		  
#13maggio funzione.somma<-paste0("sommaColonne.",pool)
			
		print(pool)
		#funzione anonima per chiamare fun(lista.in) dove fun varia in base a lista.in
		#lista.in sono i fogli excel di una regione di uno specifico pool
		#fun è la funzione che calcola i totali. Il calcolo dei totali cambiano in base al pool
		#ed è per questo che abbiamo somma per aboveground..belowground etc

		class(temp.lista)<-c(pool,class(temp.lista))
		sommaColonne(lista=temp.lista,CC2DM=CC2DM,CD2C=CD2C)->somma.out
			
		melt(somma.out)->msomma.out
		#dcast(msomma.out,yy+pool~variable)->dsomma.out2
		msomma.out %>% dplyr::select(yy,pool,variable,value) %>% spread(key=variable,value=value)->dsomma.out
		#stopifnot(identical(dsomma.out,dsomma.out2))
		
		if(SCRIVI.XLS){
			#qui scriviamo tutti i totali colonna, un foglio per pool
			paste0(nome.regione,"/totali/")->prefix

			wb <- createWorkbook("Guido")

			(nrow(dsomma.out)+1)->righe
			ncol(dsomma.out)->colonne

			addWorksheet(wb,sheetName=pool)
			#funzione in forestExcel che applica stili a ogni foglio
			applicaStili(foglio=wb,sheet.name=pool,righe=righe,colonne=colonne)->wb	
 			writeData(wb,sheet=pool,dsomma.out)
 			#cambiato su richiesta di Marina
			saveWorkbook(wb,paste0(prefix,pool,".totali.",nome.regione,".xlsx"),overwrite=TRUE)

		}		

		#qui invece prepariamo un data.frame apposito per scrivere il foglio outcome del f
		if(pool!="ci"){
			rbind(outcomes.del.f,dsomma.out[,c("pool","yy","total.carbon.dioxideF")])->>outcomes.del.f

			rbind(outcomes,dsomma.out[,c("pool","yy","total.carbon.dioxide",
			"carbon.dioxide.per.ha","surface")])->>outcomes
		}#fine su pool !=ci

		})#fine lapply su POOLs


		#funzione per scrivere i fogli outcomes del f e outcomes
    scriviFogliOutcomes(nome.regione=nome.regione,outcomes=outcomes,outcomes.del.f=outcomes.del.f,CD2C=CD2C)

#########################CALCOLA TOTALI PER CIASCUNA COLONNA DI CIASCUN FOGLIO

	})#fine lapply per lettura tutte le ragioni

	#finita elaborazione per regioni

	
  ##############################################
	###################### INIZIA PARTE PER ITALIA	
	##############################################

	#se lista.nomi.regioni è 21 allora posso procedere a calcolare l'aggregato a livello italiano
	#altrimenti (nel caso in cui effettuo l'elaborazione solo per un numero ristretto di regioni) esco senza
	#calcolare i valori per l'Italia
	if(length(lista.nomi.regioni)!=21){
		print(sprintf("NUMERO REGIONI ELABORATE INFERIORE A %s; NON POSSO CALCOLARE L'AGGREGATO ITALIA",21))
		return() #esco e finisco il programma senza calcolare i valori per l'italia
	}else{
		print("PROCEDO A CALCOLARE I VALORI PER L'ITALIA")
	}

	#a questo punto ho tutti i risultati di tutte le regioni. Sono in lista.italia.out2
	#Ogni elemento di lista.italia.out2 è una lista: tante liste quante sono i pools più
	#una lista per i fogli "ci" che non sono pools ma sono i dati di passaggio tra un anno e l'altro

	if(SCRIVI.XLS){	
		if(file.exists(NOME.ITALIA)) unlink(NOME.ITALIA,recursive=TRUE,force=TRUE)
		dir.create(NOME.ITALIA)
		dir.create(paste0(NOME.ITALIA,"/totali/"))
		dir.create(paste0(NOME.ITALIA,"/outcomes/"))
	}#xls



	#inizia il lapply su italia
	#qui memorizziamo i risultati su cui costruire il foglio outcomes del F
	outcomes.del.f.italia<-data.frame()
	outcomes.italia<-data.frame()

	lapply(1:length(lista.italia.out2),FUN=function(ii){

		names(lista.italia.out2)[ii]->poolname

		aggrega.campi.lista(lista.in=lista.italia.out2[[ii]])->out.aggrega


		if(poolname!="ci"){
			prefisso<-"gs"

			lapply(out.aggrega,FUN=function(single.gs){
	
				#importante: region==FALSE
				forest1(x=single.gs,pools=poolname,cc2dm=CC2DM,cd2c=CD2C,region=FALSE)->zzz

				#dobbiamo aggiungere queste variabili per il calcolo dei totali italia
				#gli if sono valutati con il grep per evitare che falliscano se aboveground viene scritto con o senza il suffisso "biomass"
				#lo stesso per belowground che potrebbe comparire come "belowground" o come "belowground biomass"
				if(length(grep("above",poolname))==1) {
					if(!all(abovep$inventory.tipology==zzz$inventory.tipology)) stop("Inventory.tipology ordine diverso") #controllo paranoico
					#se fallisce la verifica qui sopra dovre sostituire l'assegnamento che segue con un merge (come fatto 
					#in tutto il resto del programma)				
					abovep$bef->zzz$bef
					abovep$basic.density->zzz$basic.density
				}else if(length(grep("below",poolname))==1){
					if(!all(belowp$inventory.tipology==zzz$inventory.tipology)) stop("Inventory.tipology ordine diverso") #controllo paranoico
					belowp$bef->zzz$bef
					belowp$basic.density->zzz$basic.density
				}

				return(zzz)

			})->risultato.finale

			names(risultato.finale)<-seq.anni
		}else{
			prefisso<-"ci"

			lapply(out.aggrega,FUN=function(single.gs){
	
single.gs$current.increment.per.ha<-single.gs$total.current.increment/single.gs$surface
single.gs$current.increment.biomass.per.ha<-single.gs$total.current.increment.biomass/single.gs$surface
single.gs$current.increment.dry.matter.per.ha<-single.gs$total.current.increment.dry.matter/single.gs$surface
single.gs$current.increment.carbon.per.ha<-single.gs$total.current.increment.carbon/single.gs$surface	
single.gs$total.current.increment.carbon.dioxide<-single.gs$total.current.increment.carbon*CD2C
single.gs$current.increment.carbon.dioxide.per.ha<-single.gs$total.current.increment.carbon.dioxide/single.gs$surface

			#anche a ci dobbiamo assegnare bef e basic.density presi da aboveground
			if(!all(abovep$inventory.tipology==single.gs$inventory.tipology)) stop("Inventory.tipology ordine diverso") #controllo paranoico
			#se fallisce la verifica qui sopra dovre sostituire l'assegnamento che segue con un merge (come fatto 
			#in tutto il resto del programma)				
			abovep$bef->single.gs$bef
			abovep$basic.density->single.gs$basic.density

			return(single.gs)
		
			})->risultato.finale

			names(risultato.finale)<-seq.anni[-1]
		}

		#risultato finale è una lista con tanti elementi quanti sono gli anni da elaborare
		#Ogni anno contiene i risultati di un determianto pool. I pool variano in base a lapply.
		#Ad esempio, al primo ciclo di lapply risultato.finale conterrà i risultati di "aboveground biomass"

		if(SCRIVI.XLS){	
			paste0(NOME.ITALIA,"/",NOME.ITALIA)->prefix.name
			paste0(prefix.name,"_",poolname,".xlsx")->nome.file
			scriviXLS(lista=risultato.finale,nome=nome.file,prefixSheet=prefisso)
		}#scrivi.xls

#########################CALCOLA TOTALI PER CIASCUNA COLONNA DI CIASCUN FOGLIO, per creare i dataframe outcomes e outcomes del F

#13maggio		funzione.somma<-paste0("sommaColonne.",poolname)
		#funzione anonima per chiamare fun(lista.in) dove fun varia in base a lista.in
		#lista.in sono i fogli excel di una regione di uno specifico pool
		#fun è la funzione che calcola i totali. Il calcolo dei totali cambiano in base al pool
		#ed è per questo che abbiamo somma per aboveground..belowground etc
		
		class(risultato.finale)<-c(poolname,class(risultato.finale))
		sommaColonne(lista=risultato.finale,CC2DM=CC2DM,CD2C=CD2C)->somma.out		
		
		melt(somma.out)->msomma.out
		dcast(msomma.out,yy+pool~variable)->dsomma.out

		if(SCRIVI.XLS){
			#qui scriviamo tutti i totali colonna, un foglio per pool

			paste0(NOME.ITALIA,"/totali/")->prefix

			wb <- createWorkbook("Guido")

			(nrow(dsomma.out)+1)->righe
			ncol(dsomma.out)->colonne

			addWorksheet(wb,sheetName=poolname)
			#funzione in forestExcel che applica stili a ogni foglio
			applicaStili(foglio=wb,sheet.name=poolname,righe=righe,colonne=colonne)->wb	
 			writeData(wb,sheet=poolname,dsomma.out)
			saveWorkbook(wb,paste0(prefix,poolname,".totali.xlsx"),overwrite=TRUE)	

		}		

		#qui invece prepariamo un data.frame apposito per scrivere il foglio outcome del f
		if(poolname!="ci"){
			rbind(outcomes.del.f.italia,dsomma.out[,c("pool","yy","total.carbon.dioxideF")])->>outcomes.del.f.italia
			rbind(outcomes.italia,dsomma.out[,c("pool","yy","total.carbon.dioxide",
			"carbon.dioxide.per.ha","surface")])->>outcomes.italia
		}#fine su pool !=ci

#########################CALCOLA TOTALI PER CIASCUNA COLONNA DI CIASCUN FOGLIO

			invisible()

	})#fine lapply su italia

	#scrivi fogli outcomes per Italia
    scriviFogliOutcomes(nome.regione=NOME.ITALIA,outcomes=outcomes.italia,outcomes.del.f=outcomes.del.f.italia,CD2C=CD2C)	

}#fine elabora
