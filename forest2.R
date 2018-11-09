forest2<-function(x,pools,yy,cc2dm=CC2DM,cd2c=CD2C){

	if(missing(yy)) stop(paste0(match.call()[[1]],": parametro yy mancante"))
	if(missing(pools)) stop(paste0(match.call()[[1]],": parametro pools mancante"))

	as.character(pools)->pools

	if(!(pools %in% POOLS)) stop(paste0(match.call()[[1]],": parametro pools errato"))
	if(yy<1985) stop(paste0(match.call()[[1]],": parametro yy errato, minore di 1985"))

	#data.frame di output
	new.x<-x[,c("inventory.tipology","surface")]


	#solo per aboveground biomass
	if(pools=="aboveground"){

		#aggiorniamo le superfici che sono maggiori di 0
		which(x$surface>0)->index0
		if(!length(index0)) stop(sprintf("FUNZIONE forest2 pools %s: nessun dato di surface maggiore di 0",pools))
		
		#superfice totale
		sum(x$surface)->surfaceTotal
		(surfaces[surfaces$yy==(anno-1),]$totale.gaps)->totale.gaps
		denominatore<-(surfaceTotal*totale.gaps)
		new.x[index0,]$surface<-(x[index0,]$surface)/denominatore

		#calcolo total.growing.stock
		which(new.x$surface>0)->index0
		if(!length(index0)) stop(sprintf("FUNZIONE forest2 pools %s: nessun dato di surface maggiore di 0",pools))
		
		new.x[-index0,]$total.growing.stock<-0
		new.x[index0,]$total.growing.stock<-(x[index0,]$total.growing.stock+D-W-X)*(1-Z8)*(1-AB8)

	}		

	return(new.x)

}#fine.forest2


