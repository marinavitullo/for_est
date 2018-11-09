#funzioni per sommare le colonne all'interno di ciascuna regione. Queste sono le somme che compaiono all'interno dei fogli outcomes e outcomes del F.

#L'ultima funzione scrive i fogli outcomes e outcomes del F
#################################################################
#################################################################
sommaColonne=function(lista=NA,CC2DM=NA,CD2C=NA){
 UseMethod("sommaColonne",lista)
}

sommaColonne.aboveground<-function(lista=NA,CC2DM=NA,CD2C=NA){

	lapply(1:length(lista),FUN=function(iii){

				single.gs<-lista[[iii]]
				names(lista)[[iii]]->anno

				single.gs %>% dplyr::group_by(var3) ->single.gs.var3
				single.gs.var3 %>% dplyr::summarise(bef=sum(total.growing.stock*bef,na.rm=TRUE))->tgs
				surface<-sum(single.gs$surface,na.rm=TRUE)
				total.growing.stock<-sum(single.gs$total.growing.stock,na.rm=TRUE)	
				growing.stock.per.ha<-total.growing.stock/surface

				bef<-sum(tgs[,2],na.rm=TRUE)/total.growing.stock
				total.biomass<-total.growing.stock*bef
				biomass.per.ha<-growing.stock.per.ha*bef

				single.gs.var3 %>%  dplyr::summarise(basic.density=sum(total.biomass*basic.density,na.rm=TRUE)/sum(total.biomass,na.rm=TRUE))->tgs
				single.gs.var3 %>%  dplyr::summarise(tb=sum(total.biomass,na.rm=TRUE))->tgs2
				  
				basic.density<-sum(tgs[,2]*tgs2[,2],na.rm=TRUE)/sum(tgs2[,2])
				total.dry.matter<-basic.density*total.biomass
				dry.matter.per.ha<-basic.density*biomass.per.ha
				
				single.gs.var3 %>%  dplyr::summarise(conversion.factor=sum(total.dry.matter*CC2DM,na.rm=TRUE)/sum(total.dry.matter,na.rm=TRUE))->tgs
				single.gs.var3 %>%  dplyr::summarise(tdm=sum(total.dry.matter,na.rm=TRUE))->tgs2

				conversion.factor<-sum(tgs[,2]*tgs2[,2],na.rm=TRUE)/sum(tgs2[,2])
				total.carbon<-conversion.factor*total.dry.matter
				carbon.per.ha<-dry.matter.per.ha*conversion.factor
				#outcomes
				total.carbon.dioxide<-total.carbon*CD2C
				#total carbon dioxide outcomes del F
				total.carbon.dioxideF<-sum(single.gs[single.gs$var3=="stands",]$total.carbon.dioxide,na.rm=TRUE)+
										   sum(single.gs[single.gs$var3=="coppices",]$total.carbon.dioxide,na.rm=TRUE)+
								(single.gs[single.gs$var3=="plantations" & single.gs$inventory.tipology=="others",]$total.carbon.dioxide)+
							sum(single.gs[single.gs$var3=="other" & single.gs$inventory.tipology!="shrublands",]$total.carbon.dioxide,na.rm=TRUE)
 
					carbon.dioxide.per.ha<-carbon.per.ha*CD2C
					harvest<-sum(single.gs$harvest,na.rm=TRUE)
					fire<-sum(single.gs$fire,na.rm=TRUE)
					one.third.gs<-sum(single.gs$one.third.gs,na.rm=TRUE)
					total<-sum(single.gs$total,na.rm=TRUE)

					data.frame(yy=anno,pool=single.gs$pools[1],
					surface,total.growing.stock,growing.stock.per.ha,bef,total.biomass,biomass.per.ha,
					basic.density,total.dry.matter,dry.matter.per.ha,conversion.factor,total.carbon,
					carbon.per.ha,total.carbon.dioxide,total.carbon.dioxideF,
					carbon.dioxide.per.ha,harvest,fire,one.third.gs,total)

	})#fine lapply
		
}#fine sommaColonne

#################################################################à
#################################################################à

sommaColonne.belowground<-function(lista=NA,CC2DM=NA,CD2C=NA){
	
	lapply(1:length(lista),FUN=function(iii){

				single.gs<-lista[[iii]]
				names(lista)[[iii]]->anno

				single.gs %>% dplyr::group_by(var3) ->single.gs.var3
				single.gs.var3 %>% dplyr::summarise(bef=sum(total.growing.stock*bef,na.rm=TRUE))->tgs

				surface<-sum(single.gs$surface,na.rm=TRUE)
				total.growing.stock<-sum(single.gs$total.growing.stock,na.rm=TRUE)	
				bef<-sum(tgs[,2],na.rm=TRUE)/total.growing.stock

				total.biomass<-sum(single.gs$total.biomass,na.rm=TRUE)
				biomass.per.ha<-total.biomass/surface

				single.gs.var3 %>% dplyr::summarise(basic.density=sum(total.biomass*basic.density,na.rm=TRUE)/sum(total.biomass,na.rm=TRUE))->tgs
				single.gs.var3 %>% dplyr::summarise(tb=sum(total.biomass,na.rm=TRUE))->tgs2
				basic.density<-sum(tgs[,2]*tgs2[,2],na.rm=TRUE)/sum(tgs2[,2])
				
				total.dry.matter<-basic.density*total.biomass
				dry.matter.per.ha<-total.dry.matter/surface

				single.gs.var3 %>% dplyr::summarise(conversion.factor=sum(total.dry.matter*CC2DM,na.rm=TRUE)/sum(total.dry.matter,na.rm=TRUE))->tgs
				single.gs.var3 %>% dplyr::summarise(tdm=sum(total.dry.matter,na.rm=TRUE))->tgs2
				
				#conversion.factor<-sum(tgs$conversion.factor,na.rm=TRUE)/total.dry.matter
				conversion.factor<-sum(tgs[,2]*tgs2[,2],na.rm=TRUE)/sum(tgs2[,2])
				total.carbon<-conversion.factor*total.dry.matter
				carbon.per.ha<-total.carbon/surface
				total.carbon.dioxide<-total.carbon*CD2C
				#total carbon dioxide outcomes del F
				total.carbon.dioxideF<-sum(single.gs[single.gs$var3=="stands",]$total.carbon.dioxide,na.rm=TRUE)+
										   sum(single.gs[single.gs$var3=="coppices",]$total.carbon.dioxide,na.rm=TRUE)+
								(single.gs[single.gs$var3=="plantations" & single.gs$inventory.tipology=="others",]$total.carbon.dioxide)+
							sum(single.gs[single.gs$var3=="other" & single.gs$inventory.tipology!="shrublands",]$total.carbon.dioxide,na.rm=TRUE)

				data.frame(yy=anno,pool=single.gs$pools[1],
				surface=surface,
				total.growing.stock=NA,growing.stock.per.ha=NA,bef,total.biomass,biomass.per.ha,
				basic.density,total.dry.matter,dry.matter.per.ha,conversion.factor,total.carbon,
				carbon.per.ha,total.carbon.dioxide,total.carbon.dioxideF,carbon.dioxide.per.ha=carbon.per.ha*CD2C)

	})#fine lapply
		
}#fine sommaColonne bewloground


#################################################################à
#################################################################à

sommaColonne.dead<-function(lista=NA,CC2DM=NA,CD2C=NA){
	
	lapply(1:length(lista),FUN=function(iii){

				single.gs<-lista[[iii]]
				names(lista)[[iii]]->anno

				surface<-sum(single.gs$surface,na.rm=TRUE)
				total.dry.matter<-sum(single.gs$total.dry.matter,na.rm=TRUE)
				dry.matter.per.ha<-total.dry.matter/surface
				single.gs %>% dplyr::group_by(var3) %>% dplyr::summarise(conversion.factor=sum(total.dry.matter*CC2DM,na.rm=TRUE))->tgs

				conversion.factor<-sum(tgs[,2],na.rm=TRUE)/total.dry.matter

				total.carbon<-conversion.factor*total.dry.matter
				carbon.per.ha<-dry.matter.per.ha*conversion.factor

				total.carbon.dioxide<-total.carbon*CD2C
				#total carbon dioxide outcomes del F
				total.carbon.dioxideF<-sum(single.gs[single.gs$var3=="stands",]$total.carbon.dioxide,na.rm=TRUE)+
										   sum(single.gs[single.gs$var3=="coppices",]$total.carbon.dioxide,na.rm=TRUE)+
								(single.gs[single.gs$var3=="plantations" & single.gs$inventory.tipology=="others",]$total.carbon.dioxide)+
							sum(single.gs[single.gs$var3=="other" & single.gs$inventory.tipology!="shrublands",]$total.carbon.dioxide,na.rm=TRUE)

				data.frame(yy=anno,pool=single.gs$pools[1],
				surface=surface,total.growing.stock=NA,growing.stock.per.ha=NA,bef=NA,total.biomass=NA,
				biomass.per.ha=NA,basic.density=NA,
				total.dry.matter,dry.matter.per.ha,conversion.factor,total.carbon,
				carbon.per.ha,total.carbon.dioxide,total.carbon.dioxideF,carbon.dioxide.per.ha=carbon.per.ha*CD2C)

	})#fine lapply
		
}#fine sommaColonne dead

#################################################################
#################################################################à


sommaColonne.litter<-function(lista=NA,CC2DM=NA,CD2C=NA){
	
	lapply(1:length(lista),FUN=function(iii){

				single.gs<-lista[[iii]]
				names(lista)[[iii]]->anno

				surface<-sum(single.gs$surface,na.rm=TRUE)
				total.carbon<-sum(single.gs$total.carbon,na.rm=TRUE)
				carbon.per.ha<-total.carbon/surface
				total.carbon.dioxide<-total.carbon*CD2C
				#total carbon dioxide outcomes del F
				#Sbagliata: total.carbon.dioxideF<-sum(single.gs$total.carbon.dioxide,na.rm=TRUE)
				total.carbon.dioxideF<-sum(single.gs[single.gs$var3=="stands",]$total.carbon.dioxide,na.rm=TRUE)+
										   sum(single.gs[single.gs$var3=="coppices",]$total.carbon.dioxide,na.rm=TRUE)+
								(single.gs[single.gs$var3=="plantations" & single.gs$inventory.tipology=="others",]$total.carbon.dioxide)+
							sum(single.gs[single.gs$var3=="other" & single.gs$inventory.tipology!="shrublands",]$total.carbon.dioxide,na.rm=TRUE)

					data.frame(yy=anno,pool=single.gs$pools[1],
					surface=surface,total.growing.stock=NA,growing.stock.per.ha=NA,bef=NA,total.biomass=NA,
					biomass.per.ha=NA,basic.density=NA,
					total.dry.matter=NA,dry.matter.per.ha=NA,conversion.factor=NA,total.carbon,
					carbon.per.ha,total.carbon.dioxide,total.carbon.dioxideF,carbon.dioxide.per.ha=carbon.per.ha*CD2C)

	})#fine lapply
		
}#fine sommaColonne dead

#################################################################à
#################################################################à


sommaColonne.soil<-sommaColonne.litter

#################################################################à
#################################################################à


sommaColonne.ci<-function(lista=NA,CC2DM=NA,CD2C=NA){

	lapply(1:length(lista),FUN=function(iii){

				single.gs<-lista[[iii]]
				names(lista)[[iii]]->anno

				total.current.increment<-sum(single.gs$total.current.increment,na.rm=TRUE)
				surface<-sum(single.gs$surface,na.rm=TRUE)
				current.increment.per.ha<-total.current.increment/surface

				single.gs %>% dplyr::group_by(var3) ->single.gs.var3
				single.gs.var3 %>% dplyr::summarise(bef=sum(total.current.increment*bef,na.rm=TRUE)/sum(total.current.increment,na.rm=TRUE))->tgs
				single.gs.var3 %>% dplyr::summarise(tci=sum(total.current.increment,na.rm=TRUE))->tgs2
				
				bef<-sum((tgs2$tci*tgs$bef) /sum(tgs2$tci),na.rm=T)
				total.current.increment.biomass<-total.current.increment*bef
				current.increment.biomass.per.ha<-current.increment.per.ha*bef

        #	single.gs$basic.density<-single.gs$total.current.increment.dry.matter/single.gs$total.current.increment.biomass
	      single.gs.var3 %>% dplyr::summarise(basic.density=sum(total.current.increment.biomass*basic.density,na.rm=TRUE)/sum(total.current.increment.biomass,na.rm=TRUE))->tgs
	      single.gs.var3 %>% dplyr::summarise(tcib=sum(total.current.increment.biomass,na.rm=TRUE))->tgs2

        basic.density<-sum((tgs2$tcib*tgs$basic.density) /sum(tgs2$tcib),na.rm=T)
	
				total.current.increment.dry.matter<-total.current.increment.biomass*basic.density
				current.increment.dry.matter.per.ha<-basic.density*current.increment.biomass.per.ha
				
	      single.gs.var3 %>% dplyr::summarise(conversion.factor=sum(total.current.increment.dry.matter*CC2DM,na.rm=TRUE)/sum(total.current.increment.dry.matter,na.rm=TRUE))->tgs
	      single.gs.var3 %>% dplyr::summarise(tcidm=sum(total.current.increment.dry.matter,na.rm=TRUE))->tgs2
		
	      conversion.factor<-sum((tgs2$tcidm*tgs$conversion.factor) /sum(tgs2$tcidm),na.rm=T)					
				total.current.increment.carbon<-conversion.factor*total.current.increment.dry.matter
				current.increment.carbon.per.ha<-current.increment.dry.matter.per.ha*conversion.factor
				total.current.increment.carbon.dioxide<-total.current.increment.carbon*CD2C
				current.increment.carbon.dioxide.per.ha<-current.increment.carbon.per.ha*CD2C

				data.frame(yy=anno,pool=single.gs$pools[1],total.current.increment,current.increment.per.ha,bef,
				total.current.increment.biomass,current.increment.biomass.per.ha,
				basic.density,total.current.increment.dry.matter,current.increment.dry.matter.per.ha,
				conversion.factor,total.current.increment.carbon,current.increment.carbon.per.ha,
				total.current.increment.carbon.dioxide,current.increment.carbon.dioxide.per.ha)
				

	})#fine lapply
		
}#fine sommaColonne ci


#questa funzione scrive i fogli outcomes e outcomes del f
scriviFogliOutcomes<-function(nome.regione,outcomes,outcomes.del.f,CD2C){

		if(missing(nome.regione) | missing(outcomes) | missing(outcomes.del.f) | missing(CD2C)) stop("scriviFogliOutcomes: verificare parametri")

		#ora possiamo scrivere foglio outcomes per la singola regione
		#utilizzando la variabile total.carbon.dioxide
		melt(outcomes,c("yy","pool"))->moutcomes
		split(moutcomes,f=moutcomes$variable)->loutcomes

		if(SCRIVI.XLS){
			paste0(nome.regione,"/outcomes/")->prefix
			dcast(loutcomes[[1]],yy~pool,value.var="value")->temporaneo1
			#aggiungo colonna total
			temporaneo1$total<-rowSums(temporaneo1[,-c(1)])

			wb <- createWorkbook("Guido")
			
			(nrow(temporaneo1)+1)->righe
			ncol(temporaneo1)->colonne
			
			sheet.name<-"t.of.CO2.equivalent"

		  addWorksheet(wb,sheet.name)
			#funzione in forestExcel che applica stili a ogni foglio
			applicaStiliOutcomes(foglio=wb,sheet.name=sheet.name,righe=righe,colonne=colonne)->wb
 			writeData(wb,sheet.name,temporaneo1)

			#write.xlsx(x=temporaneo1,
			#file=paste0(prefix,nome.regione,"_carbon_stock.outcomes.xlsx"),col.names=TRUE,
			#row.names=FALSE,append=FALSE,showNA=TRUE,sheetName="t.of.CO2.equivalent")

			dcast(loutcomes[[2]],yy~pool,value.var="value")->temporaneo2

			#aggiungo colonna total
			temporaneo2$total<-rowSums(temporaneo2[,-c(1)])	

			(nrow(temporaneo2)+1)->righe
			ncol(temporaneo2)->colonne
			
			sheet.name<-"t.of.CO2.equivalent.per.ha"
		        addWorksheet(wb,sheet.name)
			applicaStiliOutcomes(foglio=wb,sheet.name=sheet.name,righe=righe,colonne=colonne)->wb
 			writeData(wb,sheet.name,temporaneo2)			

			#write.xlsx(x=temporaneo2,
			#file=paste0(prefix,nome.regione,"_carbon_stock.outcomes.xlsx"),col.names=TRUE,
			#row.names=FALSE,append=TRUE,showNA=TRUE,sheetName="t.of.CO2.equivalent.per.ha")	

			#superficie
			dcast(loutcomes[[3]],yy~pool,value.var="value")[,c(2)]->superficie

			#temporaneo1 e temporaneo2 contengono le colonne total			
			data.frame("yy"=temporaneo1[,c("yy")])->temporaneo3
			data.frame("yy"=temporaneo2[,c("yy")])->temporaneo4

			lapply(2:ncol(temporaneo1),FUN=function(ii){
				c(NA,diff(temporaneo1[,c(ii)]))->temp3
				temp3->temp4
				temporaneo3$temp3<-temp3

				temporaneo4$temp4<-temp4/superficie
				#in modi più semplici non riesco a cambiare il nome, probabilmente dipende dalla
				#soppressione dei warnings con options.
				names(temporaneo1)[ii]->nome.temporaneo
				eval(parse(text=paste0("temporaneo3$'",nome.temporaneo,"'<<-temp3")))
				eval(parse(text=paste0("temporaneo4$'",nome.temporaneo,"'<<-temp4/superficie")))


			})#fine lapply

			(nrow(temporaneo3)+1)->righe
			ncol(temporaneo3)->colonne
			
			sheet.name<-"changes.t.of.CO2.equivalent"	

			addWorksheet(wb,sheetName=sheet.name)
			applicaStiliOutcomes(foglio=wb,sheet.name=sheet.name,righe=righe,colonne=colonne)->wb
 			writeData(wb,sheet.name,temporaneo3)

			#write.xlsx(x=temporaneo3,
			#file=paste0(prefix,nome.regione,"_carbon_stock.outcomes.xlsx"),col.names=TRUE,
			#row.names=FALSE,append=TRUE,showNA=TRUE,sheetName="changes.t.of.CO2.equivalent")

			(nrow(temporaneo4)+1)->righe
			ncol(temporaneo4)->colonne
			
			sheet.name<-"changes.t.of.CO2.equival.per.ha"	
			addWorksheet(wb,sheetName=sheet.name)
			applicaStiliOutcomes(foglio=wb,sheet.name=sheet.name,righe=righe,colonne=colonne)->wb
 			writeData(wb,sheet.name,temporaneo4)
			saveWorkbook(wb,paste0(prefix,nome.regione,"_carbon_stock.outcomes.xlsx"),overwrite=TRUE)

			#write.xlsx(x=temporaneo4,
			#file=paste0(prefix,nome.regione,"_carbon_stock.outcomes.xlsx"),col.names=TRUE,
			#row.names=FALSE,append=TRUE,showNA=TRUE,sheetName="changes.t.of.CO2.equivalent.per.ha")	

			
		}#su scrivi xls


		#ora possiamo scrivere foglio outcomes.delf per la singola regione
		#la variabile total.carbon.dioxideF la rinominiamo in total.carbon.dioxide
		which(names(outcomes.del.f)=="total.carbon.dioxideF")->colonna
		stopifnot(colonna %in% c(1,2,3))
		#rinominiamo le variabili: avevavo il suffisso "F" per distinguere dalle somme analoghe  del foglio outcome
		names(outcomes.del.f)[colonna]<-c("t.of.CO2.equivalent")
		outcomes.del.f$tC<-outcomes.del.f$t.of.CO2.equivalent/CD2C
		outcomes.del.f$GgC<-outcomes.del.f$tC/1000
		melt(outcomes.del.f,c("yy","pool"))->moutcomes.del.f
		split(moutcomes.del.f,f=moutcomes.del.f$variable)->loutcomes.del.f
		#nuovo dcast yy vs pool*variable
		#dcast(moutcomes.del.f,yy~pool*variable)->doutcomes.del.f

		if(SCRIVI.XLS){
			paste0(nome.regione,"/outcomes/")->prefix
			APPENDI<-FALSE

                        wb <- createWorkbook("Guido")

			lapply(1:length(loutcomes.del.f),FUN=function(iii){
				names(loutcomes.del.f)[iii]->nome.foglio
				dcast(loutcomes.del.f[[iii]],yy~pool)->temporaneo

				#aggiungo colonna total
				temporaneo$total<-rowSums(temporaneo[,-c(1)])

				(nrow(temporaneo)+1)->righe
				ncol(temporaneo)->colonne
			
				sheet.name<-nome.foglio	
				addWorksheet(wb,sheetName=sheet.name)
				#funzione in forestExcel che applica stili a ogni foglio	
				applicaStiliOutcomes(foglio=wb,sheet.name=sheet.name,righe=righe,colonne=colonne)->wb2
 				writeData(wb2,sheet.name,temporaneo)
				
				wb2->>wb
	
				#write.xlsx(x=temporaneo,
				#file=paste0(prefix,nome.regione,"carbon_stock.outcomes_del_F.xlsx"),col.names=TRUE,
				#row.names=FALSE,append=APPENDI,showNA=TRUE,sheetName=nome.foglio)
				APPENDI<<-TRUE
			})#fine lapply

			saveWorkbook(wb,paste0(prefix,nome.regione,"carbon_stock.outcomes_del_F.xlsx"),overwrite=TRUE)


		}#su scrivi xls

}#fine funzione scriviFogliOutcomes








