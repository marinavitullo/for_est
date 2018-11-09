scriviFoglioSurface<-function(nome.regione,dsurface){
  
  #foglio surface_F_regioni. I dati di questo foglio vengono utilizzati per calcolare
  #nei file regionali Sintesi per C per Pool le quantità da sottrarre ai totali colonna
  #nel tab dead organic matter last per litter e deadwood
  
  dsurface %>% dplyr::select(-c(1,2,3)) %>% colSums -> totaliColonna
  
  #estrai deforestazione
  tryCatch({
    deforestationp %>% dplyr::select_(nome.regione) %>% extract2(1)->valoriDeforestazione 
  },error=function(e){
    stop(sprintf("Regione %s - non trovati dati di deforestazione",nome.regione))
  })
  
  sommaPerVar3(dsurface,deforestation=valoriDeforestazione)->regione.sum
  
  #totale regione.sum escludendo plantations
  regione.sum %>% filter(var3!="plantations") %>% dplyr::select(-c(1)) %>% colSums()->totale.regione.sum
  #la prima posizione con un NA
  c(NA,diff(totale.regione.sum))->differenza.totale.regione.sum

  
  #estrazione dati deadmass
  estraiDati(x=dati.dead,nome.variabile="total.dry.matter")->ddrymatter
  ddrymatter %>% dplyr::select(-c(1,2,3)) %>% colSums(.,na.rm=TRUE)->total.ddrymatter
  
  #foglio surface F regioni: colonna deadmass
  (total.ddrymatter/totaliColonna)*CC2DM->deadmass.tCha
  
  #estrazione dati litter
  #colonna Litter
  estraiDati(x=dati.litter,nome.variabile="total.carbon")->dtotal.carbon
  dtotal.carbon %>% dplyr::select(-c(1,2,3)) %>% colSums(.,na.rm=TRUE)->total.dtotal.carbon
  (total.dtotal.carbon/totaliColonna)->litter.tCha
  
  (deadmass.tCha*differenza.totale.regione.sum)->deadmass.tC
  #moltiplico litter.tCa di un anno con differenza.totale.regione.sum dell'anno successivo
  (c(0,litter.tCha[1:(length(litter.tCha)-1) ])*differenza.totale.regione.sum)->litter.tC  
  
  #data.frame di output
  data.frame(year=annoI:annoF,surface=totaliColonna,surface.only.forest=totale.regione.sum,
             fl.fl=totale.regione.sum,l.to.fl=differenza.totale.regione.sum)->df1
  
  data.frame(year=annoI:annoF,deadmass.tCha=deadmass.tCha,litter=litter.tCha,
             deadmass.tC=deadmass.tC,litter.tC=litter.tC)->df2
  
  return(list(df1,df2))
  
}#forest Surface