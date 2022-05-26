require(readxl)
require(stringi)
dataDir    <- file.path(getwd(),"data")
GUSDataUrl="https://stat.gov.pl/download/gfx/portalinformacyjny/pl/defaultaktualnosci/5468/39/2/1/zgony_wg_tygodni.zip"

res <- try({
  download.file(
    url=GUSDataUrl,
    destfile=file.path(dataDir,"zgony_wg_tygodni.zip"),
    method="curl"
  )
  
  unzip(file.path(dataDir,"zgony_wg_tygodni.zip"),exdir=file.path(dataDir),setTimes=T)
  
  setwd(dataDir)
  
  setwd("./zgony_wg_tygodni")
  
  
  
})

if(inherits(res,"try-error")){
  print("Proces zakonczyl sie niepowidzeniem - wprowadz url")
}else{
  print("Proces przebiegl pomyslnie") 
  
}

fileNames <- list.files()
file.rename(fileNames, paste0("Zgony wedlug tygodni w Polsce_", stri_sub(fileNames,-9)))
czytajDaneLiczboweZZakladki <- function(f,sheet,plec){
  d <- as.data.frame(read_xlsx(f,sheet=sheet))
  colnames(d)[1:3] <- c("Grupa_wiekowa","Region_id","Region")
  d <- d[-c(1:(grep("^Og",d$Grupa_wiekowa)[1]-1)),]
  
  tygodnie <- 1:(ncol(d)-3)
  tygodnie[nchar(tygodnie)<2] <- paste0("0",tygodnie[nchar(tygodnie)<2])
  colnames(d)[4:ncol(d)] <- tygodnie
  
  d <- reshape::melt(d,id.vars=c("Grupa_wiekowa","Region_id","Region"))
  colnames(d) <- c("Grupa_wiekowa","Region_id","Region","Tydzien","Liczba")
  d$Grupa_wiekowa[grep("Og",d$Grupa_wiekowa)] <- "0 - Inf"
  d$Grupa_wiekowa[grep("wi",d$Grupa_wiekowa)] <- "90 - Inf"
  #d$Liczba[is.na(d$Liczba)] <- 0 
  d <- cbind("Plec"=plec,d)
  
  return(d)
  
}

try({
  mainRet <- do.call("rbind",lapply(dir(),function(f){
   
    ogolem <- czytajDaneLiczboweZZakladki(f,1,"Ogolem")
    mezczyzni <- czytajDaneLiczboweZZakladki(f,2,"Mezczyzni")
    kobiety <- czytajDaneLiczboweZZakladki(f,3,"Kobiety")
    
    calosc <- rbind(ogolem,mezczyzni,kobiety)
    calosc$Tydzien <- as.character(calosc$Tydzien)
    
    rok = 1999
    day = "4"
    for (i in seq(1,nrow(calosc),1)){
        if (isTRUE(calosc$Tydzien[i] == "01") && isTRUE(calosc$Region_id[i] == "PL") && isTRUE(calosc$Plec[i] == "Ogolem") && isTRUE(calosc$Grupa_wiekowa[i] =="0 - Inf")) { 
          rok = rok+1
          calosc$Tydzien[i] <- paste(rok, paste0("W", calosc$Tydzien[i]), day, sep="-")
        } else {
          calosc$Tydzien[i] <- paste(rok, paste0("W", calosc$Tydzien[i]), day, sep="-")
        }
    }
    return(calosc)
  }))
  
  write.table(mainRet,file="../GUS_dane_przetworzone_pelne.csv",sep=";",dec=",",row.names=F)
})



