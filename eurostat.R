require(data.table)
require(R.utils)
require(tidyverse)
dataDir    <- file.path(getwd(),"data")
setwd(dataDir)
EURDataUrl <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/demo_r_mwk_ts.tsv.gz"
res <- try({
  download.file(
    url=EURDataUrl,
    destfile=file.path("demo_r_mwk_ts.tsv.gz"),
    method="curl"
  )
}
)

if(inherits(res,"try-error")){
  print("Proces zakonczyl sie niepowidzeniem - wprowadz url")
}else{
  print("Proces przebiegl pomyslnie") 
}

gunzip("demo_r_mwk_ts.tsv.gz", remove=FALSE)
dataEur <- read.table("demo_r_mwk_ts.tsv",header=F,sep = '\t')
legend <- dataEur[,1]
legend_edit <- strsplit(legend,split=",")
legend <- as.data.frame(legend_edit)
legend <- as.data.frame(t(legend))
legend[1,3] = "country"
legend <- subset(legend, select = -c(2))
len <- as.numeric(length(dataEur))

for (j in seq(2,115)) {
  for (i in seq(2,len)){
  dataEur[j,i] <- gsub("\\D","",dataEur[j,i])
}
}
dataEurNew <- subset(dataEur, select = -c(1))
dataEurEdit <- cbind(legend,dataEurNew)
countries <- c('countries', 'Albania', 'Andorra', 'Armenia', 'Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czechia', 'Denmark', 'Estonia', 'Finland', 'France', 'Georgia', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Malta', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Romania', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'United Kingdom', 'Albania', 'Andorra', 'Armenia', 'Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czechia', 'Denmark', 'Estonia', 'Finland', 'France', 'Georgia', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Malta', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Romania', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'United Kingdom', 'Albania', 'Andorra', 'Armenia', 'Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czechia', 'Denmark', 'Estonia', 'Finland', 'France', 'Georgia', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Latvia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Malta', 'Montenegro', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Romania', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'Switzerland', 'United Kingdom')
dataEurEdit[,2] <- countries
colnames(dataEurEdit) <- as.character(dataEurEdit[1, ])
dataEurEdit <- dataEurEdit[-1,]
dataEurostatReady <- gather(dataEurEdit,"Week","value",!(c(1,2)))
for (i in seq(1,nrow(dataEurostatReady)))
{
  string <- str_split(dataEurostatReady$Week[i],"W",simplify=T)
  str_sub(string[2],3,4) <- "-4"
  weekdate <- string[2]
  dataEurostatReady$Week[i] <- paste(string[1],paste0("W",string[2]),sep="-")
}
write.table(dataEurostatReady,file="./eurostat.csv",sep=";",dec=",",col.names=T)
