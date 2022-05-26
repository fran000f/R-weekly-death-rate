require(DBI)
require(RSQLite)
dataDir    <- file.path(getwd(),"data")
dbName <- file.path(dataDir,"data.db")

con <- dbConnect(
  dbDriver("SQLite"),
  dbname = dbName
)

try({
  d <- read.table(file=file.path(dataDir,"eurostat.csv"),sep=";",dec=",",header=T)
  dbWriteTable(con, "zgony_wg_tygodni_eurostat", d, overwrite = TRUE, row.names = FALSE)
})

dbDisconnect(con)