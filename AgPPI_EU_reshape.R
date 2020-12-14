# -- Load Packages ----

suppressMessages({
  library(data.table)
  library(DT)
  library(forecast)
  library(tseries)
  library(faosws)
  library(faoswsFlag)
  library(faoswsProcessing)
  library(faoswsUtil)
  library(faoswsImputation)
  library(imputeTS)
  library(ggplot2)
  library(sendmailR)
  library(openxlsx)
})

# -- Token QA ----

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '4e9d9a2e-5258-48b6-974f-3656d1af8217')
}


tab <- read.xlsx('AgPPI_EU.xlsx', startRow = 5)
tab <- as.data.table(tab)
names(tab)
setnames(tab, c("Source.of.data", "Eurostat"),
         c('geo', 'X2'))
matadata1 <- unique(tab[geo == 'P_ADJ']$X2) # Price adjusted

if(nrow(unique(tab[geo == 'P_ADJ']))){
  tab <- tab[geo != 'P_ADJ']
}

metadata2 <- unique(tab[geo == 'UNIT']$X2)

if(nrow(unique(tab[geo == 'UNIT']))){
  tab <- tab[geo != 'UNIT']
}

tab[geo == 'PRODUCT', c('X3', 'X4', 'X5') := list(X2, X2, X2)]

idx <- which(tab$geo == 'PRODUCT')

agPPIlist <- list()

for(i in 2:(length(idx)-1)){
  lastrow <- (idx[(i+1)]-1)
  tab2insert <- tab[idx[i]:lastrow]
  product <- tab2insert[geo == 'PRODUCT']$X2
  tab2insert <- tab2insert[!geo %in% c('PRODUCT', 'GEO/TIME') ]
  agPPIlist[[product]] <- tab2insert
}

agPPIs <- rbindlist(agPPIlist, use.names=TRUE, idcol = "ID")
setnames(agPPIs, c("X2",  "X3",  "X4",  "X5"),
         c('2015', '2016', '2017', '2018'))

agPPIsNorm <- melt(agPPIs,
                   id.vars = 1:2,
                   variable.name = 'timePointYears')

agPPIsNorm[, value := as.numeric(value)]
agPPIsNorm <- agPPIsNorm[!is.na(value)]

products <- unique(agPPIsNorm$ID)

crops <- GetCodeList('agriculture', 'eurostat_apro_cpsh1', 'eurostatCrops')

derived <- GetCodeList('agriculture', 'eurostat_derived', 'eurostatDerived')

farmliv <- GetCodeList('agriculture', 'eurostat_farm_livestock', 'eurostatFarmLivestock')

farmmilk <- GetCodeList('agriculture', 'eurostat_farm_milk', 'eurostatFarmMilk')

livestock <- GetCodeList('agriculture', 'eurostat_livestock', 'eurostatLivestock')

eurostat_meat_in_slaughterhouses <- GetCodeList('agriculture', 'eurostat_meat_in_slaughterhouses', 'eurostatMeatInSlaughterhouses')

eurostat_meat_other_than_slaughterhouses <- GetCodeList('agriculture', 'eurostat_meat_other_than_slaughterhouses', 'eurostatMeatOtherThanSlaughterhouses')

eurostat_poultry_farming <- GetCodeList('agriculture', 'eurostat_poultry_farming', 'eurostatPoultryFarming')

okay <- products[products %in% crops$description]
okay <- c(okay, products[products %in% derived$description])
okay <- c(okay, products[products %in% farmliv$description])
okay <- c(okay, products[products %in% farmmilk$description])
okay <- c(okay, products[products %in% livestock$description])
okay <- c(okay, products[products %in% eurostat_meat_in_slaughterhouses$description])
okay <- c(okay, products[products %in% eurostat_meat_other_than_slaughterhouses$description])
okay <- c(okay, products[products %in% eurostat_poultry_farming$description])
