# This plugin calculates the Producer Price Indices (PPI).

# Packages

library(faosws)
library(faoswsUtil)
library(data.table)

if(CheckDebug()){
  
  library(faoswsModules)
  sett <- ReadSettings("C:/Users/Taglionic/OneDrive - Food and Agriculture Organization/Github/faoswsProducerPrices/sws1.yml")
  
  SetClientFiles(sett$certdir)
  GetTestEnvironment(sett$server, sett$token)
  # files = dir("R", full.names = TRUE)
  #  invisible(sapply(files, source))
}

# Parameters

min_year <- as.numeric(swsContext.computationParams$min_year)
max_year <- as.numeric(swsContext.computationParams$max_year)
base_year <- as.numeric(swsContext.computationParams$base_year)

# Conditions
if(min_year >= base_year){
  stop('The Minimum year must be less than the Base year.')
}

if(max_year <= base_year){
  stop('The Maximum year must be greater than the Base year.')
}

# Base period
base_period <- (base_year - 1):(base_year + 1)

if(median(base_period) == 2005) {
  element_code <- '5539'} else 
    if(median(base_period) == 2010) {
      element_code <- '55391'} else
        if(median(base_period) == 2015) {
          element_code <- '55392'
        }

## Get data

# Production data
prodKey = DatasetKey(
  domain = 'agriculture',
  dataset = 'aproduction',
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList('agriculture', 'aproduction', 'geographicAreaM49')[type == 'country', code]),
    Dimension(name = "measuredElement", 
              keys = '5510'),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList('agriculture', 'aproduction', 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = as.character(min_year:max_year))
  )
)

productionData <- GetData(prodKey, flags = FALSE)
productionData[, measuredElement := NULL]
setnames(productionData, 'Value', 'production')

fs1 <- read.csv('C:/Users/Taglionic/Desktop/FAOSTAT_data_8-18-2021.csv')
fs2 <- read.csv('C:/Users/Taglionic/Desktop/FAOSTAT_data_8-18-2021 (1).csv')
fs3 <- read.csv('C:/Users/Taglionic/Desktop/FAOSTAT_data_8-18-2021 (2).csv')
fs4 <- read.csv('C:/Users/Taglionic/Desktop/FAOSTAT_data_8-18-2021 (3).csv')

fs <- rbind(fs1, fs2,fs3,fs4)
fs <- as.data.table(fs)

names(fs)
fs[,c("?..Domain.Code", "Domain", "Element", "Year.Code", "Unit", "Element.Code")] <- NULL
names(productionData)

setnames(fs, c("Area.Code..M49.", "Item.Code..CPC.", "Year"),
         c( "geographicAreaM49", "measuredItemCPC",   "timePointYears" ))

fs$geographicAreaM49 <- as.character(fs$geographicAreaM49)
fs$timePointYears <- as.character(fs$timePointYears)

fs[geographicAreaM49 == '156', geographicAreaM49 := '1248']
fs <- fs[timePointYears > 1990]



library(openxlsx)
amandaData0 <- read.xlsx('C:/Users/Taglionic/Desktop/producer_price_indices_(ppi)_v10/Producer Price Indices (PPI)_FAODOMAIN_caetano_10/Book1.xlsx')
amandaData <- as.data.table(amandaData0)
amandaData$AreaCode <- as.character(amandaData$AreaCode)
amandaData[,  geographicAreaM49 := fs2m49(AreaCode)]
unique(amandaData[is.na(geographicAreaM49)]$AreaCode)
amandaData <- amandaData[!is.na(geographicAreaM49)]
amandaData$ItemCode <- as.character(amandaData$ItemCode)
amandaData[ , ItemCode := ifelse(nchar(ItemCode)==3, paste('0', ItemCode, sep = ''),
                                 ifelse(nchar(ItemCode)==2, paste('00', ItemCode, sep = ''),ItemCode))]
amandaData[ , measuredItemCPC := fcl2cpc(ItemCode)]

amandaData <- amandaData[!is.na(measuredItemCPC)]
amandaData[ , c('AreaCode', 'ItemCode',  'EleCode')] <- NULL
setnames(amandaData, 'Value', 'production')
setnames(amandaData, 'Year', 'timePointYears')

amandaData$timePointYears <- as.character(amandaData$timePointYears)
# compfaostatAmanda <- merge(fs, amandaData, 
#                            by = c("geographicAreaM49", "measuredItemCPC",   "timePointYears" ),
#                            all = T)


compfaostatAmanda[Value.x == Value.y, check := 'ok']
compfaostatAmanda[Value.x != Value.y, check := 'discrepancy']
compfaostatAmanda[is.na(Value.x) & !is.na(Value.y),check := 'only Amanda']
compfaostatAmanda[!is.na(Value.x) & is.na(Value.y),check := 'only FAOSTAT']
compfaostatAmanda[Value.x == 0 & is.na(Value.y), check := 'only FAOSTAT but 0']
compfaostatAmanda[is.na(Value.x) & Value.y == 0,check := 'only Amanda but 0']
compfaostatAmanda[is.na(Value.x) & is.na(Value.y),check := 'both NAs']    

compfaostatAmanda[,.N, check]
# compAP <- merge(productionData[, production := round(production)], fs[geographicAreaM49 != '351' & timePointYears > 1990], by = c( "geographicAreaM49", "measuredItemCPC",   "timePointYears" ),
#       all = T)
# 
# names(compAP)
# compAP[,.N]
# compAP[production == Value, check := 'ok']
# compAP[production != Value, check := 'discrepancy']
# compAP[is.na(production) & !is.na(Value),check := 'only FAOSTAT']
# compAP[!is.na(production) & is.na(Value),check := 'only SWS']
# compAP[production == 0 & is.na(Value), check := 'only SWS but 0']
# compAP[is.na(production) & Value == 0,check := 'only FAOSTAT but 0']
# compAP[is.na(production) & is.na(Value),check := 'both NAs']
# compAP[,.N,check]
# 
# View(compAP[check == 'only FAOSTAT',.N, Item])

setnames(fs, 'Value', 'production')

productionData <- amandaData # fs[, c( "geographicAreaM49", "measuredItemCPC",   "timePointYears", 'production'), with = F] #

# Price data
if(CheckDebug()){
  
  library(faoswsModules)
  sett <- ReadSettings("C:/Users/Taglionic/OneDrive - Food and Agriculture Organization/Github/faoswsProducerPrices/sws.yml")
  
  SetClientFiles(sett$certdir)
  GetTestEnvironment(sett$server, sett$token)
  # files = dir("R", full.names = TRUE)
  #  invisible(sapply(files, source))
}

domainPP <- 'prod_prices'
datasetPP <- 'annual_producer_prices_legacy_arimax'

priceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPP,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = GetCodeList(domainPP, datasetPP, 'geographicAreaM49')[type == 'country', code]),
    Dimension(name = "measuredElement", 
              keys = '5531'),
    Dimension(name = "measuredItemCPC",
              keys = GetCodeList(domainPP, datasetPP, 'measuredItemCPC')[, code]),
    Dimension(name = "timePointYears", 
              keys = as.character(min_year:max_year))
    
  )
)

priceData = GetData(priceKey, flags = FALSE)
setnames(priceData, 'Value', 'price')


xxx <- read.xlsx('C:/Users/Taglionic/Desktop/producer_price_indices_(ppi)_v10/Producer Price Indices (PPI)_FAODOMAIN_caetano_10/Book1_prices.xlsx' ,sheet = 2)
xxx <- as.data.table(xxx)
names(xxx)
xxx[, geographicAreaM49 := fs2m49(as.character(AreaCode))]
xxx <- xxx[!is.na(geographicAreaM49)]
xxx[ , ItemCode := ifelse(nchar(ItemCode)==3, paste('0', ItemCode, sep = ''),
                          ifelse(nchar(ItemCode)==2, paste('00', ItemCode, sep = ''),ItemCode))]
xxx[ , measuredItemCPC := fcl2cpc(ItemCode)]
unique(xxx[is.na(measuredItemCPC)]$ItemCode)

xxx[ItemCode == '0484', measuredItemCPC := '01312.01']
xxx[ItemCode == '0485', measuredItemCPC := '01312.02']
xxx[ItemCode == '0488', measuredItemCPC := '01313.02']
xxx[ItemCode == '0487', measuredItemCPC := '01313.01']

xxx <- xxx[!is.na(measuredItemCPC)]
xxx[,ItemCode := NULL]
xxx[,AreaCode := NULL]
priceData <- copy(xxx)
priceData[,EleCode := as.character(EleCode)]
priceData[,Value := as.numeric(Value)]

setnames(priceData, c( 'EleCode', 'Year'), c('measuredElement', 'timePointYears'))
setnames(priceData, 'Value', 'price')

priceData <- priceData[!measuredItemCPC %in% c('01219.01',
                                               '01492',
                                               '01499.06',
                                               '01911',
                                               '01912',
                                               '01919.01',
                                               '01919.02',
                                               '01919.03',
                                               '01929.03',
                                               '2167',
                                               '21411',
                                               '23511.01',
                                               '23512',
                                               '24212.02',
                                               '2351f')]

productionData$timePointYears <- as.character(productionData$timePointYears)
priceData$timePointYears <- as.character(priceData$timePointYears)
# Data merge
prod_price_data <- merge(productionData, 
                         priceData[, mget(c('geographicAreaM49', 'measuredItemCPC', 'timePointYears', 'price'))],
                         by = c('geographicAreaM49', 'measuredItemCPC', 'timePointYears'),
                         all = T)

# Exclude NA's
#prod_price_data <- prod_price_data[!(is.na(production) | (is.na(price)))]

## Item groups

if(CheckDebug()){
  
  library(faoswsModules)
  sett <- ReadSettings("C:/Users/Taglionic/OneDrive - Food and Agriculture Organization/Github/faoswsProducerPrices/sws1.yml")
  
  SetClientFiles(sett$certdir)
  GetTestEnvironment(sett$server, sett$token)
  # files = dir("R", full.names = TRUE)
  #  invisible(sapply(files, source))
}

aggregate_item_groups <- ReadDatatable('aggregate_groups_faostat_sws',
                                       columns = c('var_group_code_sws', 'var_code_sws', 'factor'),
                                       where = "var_type = 'item' AND domain_code = 'PI'")

#ReadDatatable('aggregate_groups',columns = c('var_group_code_sws', 'var_code_sws', 'factor'),
#where = "var_type = 'item' AND domain_code = 'PI'")




setnames(aggregate_item_groups, old=c('var_group_code_sws', 'var_code_sws'),
         new = c('item_group_code', 'measuredItemCPC'))

# Set of items that must be used on the aggregates
aggregate_type_item = ReadDatatable('aggregate_type',
                                    where = paste0("var_type = 'item' AND domain_code = 'PI' "))

# aggregate_type_item[, .N, c('domain_var_diss_flag_agg_int', 'domain_var_diss_flag_agg_ext')]
list_item <- aggregate_type_item[!(domain_var_diss_flag_agg_int == 0 | is.na(var_code_sws))]

aggregate_item_groups <- aggregate_item_groups[measuredItemCPC %in% unique(list_item$var_code_sws)] 

prod_price_data <- merge(prod_price_data, aggregate_item_groups, by = 'measuredItemCPC',
                         all.x = T, allow.cartesian = T)

prod_price_data[, productionFactor := production * as.numeric(factor)]

prod_price_data[, 'production' := NULL]

setnames(prod_price_data, old = c('productionFactor'),
         new = c('production'))

# Calc averages

average_tab <- prod_price_data[timePointYears %in% base_period, 
                               list(average_price = mean(price, na.rm = T), 
                                    average_production = mean(production, na.rm = T)),
                               by = c('geographicAreaM49', 'measuredItemCPC')]

# Merge average_tab with prod_price_data

prod_price_data <- merge(prod_price_data, average_tab, 
                         by = c('geographicAreaM49', 'measuredItemCPC'), 
                         all.x = T)

# PPI for items
prod_price_data[, ppi := price/average_price * 100]

# Only items

ppi_item_tab <- prod_price_data[, mget(c('geographicAreaM49', 'measuredItemCPC',
                                         'timePointYears', 'ppi'))]

ppi_item_tab[, measuredElement := element_code]
ppi_item_tab[, `:=` (flagObservationStatus = 'I',
                     flagMethod = 'i')]

setnames(ppi_item_tab, old = 'ppi', new = 'Value')

setcolorder(ppi_item_tab, 
            c('geographicAreaM49',  'measuredElement', 'measuredItemCPC', 'timePointYears',
              'Value', 'flagObservationStatus', 'flagMethod'))

setkey(ppi_item_tab)
ppi_item_tab <- unique(ppi_item_tab)

## Calc PPI for item groups
ppi_item_groups_tab <- prod_price_data[, list(ppi_item_groups = 
                                                100 * sum(price * average_production, na.rm = T)/sum(average_price * average_production, na.rm = T)),
                                       by = c('geographicAreaM49', 'timePointYears', 'item_group_code')]

prod_price_data <- prod_price_data[, ppi_item_groups := 
                                     (100 * sum(price * average_production, 
                                                na.rm = T)/sum(average_price * average_production, 
                                                               na.rm = T)),
                                   by = c('geographicAreaM49', 'timePointYears', 'item_group_code')]

prod_price_data[ , num := price*average_production ]
prod_price_data[, den := average_price*average_production]
setkey(prod_price_data)
prod_price_data <- unique(prod_price_data[,.(geographicAreaM49, timePointYears,
                                             item_group_code, num, den)])

prod_price_data[ , num1 := sum(num, na.rm = T),
                 by = c('geographicAreaM49', 'timePointYears', 'item_group_code')]


prod_price_data[ , den1 := sum(den, na.rm = T),
                 by = c('geographicAreaM49', 'timePointYears', 'item_group_code')]


prod_price_data[, ratio := num/den]
prod_price_data[, ratio1 := num1/den1]


View(prod_price_data[, list(ppi_item_groups_comp = 
                              mean(ratio, na.rm = T)),
                     by = c('geographicAreaM49', 'timePointYears', 'item_group_code')])


prod_price_data <- prod_price_data[ ,.(geographicAreaM49, timePointYears,
                                       item_group_code, num, den, ppi_item_groups)]

setkey(prod_price_data)
prod_price_data <- unique(prod_price_data)

prod_price_data[, num1 := sum(num, na.rm = T), 
                by = c('geographicAreaM49', 'timePointYears', 'item_group_code')]
prod_price_data[, den1 := sum(den, na.rm = T), 
                by = c('geographicAreaM49', 'timePointYears', 'item_group_code')]

prod_price_data[, ppi_item_groups2 := sum(ppi_item_groups1), 
                by = c('geographicAreaM49', 'timePointYears', 'item_group_code')]

prod_price_data[, ppi_item_groups_comp := 100*(num1/den1)]
prod_price_data[ppi_item_groups_comp != ppi_item_groups]
##############
ppi_item_groups_tab[, measuredElement := element_code]
ppi_item_groups_tab[, `:=` (flagObservationStatus = 'I',
                            flagMethod = 'i')]

setnames(ppi_item_groups_tab, old = c('item_group_code', 'ppi_item_groups'),
         new = c('measuredItemCPC', 'Value'))

setcolorder(ppi_item_groups_tab, 
            c('geographicAreaM49',  'measuredElement', 'measuredItemCPC', 'timePointYears',
              'Value', 'flagObservationStatus', 'flagMethod'))


# Bind the two results and excluding NA's

final_ppi <- rbind(ppi_item_tab, ppi_item_groups_tab)
final_ppi <- final_ppi[!(is.na(Value)| is.nan(Value))]
final_ppi <- final_ppi[ Value < Inf]

write.csv(final_ppi,'PPIs_non-published-products-excluded_310821.csv', row.names = F)
# save data

stats = SaveData(domain = domainPP, 
                 dataset = datasetPP, 
                 data = final_ppi, 
                 waitTimeout = 10000)

paste0("PPI completed successfully!!! ",
       stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
