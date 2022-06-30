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

message("PPI calculations: Getting parameters")

min_year <- as.numeric(swsContext.computationParams$min_year)
max_year <- as.numeric(swsContext.computationParams$max_year)
base_year <- as.numeric(swsContext.computationParams$base_year)
country <- as.character(swsContext.computationParams$country)

# PP domain and datasets
domainPP <- 'prod_prices'
datasetPP <- swsContext.datasets[[1]]@dataset  #'annual_producer_prices_validation'

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

# Countries

if(!is.null(country) & length(country) > 0){
  country <- swsContext.computationParams$country
  sessionCountry <- strsplit(country, ', ')[[1]]
} else {
  sessionCountry <- GetCodeList(domainPP, datasetPP, "geographicAreaM49")[ type == 'country']$code
}

M49 <- GetCodeList('agriculture', 'aproduction', 'geographicAreaM49')[type == 'country',]

## Get data

message("PPI calculations: Getting data")

# Production data
prodKey = DatasetKey(
  domain = 'agriculture',
  dataset = 'aproduction',
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = M49[code %in% sessionCountry]$code),
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

if(productionData[geographicAreaM49 == '156',.N] == 0){
  productionData[geographicAreaM49 == '1248', geographicAreaM49 := '156']
  
}


priceKey = DatasetKey(
  domain = domainPP,
  dataset = datasetPP,
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = M49[code %in% sessionCountry]$code),
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
 
message("PPI calculations: Start calculations")
# Data merge
prod_price_data <- merge(productionData, 
                         priceData[, mget(c('geographicAreaM49', 'measuredItemCPC', 'timePointYears', 'price'))],
                         by = c('geographicAreaM49', 'measuredItemCPC', 'timePointYears'),
                         all = T)

# Exclude NA's
#prod_price_data <- prod_price_data[!(is.na(production) | (is.na(price)))]

## Item groups

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
message("PPI calculations: Single item PPI calculation")
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
message("PPI calculations: Group PPI calculations")
ppi_item_groups_tab <- prod_price_data[, list(ppi_item_groups = 
                                               100 * sum(price * average_production, na.rm = T)/sum(average_price * average_production, na.rm = T)),
                                   by = c('geographicAreaM49', 'timePointYears', 'item_group_code')]

ppi_item_groups_tab[, measuredElement := element_code]
ppi_item_groups_tab[, `:=` (flagObservationStatus = 'I',
                             flagMethod = 'i')]

setnames(ppi_item_groups_tab, old = c('item_group_code', 'ppi_item_groups'),
         new = c('measuredItemCPC', 'Value'))

setcolorder(ppi_item_groups_tab, 
            c('geographicAreaM49',  'measuredElement', 'measuredItemCPC', 'timePointYears',
              'Value', 'flagObservationStatus', 'flagMethod'))


# Bind the two results and excluding NA's
message("PPI calculations: Final data to save")
final_ppi <- rbind(ppi_item_tab, ppi_item_groups_tab)
final_ppi <- final_ppi[!(is.na(Value)| is.nan(Value))]
final_ppi <- final_ppi[ Value < Inf]

# save data
message("PPI calculations: Data saving")
stats = SaveData(domain = domainPP, 
                 dataset = datasetPP, 
                 data = final_ppi, 
                 waitTimeout = 10000)

paste0("PPI completed successfully!!! ",
       stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
