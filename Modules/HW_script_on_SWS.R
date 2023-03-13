# .libPaths("/newhome/shared/Library/3.3.3")
library(stats)
library(data.table)
library(faoswsUtil)
library(faosws)
library(faoswsFlag)
library(zoo)
library(tidyr)
library(faoswsProcessing)

getKey <- function(domain_, dataset_) {
  
  keyAreaItem <- DatasetKey(
    domain = domain_,
    dataset = dataset_,
    dimensions = list(
      Dimension(name = country,
                keys = GetCodeList(domain_, dataset_, country)[type == 'country', code]
                # keys = selected_country
                ),
      Dimension(name = element,
                keys = GetCodeList(domain_, dataset_, element)[, code]),
      Dimension(name = item,
                keys = GetCodeList(domain_, dataset_, item)[, code]),
      Dimension(name = years,
                keys = as.character(as.numeric(first_year:last_year)) #Can be set by researcher later
      )
    ))
}
message(paste0(packageVersion('forecast')))
message(paste0('Holt winters Indicators plugin has started.'))
#### Setting env ####
if(CheckDebug()){
  
  library(faoswsModules)
  SETTINGS <- ReadSettings("modules/HoltWinters//sws.yml") #~/Documents/InternshipFAO/HoltWinters/modules/sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}



first_year <- swsContext.computationParams$min_year # stores params specifiied by user
last_year <- swsContext.computationParams$max_year # 2021 

## Want to run the plugin on a specific dataset
dataset <- swsContext.datasets[[1]]@dataset # 'annual_producer_prices_prep' #
domain <- swsContext.datasets[[1]]@domain # 'prod_prices'#

country <- grep('geo', GetDatasetConfig(domain, dataset)$dimensions,
                value = T, ignore.case = T)
element <- grep('elem', GetDatasetConfig(domain, dataset)$dimensions,
                value = T, ignore.case = T)
item <- grep('item', GetDatasetConfig(domain, dataset)$dimensions,
             value = T, ignore.case = T)
years <- grep('years', GetDatasetConfig(domain, dataset)$dimensions,
              value = T, ignore.case = T)

# Get only countries of interest
countryPar <- swsContext.computationParams$countries

keys <- getKey(domain_ = domain, dataset_ = dataset)

pp <- GetData(keys, flags = TRUE)


if(!is.null(countryPar) & length(countryPar) > 0){
  countryPar <- swsContext.computationParams$countries
  sessionCountry <- strsplit(countryPar, ', ')[[1]]
  
} else {
  countries <- GetCodeList(domain, dataset, "geographicAreaM49")[ type == 'country']$code
  sessionCountry <- countries #sessionCountry[sessionCountry %in% countries]
}

pp <- pp[geographicAreaM49 %in% sessionCountry]

message(paste0('Printing pp after loading data'))

message(paste0('Data loaded and now holt winters will be applied'))
# pp$integrated <- pp$Value

p <- split(pp, pp$measuredElement) # We split by currency
p <- split(p[['5531']], p[['5531']]$geographicAreaM49) # We split the values by country and select SLC
pc <- lapply(p, function(x){split(x, x$measuredItemCPC)}) # We split the country values by commodity

message(paste0('Printing pp after splitting data'))

year <- as.numeric(format(Sys.Date(), "%Y")) # Determine the year of today

endYear <- as.numeric(head(pp$timePointYears,1))
minYear <- as.numeric(endYear) - 3 # 2018

### length(pc) added, check what is the column name of pprice,"Value" or pprice?
p <- lapply(pc[1:length(pc)], function(x){ # and now apply the holt winters function to the nested dataframes
  
  lapply(x, function(y){
    # y$Value <- (y$Value-mean(y$Value, na.rm=T))/sd(y$Value,na.rm=T)
    end <- as.numeric(head(y$timePointYears,1))
    if (end < minYear){
      return(y)
    } else if (length(y$Value) < 5){
      return(y)
    } else{
      tryCatch({
        hw <- HoltWinters(ts(rev(y$Value[1:5])), gamma=F)
        #h <- length((year-1):(end+1))
        h <- length((year):(end+1)) # michele asked to change it
        nn <- head(y,h)
        #nn$timePointYears <- (year-1):(end+1)
        nn$timePointYears <- (year):(end+1) # michele asked to change it
        nn$Value <- rev(predict(hw,n.ahead=h))
        nn$flagObservationStatus <- rep('F', h)
        nn$flagMethod <- rep('e', h)
        y <- rbind(nn,y)
        },
        error = function(e) {y})
      }
    })
})

#lapply(p[1:length(pc)], function(x){ lapply(x, function(y) min(y$Value))})

# Remove all negative forecast
p2 <- lapply(p[1:length(p)], function(x){
  lapply(x, function(y){
    y <- as.data.table(y)
    y <- y[Value > 0]
  })
})

# Repeat forecast with 10 years
p3 <- lapply(p2[1:length(p2)], function(x){ # and now apply the holt winters function to the nested dataframes
  
  lapply(x, function(y){
    # y$Value <- (y$Value-mean(y$Value, na.rm=T))/sd(y$Value,na.rm=T)
    end <- as.numeric(head(y$timePointYears,1))
    
    #ifelse(as.numeric(head(y$timePointYears,1)) >= year-1 , year - 1, as.numeric(head(y$timePointYears,1)))
    ifelse(as.numeric(head(y$timePointYears,1)) >= year, year, as.numeric(head(y$timePointYears,1))) # michele asked to change it
    if (end < minYear){
      return(y)
    } else if (length(y$Value) < 10){
      return(y)
    # } else if(end >= (year-1)){ 
    } else if(end >= (year)){ # michele asked to change it
      return(y)
    }else{
      tryCatch({
        hw <- HoltWinters(ts(rev(y$Value[1:10])), gamma=F)
        #h <- length((year-1):(end+1))
        h <- length((year):(end+1)) # michele asked to change it
        nn <- head(y,h)
        #nn$timePointYears <- (year-1):(end+1)
        nn$timePointYears <- (year):(end+1) # michele asked to change it
        nn$Value <- rev(predict(hw,n.ahead=h))
        nn$flagObservationStatus <- rep('F', h)
        nn$flagMethod <- rep('e', h)
        y <- rbind(nn,y)
      },
      error = function(e) {y})
    }
  })
})

# Remove again all negative forecast
p4 <- lapply(p3[1:length(p3)], function(x){
  lapply(x, function(y){
    y <- as.data.table(y)
    y <- y[Value > 0]
  })
})

# Carry-forward

p5 <- lapply(p4[1:length(p4)], function(x){ # and now apply the holt winters function to the nested dataframes
  
  lapply(x, function(y){
    # y$Value <- (y$Value-mean(y$Value, na.rm=T))/sd(y$Value,na.rm=T)
    # end <- ifelse(as.numeric(head(y$timePointYears,1)) >= year-1 , year - 1, as.numeric(head(y$timePointYears,1))) 
    end <- ifelse(as.numeric(head(y$timePointYears,1)) >= year , year, as.numeric(head(y$timePointYears,1))) # michele asked to change it
    if (end < minYear){
      return(y)
    } else if (length(y$Value) < 10){
      return(y)
      
    # } else if(end >= (year-1)){ 
    } else if(end >= (year)){ # michele asked to change
      return(y)
    } else{
      tryCatch({
      #  hw <- HoltWinters(ts(rev(y$Value[1:10])), gamma=F)
        # h <- length((year-1):(end+1)) 
        h <- length((year):(end+1)) # michele asked to change it
        nn <- head(y,h)
        # nn$timePointYears <- (year-1):(end+1)
        nn$timePointYears <- (year):(end+1) # michele asked to change it
        nn$Value <- rep(y$Value[1],h) #rev(predict(hw,n.ahead=h))
        nn$flagObservationStatus <- rep('F', h)
        nn$flagMethod <- rep('e', h)
        y <- rbind(nn,y)
      },
      error = function(e) {y})
    }
  })
})



# Check if negative series


message(paste0('holt winters is done, saving data'))

message(paste0('Print after holt winters function'))

suppressMessages(require(dplyr))
pp_forecasted <- bind_rows(lapply(p5, function(x){bind_rows(x, .id = "column_label")}), .id = "column_label")
pp_forecasted <- pp_forecasted[,c("geographicAreaM49", "measuredElement",
                                  "measuredItemCPC", "timePointYears",
                                  "Value", "flagObservationStatus", "flagMethod"), 
                               with = F] #%>%select(-c(column_label)) ## changed compare with last version

message(paste0('Final forecast'))

# Conversion

# Pull XR dataset
erKey = DatasetKey(
  domain = 'common',
  dataset = 'exchange_rates_annual',
  dimensions = list(
    Dimension(name = 'geographicAreaM49',
              keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[code %in% unique(pp_forecasted$geographicAreaM49), code]),
    Dimension(name = "from_currency",
              keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[code != 'ECU' & endDate >= '1991-01-01' | 
                                                                                       is.na(endDate), code]),
    Dimension(name = "to_currency", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
    Dimension(name = 'measuredElement',
              keys = 'LCU'),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[code %in% unique(pp_forecasted$timePointYears), code]))
  
)

erdt <- GetData(erKey, flags = F) 

erdt[,c('measuredElement', 'to_currency')] <- NULL

pp_converted <- convert_currency(priceData = pp_forecasted, erdt = erdt, sessionElement = 'SLC')

pp2save <- pp_converted[ !is.na(Value)]

message(paste0('Conversion to LCU and USD completed'))

stats <- SaveData(domain = domain,
                  dataset = dataset,
                  data = pp2save[timePointYears > minYear],
                  chunkSize = 50000,
                  waitTimeout = 100000)
message(paste0('Data has been uploaded'))

# write.csv(bubu, file = "hw_forecasted.csv", row.names = FALSE)

