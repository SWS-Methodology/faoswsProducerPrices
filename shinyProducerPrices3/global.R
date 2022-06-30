# The shiny app is based on the Macro used during the last ten years by the producer prices team.

.libPaths( c( 
  "/home/shiny/R/x86_64-pc-linux-gnu-library/3.2",
  "/usr/local/lib64/R-3.1.2/library",
  "/work/SWS_R_Share/shiny/Rlib/3.1",
  .libPaths()))


suppressMessages({
  
  library(plotly)
  #library(data.table)
  library(DT)
  #library(ggplot2)
  library(zoo)
  
  library(rhandsontable)
  library(shiny)
  library(shinyWidgets)
  #library(shinyalert)
  library(stringr)
  library(faosws)
  library(faoswsFlag)
  library(faoswsProcessing)
  library(faoswsUtil)
  library(faoswsImputation)
  library(dplyr)
  library(plyr)
  library(tseries)
  library(forecast)
  library(imputeTS)
})

localrun <- TRUE
source('conversion_function.R')
source('best_Model_function.R')
source('modelRevision.R')
source('modified_functions.R')
#source('fix_xr.R')
#-- Token QA ----

if(localrun){
  if(CheckDebug()){
    library(faoswsModules)
    SETTINGS = ReadSettings("C:/Users/Taglionic/OneDrive - Food and Agriculture Organization/Desktop/shinyProducerPrices3/sws.yml")
    R_SWS_SHARE_PATH = SETTINGS[["share"]]
    SetClientFiles(SETTINGS[["certdir"]])
    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                       token =SETTINGS[["token"]])  #'0211dad4-4c6a-4f66-a2f0-f21a1edef8a2')
  }
  
} else {
  R_SWS_SHARE_PATH = "Z:"
  SetClientFiles("/srv/shiny-server/.R/PROD/")
  GetTestEnvironment(baseUrl = "https://sws.aws.fao.org:8181",
                     token = "9c7fd281-56a2-410e-801d-602677b8ee5a")
}

options(scipen=999)

message('Connected')

tokenTab <- data.table() #ReadDatatable('pp_token', readOnly = FALSE)

message('Load token dt')

tokenOutlier <- c()#tokenTab[dataset == 'Annual Producer Prices (Preparation)']$token
tokenSeries <- tokenOutlier # tokenTab[dataset == 'Annual Producer Prices (Preparation)']$token

# dateOutlierToken <- tokenTab[dataset == 'Annual Producer Prices (Preparation)']$last_upd
protectedFlags <- c('', 'X', 'B')
domainPP <- 'prod_prices'
datasetPrep <- 'annual_producer_prices_prep'
datasetVal <- 'annual_producer_prices_validation'

# Country input all countries in M49
M49 <- GetCodeList(domain =domainPP,
                   dataset = datasetPrep,
                   dimension = "geographicAreaM49")

M49 <- M49[ type == "country", .( description, code)]
M49[, description := iconv(description, from = 'UTF-8')]

country_input <-  sort(sprintf("%s - %s", M49$description, as.numeric(M49$code)))
country_input <- data.table(label = country_input, code = sub(" ", "", sub(".*-", "", country_input)))

# Years from 1991 until present
currentYear <- as.numeric(gsub("\\-[0-9]*", "", Sys.Date()))
years_input <- as.character(sort(1991:currentYear, decreasing = TRUE))

# Products from CPC dimension
cpc <- GetCodeList(domain = domainPP,
                   dataset = datasetPrep,
                   dimension = "measuredItemCPC")[ , .(code, description)]

cpc_list <- sort(sprintf("%s - %s", cpc$description, cpc$code))
cpc_list <- data.table(label = cpc_list, code = sub(" ", "", sub(".*-", "", cpc_list)))

currElement <- c('5531', '5532', '5530')
SLCel <- '5531'

message('Codelists loaded')
#-- Datasets ----

dataPP <- reactiveValues(prepUSD =  data.table(),#priceDataUSD,
                         prepSLC = data.table(),#priceDataSLC,
                         prepOut = data.table(),#rbind(priceDataLCU[flagObservationStatus == 'E' & flagMethod == 'e'],
                         #priceDataSLC[flagObservationStatus == 'E' & flagMethod == 'e'],
                         #priceDataUSD[flagObservationStatus == 'E' & flagMethod == 'e']),
                         validated = data.table(),#priceData,
                         valRev = data.table()) #,#priceproposed)


priceDataPast <- readRDS('priceDataFull.rds')
# priceDataPast <- priceDataPast[timePointYears >= 2000]
# priceData[,timePointYears_description := NULL]
#
priceDataLCU <- priceDataPast[measuredElement == '5530' ]
priceDataSLC <- priceDataPast[measuredElement == '5531' ]
priceDataUSD <- priceDataPast[measuredElement == '5532' ]

maxYear <- as.character(max(as.numeric(priceDataPast$timePointYears)))

#-- Estimated data proposed

priceproposed <- data.table()#ReadDatatable('imputation_annual_prices', readOnly = F)

# Do not include already imputed values
imputed <- data.table() # priceproposed[selected == TRUE]
# priceproposed <- priceproposed[!imputed, on = c("geographicaream49",
#                                                 "measureditemcpc",
#                                                 "timepointyears" )]
countries_imp <- c() #unique(priceproposed$geographicaream49)

approachesTot <- ReadDatatable('imputation_annual_prices', columns = 'approach', readOnly = T)
approachList <- unique(approachesTot$approach)

#-- Interpolated data proposed ----

interpolation0 <- data.table() #ReadDatatable('interpolation_annual_prices', readOnly = F)
interpolation <- data.table() #copy(interpolation0[selected == FALSE])

# Do not include already imputed values
priceintproposed <- data.table() # interpolation[, on = c("geographicaream49",
                      #                          "measureditemcpc",
                     #                            "timepointyears" )]
countries_int <- c() #unique(priceintproposed$geographicaream49)


# -- Price ratio ----

# ppr <- ReadDatatable('pp_tcf')

# -- Geo and CPC hierachy + geo-currency link

geohier <- ReadDatatable('geographic_hierarchy_ebx5')
cpchier <- ReadDatatable('cpc_hierarchy_ebx5')
#lcu_2_m49 <- ReadDatatable('lcu_2_m49')
#eco_curr0 <- ReadDatatable('currency_country_years')
## xr_corr <- ReadDatatable('exchange_rates_correspondences')
## xrcountry <-  ReadDatatable('currency_changes')
PPIaggr <- ReadDatatable('aggregate_groups_faostat_sws', where = "var_type = 'item' AND domain_code = 'PI'")
outlier_det <- data.table() # ReadDatatable('outlier_detected')
country_out <- c() #country_input[code %in% unique(outlier_det$geographicaream49)]
year_out <- c() #unique(outlier_det$timepointyears)
startYear <- c() #ifelse(outlier_det[,.N] == 0, as.character(as.numeric(format(Sys.Date(), '%Y')) - 10),
            #         as.character(as.numeric(year_out) - 10))

message('Datatable loaded')

# Check if all products are there!!!!!!!1
comm_groups <- unique(cpchier[code_l2 %in% c('01','02','21') & !is.na(name_en_l3),.(name_en_l3, code_l3)])

