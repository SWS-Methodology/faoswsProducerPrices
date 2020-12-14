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



cpi <- fread('C:/Users/Taglionic/Downloads/ConsumerPriceIndices_E_All_Data_(Normalized)/ConsumerPriceIndices_E_All_Data_(Normalized).csv')
names(cpi)

cpi1 <- cpi[,c("Area Code", "Item Code", "Months Code",
               "Year Code", "Value", "Flag", "Note"), with = F]

cpi1[, geographicAreaM49 := fs2m49(`Area Code`) ]
unique(cpi1[is.na(geographicAreaM49)]$`Area Code`)
setnames(cpi1, c("Item Code", "Months Code",
                 "Year Code", "Value", "Flag", "Note"),
         c("measuredElement", "timePointMonths",
           "timePointYears", "Value", "Flag", "Note"))
cpi1[, `Area Code`:= NULL]

unique(cpi1$Flag)
cpi1[Flag == 'E', Flag := 'F']
cpi1[Flag == '', Flag := 'Q']

flags <- ReadDatatable('mapping_input_system_flag_2_sws')
names(flags)
flags <- flags[,c("input_system_flag", "flag_observation_status", 
                  "flag_method", "comments_2_metadata"), with = F]

cpi2 <- merge(cpi1, flags, by.x = 'Flag',
              by.y = 'input_system_flag', all.x = T)

cpi2[is.na(flag_observation_status)]
cpi2[,Flag:=NULL]
unique(cpi2$comments_2_metadata)
cpi2[ , comments_2_metadata := NULL]
cpi2$Value <- as.numeric(cpi2$Value)
cpi2 <- cpi2[!is.na(Value)]
cpi3 <- copy(cpi2)
cpi3[,Note:=NULL]

cpi2[, c('flag_observation_status', 'flag_method', 'Value')] <- NULL
setnames(cpi2, 'Note', 'Metadata_Value')
cpi2[,Metadata:="GENERAL"]
cpi2[,Metadata_Element:="COMMENT"]
cpi2[,Metadata_Language:="en"]
cpi2[,Metadata_Group := c(1:dim(cpi2)[1])]

setcolorder(cpi2,c( "geographicAreaM49","measuredElement",
                    "timePointMonths",
                    "timePointYears", "Metadata",
                    "Metadata_Element", "Metadata_Language",
                    "Metadata_Group","Metadata_Value"))
names(cpi3)
setnames(cpi3, c("flag_observation_status", "flag_method"),
         c("flagObservationStatus", "flagMethod"))

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '3cf3e4e8-ec51-47df-b1b1-006a1b3ee1fb')
}

cpi3[is.na(Value)]

SaveData(domain = 'prod_prices', dataset = 'consumer_price_indices',
         data = cpi3, metadata = cpi2)
