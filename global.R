# The shiny app is based on the Macro used during the last ten years by the producer prices team.

 .libPaths( c("/usr/local/lib64/R-3.1.2/library","/work/SWS_R_Share/shiny/Rlib/3.1",.libPaths()))
 

suppressMessages({
  library(data.table)
  library(DT)
  library(faosws)
  library(faoswsFlag)
  library(faoswsProcessing)
  library(faoswsUtil)
  library(faoswsImputation)
  library(ggplot2)
  library(rhandsontable)
  library(shiny)
  library(shinyWidgets)
})

localrun <- TRUE

#-- Token QA ----

if(localrun){
  if(CheckDebug()){
    library(faoswsModules)
    SETTINGS = ReadSettings("sws.yml")
    R_SWS_SHARE_PATH = SETTINGS[["share"]]
    SetClientFiles(SETTINGS[["certdir"]])
    GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                       token = '0211dad4-4c6a-4f66-a2f0-f21a1edef8a2')
  }
} else {
  R_SWS_SHARE_PATH = "Z:"
  SetClientFiles("/srv/shiny-server/.R/QA/")
  GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                     token = "54992801-519f-4d80-89f4-2de4aadada87")
}

domainPP <- 'prod_prices'
datasetVal <- 'annual_producer_prices_validation'
datasetPrep <- 'annual_producer_prices_prep'
LCUcode <- '5530'

# Country input all countries in M49
M49 <- GetCodeList(domain = domainPP, 
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
cpc <- GetCodeList(domain =domainPP,
                   dataset = datasetPrep, 
                   dimension = "measuredItemCPC")[ , .(code, description)]

# cpc_list <- sort(sprintf("%s - %s", cpc$description, cpc$code))
# cpc_list <- data.table(label = country_input, code = )

# Elements 
measEls <- GetCodeList(domain = domainPP, dataset = datasetPrep,
                        dimension = 'measuredElement')[, .(code, description)]
  
