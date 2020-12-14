suppressMessages({
  library(data.table)
  library(faosws)
  library(faoswsUtil)
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


CPIkeys <- DatasetKey(domain = 'prodprices',
                      dataset = 'consumer_price_indices',
                      dimensions = list(
                        Dimension(name = 'geographicAreaM49',
                                  keys = GetCodeList('prodprices', 
                                                     'consumer_price_indices', 
                                                     'geographicAreaM49')[,code]),
                        Dimension(name = 'measuredElement',
                                  keys = GetCodeList('prodprices', 'consumer_price_indices', 
                                                     'measuredElement')[,code]),
                        Dimension(name = 'timePointMonths',
                                  keys = GetCodeList('prodprices', 'consumer_price_indices', 
                                                     'timePointMonths')[,code]),
                        Dimension(name = 'timePointYears',
                                  keys = GetCodeList('prodprices', 'consumer_price_indices', 
                                                     'timePointYears')[,code])))
                                
cpi <- GetData(CPIkeys)

average <- copy(cpi)
average[, Value := mean(Value), by = c('geographicAreaM49',
                                              'measuredElement',
                                              'timePointYears')]

# '7013' Annual Average   
average[, c('timePointMonths', 
            'flagObservationStatus',
            'flagMethod'):= list('7013', 'I', 'i')]

setkey(average)
average <- unique(average) 

stats <- SaveData(domain = 'prod_prices', dataset = 'consumer_price_indices',
         data = average)

msg <- paste0(stats$inserted, " observations written, ",
              stats$ignored, " weren't updated, ",
              stats$discarded, " had problems.")

from = "sws@fao.org"
to = swsContext.userEmail
subject = "CPI_average plug-in completed"
body = paste('The plugin has correctly run. ', 
             msg,
             ' Please check and save data in the SWS. Kind regards', 
             sep = '')
sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)

