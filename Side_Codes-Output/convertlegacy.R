suppressMessages({
  library(readxl)
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
  library(openxlsx)
})



#-- Token QA ----

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '0211dad4-4c6a-4f66-a2f0-f21a1edef8a2')
}


ppdt <- ReadDatatable('producer_prices_validated_data_up_to_2017')
saveRDS(ppdt, 'legacypp.rds')

names(ppdt)
unique(ppdt$data_type)
ppann <- ppdt[data_type == 'annual']

ppann[, geographicAreaM49 := fs2m49(fk_data_area_code_area)]

ppann[nchar(fk_data_item_code_item) == 3, 
      fk_data_item_code_item:= paste('0', fk_data_item_code_item, sep = '')]

ppann[nchar(fk_data_item_code_item) == 2, 
      fk_data_item_code_item:= paste('00', fk_data_item_code_item, sep = '')]

ppann[ , measuredItemCPC := fcl2cpc(fk_data_item_code_item)]
setnames(ppann, c('fk_data_element_code_element', 'data_year'), 
         c('measuredElement', 'timePointYears'))

conflag <- ReadDatatable('mapping_input_system_flag_2_sws')
names(conflag)

ppannfl <- merge(ppann, conflag[, .(input_system_flag,
                                    flag_observation_status,
                                    flag_method)], 
      by.x = 'fk_data_symbol_code_symbol',
      by.y = 'input_system_flag', all.x = T)


setnames(ppannfl, c('flag_observation_status',
                    'flag_method', 'value'),
         c('flagObservationStatus',
           'flagMethod', 'Value'))

names(ppannfl)
ppannfl[,c("fk_data_symbol_code_symbol",
           "fk_data_area_code_area",
           "fk_data_item_code_item",
           "data_type")] <- NULL

