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

#-- Token QA ----

if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = '0211dad4-4c6a-4f66-a2f0-f21a1edef8a2')
}

list.files('ExchangeRates/Data')

link <- fread('ExchangeRates/Data/Corr_LCU_M49.csv', colClasses = rep('character', 2))
lcu <- fread('ExchangeRates/Data/LocalCurrencyUnits.csv', colClasses = rep('character', 23))
lcu[, Identifier := gsub(',', '', Identifier)]
lcu[, Start_Year := gsub(',', '', Start_Year)]
lcu[, End_Year := gsub(',', '', End_Year)]

names(lcu)
lcu[,c("Public","Type", "Name_Fr", "Name_Es", "Name_Ar",
       "Name_Zh", "Name_Ru", "ShortName_En", "ShortName_Fr",
       "ShortName_Es", "ShortName_Ar", "ShortName_Zh", 
       "ShortName_Ru", "Definition", "ScopeNote", 
       "HistoryNote", "EditorialNote", "SubSet")] <- NULL
m49 <- fread('ExchangeRates/Data/Level5_M49.csv', colClasses = rep('character', 23))
m49 <- m49[,c(1:4,7), with =F]
m49$Name_En <- iconv(m49$Name_En)

class(lcu$Identifier)
class(link$Source)
lcu1 <- merge(lcu, link, by.x = 'Identifier', by.y = 'Source', all = T)
lcu1[,Identifier := NULL]
lcum49 <- merge(lcu1, m49, by.x = 'Target', by.y = 'Identifier', all = T,
                suffixes = c('_iso', '_m49'))
misscurr <- lcum49[is.na(Code_iso)]
misscountry <- lcum49[is.na(Code_m49)]

lcum49_2 <- lcum49[!is.na(Code_iso)]
lcum49_2 <- lcum49_2[!is.na(Code_m49)]
lcum49_2[,Target := NULL]
lcum49_2$Code_m49 <- as.character(as.numeric(lcum49_2$Code_m49))

names(lcum49_2) <- tolower(names(lcum49_2))

changeset <- Changeset('lcu_2_m49')
AddInsertions(changeset, lcum49_2)
Finalize(changeset)
