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


filename <- list.files('Questionnaires2harvest')
filename <- filename[filename != 'converted']

for(i in 7:length(filename)){
pp <- read_xls(paste('Questionnaires2harvest/',filename[i], sep = ''), sheet = 1)

pp <- as.data.table(pp)

ppmod <- copy(pp)

ppgeo <- ppmod[1,]
ppgeo[ ,FAO := fs2m49(FAO)]

ppcpc <- ppmod[2:nrow(ppmod),]
ppcpc$FAO <- as.character(ppcpc$FAO)
ppcpc[nchar(FAO) == 3 , FAO := paste('0',FAO, sep = '')]
ppcpc[nchar(FAO) == 2 , FAO := paste('00',FAO, sep = '')]
ppcpc[nchar(FAO) == 1 , FAO := paste('000',FAO, sep = '')]

ppcpc[nchar(FAO) == 4 , FAO := fcl2cpc(FAO)]
setnames(ppcpc, 'FAO', 'CPC Code')
setnames(ppgeo, 'FAO', 'CPC Code')

savefile <- rbind(ppgeo, ppcpc)
name2save <- paste('Questionnaires2harvest/converted/', ppgeo$`CPC Code`[1], 'quest.xlsx', sep = '')
write.xlsx(savefile, file = name2save, colNames = TRUE, borders = "rows")
}
