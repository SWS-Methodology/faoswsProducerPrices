library(openxlsx)
library(data.table)
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


if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = 'c9cd8ef0-5dee-49ed-8b40-93c96c917fdf')
}

iso <- ReadDatatable('iso_4217_currency')
iso <- ReadDatatable('currency_changes')
iso <- ReadDatatable('currency_conversion_rate')
iso <- ReadDatatable('country_currency')

imfa <- read.csv('ExchangeRates/FilesForXRdatasets/IMFannual.csv', skip = 1)
imfa <- as.data.table(imfa)
imfa[, X.1 := NULL ]

imfgeo <- imfa$X
imfgeo <- data.table(`Country.Name` = imfgeo)

geocodeIMF <- read.csv('ExchangeRates/FilesForXRdatasets/Metadata_IFS.csv')
geocodeIMF <- as.data.table(geocodeIMF[, c("Country.Name", "Country.Code", "Metadata.Attribute", "Metadata.Value")])
geocodeIMF <- geocodeIMF[Metadata.Attribute == 'Country ISO 3 Code']

geocode <- merge(geocodeIMF, imfgeo, by = 'Country.Name', all = T)

isom49 <- ReadDatatable('m49_fs_iso_mapping')
isom49 <- unique(isom49[,.(m49, iso3)])
geocodem49 <- merge(geocode, isom49, by.x = 'Metadata.Value', by.y = 'iso3', all = TRUE)
geocode[,Metadata.Attribute := NULL]
setnames(geocode, c('Country.Name', 'Metadata.Value', 'Country.Code', 'm49'),
         c('name_imf','code_country_iso', 'code_imf', 'code_m49'))

currency <- ReadDatatable('lcu_2_m49')

completetab <- merge(geocode, currency, by = 'code_m49', all = T)
write.csv(completetab, 'geo-curr-year.csv', row.names = F)

tab <- read.xlsx('geo-curr-year.xlsx')
tab <- as.data.table(tab)
pptab <- ReadDatatable('lcu_withdrawal')

tab$name_en_m49[!tab$name_en_m49 %in% pptab$name_en_m49]
pptab$name_en_m49[!pptab$name_en_m49 %in% tab$name_en_m49]

compare <- merge(tab, pptab, by = c('name_en_m49', 'code_m49', 'code_iso'), all = T, allow.cartesian = T, suffixes = c('new','pp'))

swscurr <- GetCodeList('common', 'exchange_rates_annual', 'from_currency')
swscurr$code[!swscurr$code %in% compare$code_iso]
unique(compare$code_iso[!compare$code_iso %in% swscurr$code])

swsgeo <- GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')

curr1 <- read.xlsx('ExchangeRates/FilesForXRdatasets/currency.xlsx', sheet = 1)
curr2 <- read.xlsx('ExchangeRates/FilesForXRdatasets/currency.xlsx', sheet = 2)
curr1 <- as.data.table(curr1)
curr2 <- as.data.table(curr2)

curr1[ , Currency_StartDate := as.Date( Currency_StartDate, format = "%d/%m/%Y", origin = "30/12/1899")]
curr1[ , Currency_EndDate := as.Date( Currency_EndDate, format = "%d/%m/%Y", origin = "30/12/1899")]
curr1[Currency_StartDate == '1899-12-31', Currency_StartDate := as.Date('1900-01-01')]

curr2[ , CurrencyCountry_StartDate := as.Date(CurrencyCountry_StartDate, format = "%d/%m/%Y", origin = "30/12/1899")]
curr2[ , CurrencyCountry_EndDate := as.Date(CurrencyCountry_EndDate, format = "%d/%m/%Y", origin = "30/12/1899")]
curr2[CurrencyCountry_StartDate == '1899-12-31', CurrencyCountry_StartDate := as.Date('1900-01-01')]
curr2$fk_CurrencyCountry_AreaCode_Area <- as.character(curr2$fk_CurrencyCountry_AreaCode_Area)
curr2[ , fk_CurrencyCountry_AreaCode_Area := fs2m49(fk_CurrencyCountry_AreaCode_Area)]
curr2[is.na(fk_CurrencyCountry_AreaCode_Area) & fk_CurrencyCountry_CurrencyCode_Currency == 'FKP', fk_CurrencyCountry_AreaCode_Area := '238']
curr2[is.na(fk_CurrencyCountry_AreaCode_Area) & fk_CurrencyCountry_CurrencyCode_Currency == 'CNY', fk_CurrencyCountry_AreaCode_Area := '156']


missing <- curr1$pk_Currency_Code[!curr1$pk_Currency_Code %in% swscurr$code]
length(unique(curr1$pk_Currency_Code))

View(curr1[pk_Currency_Code %in% missing])

curr1[Currency_ActiveFlag == 0, active_flag := FALSE]
curr1[Currency_ActiveFlag == -1, active_flag := TRUE]
curr1[, Currency_ActiveFlag := NULL]
setnames(curr1, names(curr1)[-5], c('currency_code', 'currency_name', 'start_date', 'end_date'))

cng <- Changeset('currency_dates')
AddInsertions(cng, curr1)
Finalise(cng)

curr2[CurrencyCountry_ActiveFlag == 0, active_flag := FALSE]
curr2[CurrencyCountry_ActiveFlag == -1, active_flag := TRUE]
curr2[, CurrencyCountry_ActiveFlag := NULL]

setnames(curr2, names(curr2), c('country_code_m49','currency_code', 'start_date', 'end_date',
                                    'start_year', 'end_year', 'active_flag'))

chg <- Changeset('currency_country_years')
AddInsertions(chg, curr2)
Finalise(chg)

ppleg <- ReadDatatable('currency_country_years')
tab <- read.xlsx('geo-curr-year.xlsx')
tab <- as.data.table(tab)
tab[code_m49 == '891', code_iso := 'CSD']

namespp <- ReadDatatable('currency_dates')
dupls <- unique(namespp[duplicated(currency_code)]$currency_code)
uniquenames <- rbind(namespp[!currency_code %in% dupls],  namespp[currency_code %in% dupls & active_flag == TRUE])


ppleg2 <- merge(ppleg, uniquenames, by = c('currency_code', 'start_date', 'end_date'), all = T)

check <- merge(tab, ppleg, by.x = c('code_m49', 'code_iso'), by.y = c('country_code_m49', 'currency_code'), all = T)


check[, inSWS := FALSE]
check[code_iso %in% swscurr$code, inSWS := TRUE ]
check2 <- merge(check, uniquenames[,.(currency_code, currency_name)],
                by.x = 'code_iso', by.y = 'currency_code', all.x = T)
check2 <- as.data.table(check2)
check <- as.data.table(check)

countries <- unique(check2[!is.na(start_year_m49),.(code_m49, code_country_iso, code_imf, name_imf, name_en_m49, start_year_m49, end_year_m49)])
countries <-countries[name_imf !=  'Eastern Germany']

check2 <- merge(check2, countries, 
                by = 'code_m49', suffixes = c('', 'name'), all.x = T)
check2[is.na(name_en_m49), name_en_m49 := name_en_m49name]
check2[is.na(code_country_iso), code_country_iso := code_country_isoname]
check2[is.na(name_imf), name_imf := name_imfname]
check2[is.na(start_year_m49), start_year_m49 := start_year_m49name]
check2[is.na(end_year_m49), end_year_m49 := end_year_m49name]
check2[is.na(code_imf), code_imf := code_imfname]

check2[ , c('code_country_isoname', 'code_imfname', 'name_imfname', 'name_en_m49name', 'start_year_m49name', 'end_year_m49name')] <- NULL

write.xlsx(check, 'currencyCorrespondences.xlsx')
write.xlsx(check2, 'currencyCorrespondences_names.xlsx')

