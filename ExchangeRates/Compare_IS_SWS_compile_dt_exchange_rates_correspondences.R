
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
  library(openxlsx)
  library(sendmailR)
})

# -- Token QA ----


if(CheckDebug()){
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])#'4e9d9a2e-5258-48b6-974f-3656d1af8217')
}

dt1 <- ReadDatatable('exchange_rates_correspondences')
setkey(dt1)



dt2 <- ReadDatatable('lcu_2_m49')

names(dt1)
names(dt2)

xrmap <- merge(dt1, dt2, by.x = c("geographicaream49", "currency_code_iso", 'start_year_m49', 'end_year_m49'),
      by.y = c( "code_m49", "code_iso", 'start_year_m49', 'end_year_m49'), all = T,
      suffixes = c('New', 'Old'))

xrmap[start_year_isoNew == start_year_isoOld, startyearcheck := 'ok']
xrmap[(as.numeric(start_year_isoNew) - as.numeric(start_year_isoOld)) == 1, startyearcheck := 'ok1']
xrmap[end_year_isoNew == end_year_isoOld, endyearcheck := 'ok']
xrmap[(as.numeric(end_year_isoNew) - as.numeric(end_year_isoOld)) == 1, endyearcheck := 'ok1']

xrmap[,.N, startyearcheck]
xrmap[,.N, endyearcheck]

xrmap2 <- xrmap[end_year_isoNew > 1969 | is.na(end_year_isoNew)]

#----
tab <- read.xlsx('ExchangeRates/Currency_JMV3-Naglaa_JMV.xlsx')
tab <- as.data.table(tab)
tab[country_code_m49 == '1248', country_code_m49 := '156']
iso2 <- ReadDatatable('a2017regionalgroupings_sdg_feb2017')

comb <- merge(tab, iso2[,.(m49_code, iso2code)],
      by.x = 'country_code_m49', by.y = 'm49_code', all = T)
comb <- as.data.table(comb)
unique(comb[is.na(iso2code)]$name_en_m49)
comb[name_en_m49 == "Antartica", iso2code := 'AQ']
comb[country_code_m49 == "531", iso2code := 'CW']
comb[name_en_m49 == "Saint-Martin" , iso2code := 'MF']
comb[name_en_m49 == "Sint Maarten (Dutch part)", iso2code := 'SX']
comb[name_en_m49 == "Taiwan Province of China", iso2code := 'TW']
comb[name_en_m49 == "USSR(-1991)" , iso2code := 'SU']
comb[name_en_m49 == "Yugoslav SFR(-1991)" , iso2code := 'YU']
comb[name_en_m49 == "Wake Island" , iso2code := 'WK']
comb[name_en_m49 == "Yemen Dem", iso2code := 'YD']
comb[name_en_m49 ==  "NethAntilles",  iso2code := 'AN']
setkey(comb)
# View(unique(comb[,.(iso2code, country_code_m49)]))
# nrow(unique(comb[duplicated(),.(iso2code, country_code_m49)]))
# 
# 
# comb[name_en_m49 == "Yemen Ar Rp", iso2code := '']
# comb[name_en_m49 == "Sudan [former]" , iso2code := ''] ????
# 
# !!!!!comb[name_en_m49 == "Serbia-Monte(1992-2005)", iso2code := 'CS']
# !!!!comb[name_en_m49 ==  "Czechoslovak(-1992)", iso2code := 'CS']
# comb[name_en_m49 == "Channel Is", iso2code := ''] ???

names(comb)

comb[, c("country_code_imf", "country_name_imf",
         "active_flag", "inSWS")] <- NULL

setnames(comb, c("country_code_m49", "country_code_iso", "name_en_m49",
                 "start_year_m49", "end_year_m49", "currency_code_iso",
                 "currency_name_iso", "currency_name_eco_team", "start_year_iso",
                 "end_year_iso", "start_date", "end_date",
                 "start_year_eco_team", "end_year_eco_team",
                 "iso2code"),
         c("geographicaream49", "geographicareaiso3", "country",
           "start_year_m49", "end_year_m49", "currency_code_iso",
           "currency_name_iso", "currency_name_eco_team",
           "start_year_iso", "end_year_iso", 
           "start_date", "end_date",
           "start_year_eco_team", "end_year_eco_team",
           "geographicareaiso2"))

setcolorder(comb,
            c("country","geographicaream49", "geographicareaiso3", 
              "geographicareaiso2", "start_year_m49", "end_year_m49", 
              "currency_name_iso", "currency_name_eco_team",
              "currency_code_iso",
              "start_year_iso", "end_year_iso", 
              "start_date", "end_date",
              "start_year_eco_team", "end_year_eco_team"))

comb <- comb[!is.na(country)]
comb[geographicaream49 == '891', start_year_m49 := '1992']
comb[geographicaream49 == '891', end_year_m49 := '2006']
comb[geographicaream49 == '872', end_year_m49 := '1986']
m49 <- GetCodeList('prodprices', 'annual_producer_prices_prep', 'geographicAreaM49')
m49[code %in% comb[is.na(start_year_m49)]$geographicaream49]
unique(comb[country == 'Georgia']$currency_name_iso)
comb[currency_name_iso == "Georgian kuponi lari", currency_code_iso := 'GEK'] # pair with ruble

xxx <- Changeset('exchange_rates_correspondences')
AddInsertions(xxx, comb)
Finalize(xxx)

xr <- read.xlsx('ExchangeRates/FilesForXRdatasets/currency.xlsx', sheet = 4)
xr <- as.data.table(xr)
names(xr)
xr$CurrencyDollarXC_Year <- as.character(xr$CurrencyDollarXC_Year)
xr$fk_CurrencyDollarXC_AreaCode_Area <- as.character(xr$fk_CurrencyDollarXC_AreaCode_Area)
xr[, geographicaream49 := fs2m49(fk_CurrencyDollarXC_AreaCode_Area)]
unique(xr[is.na(geographicaream49)]$fk_CurrencyDollarXC_AreaCode_Area)
xr[fk_CurrencyDollarXC_AreaCode_Area == '275', geographicaream49 := '412']
names(comb)

codesIS <- unique(xr$fk_CurrencyDollarXC_CurrencyCode_Currency)
all(codesIS %in% xrmap$currency_code_iso)

erKey = DatasetKey(
  domain = 'common',
  dataset = 'exchange_rates_annual',
  dimensions = list(
    Dimension(name = 'geographicAreaM49',
              keys = GetCodeList('common', 'exchange_rates_annual', 'geographicAreaM49')[, code]),
    Dimension(name = "from_currency",
              keys = GetCodeList('common', 'exchange_rates_annual', 'from_currency')[, code]),
    Dimension(name = "to_currency", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'to_currency')[code == 'USD', code]),
    Dimension(name = 'measuredElement',
              keys = 'LCU'),
    Dimension(name = "timePointYears", 
              keys = GetCodeList('common', 'exchange_rates_annual', 'timePointYears')[, code]))
  
)

erdt <- GetData(erKey, flags = F) 
erdt[, measuredElement := NULL]
erdt[, to_currency := NULL]

ers <- merge( comb[,c("country", "geographicaream49",
        "currency_code_iso", "start_year_iso",
        "end_year_iso", "start_date",
        "end_date", "start_year_eco_team",
        "end_year_eco_team"), with = F], erdt,
      by.y = c('geographicAreaM49', 'from_currency'),
      by.x = c('geographicaream49', 'currency_code_iso'), all.y=T)
ers[geographicaream49 == '1248', geographicaream49 := '156']
check <- merge(xr[CurrencyDollarXC_Year > 1989 & !is.na(geographicaream49),c( "fk_CurrencyDollarXC_CurrencyCode_Currency",
                      "CurrencyDollarXC_Year" ,
                      "CurrencyDollarXC_RateUSD",
                      "CurrencyDollarXC_UpdateDate" ,
                      "geographicaream49" ), with = F], 
               
               ers[timePointYears > 1989 & timePointYears < 2019,c("country", "geographicaream49", 'timePointYears',
                  "currency_code_iso", "start_year_iso",
                  "end_year_iso", "start_date",
                  "end_date", "start_year_eco_team",
                  "end_year_eco_team", "Value"), with = F], 
               by.x = c('geographicaream49', 'CurrencyDollarXC_Year'),
               by.y = c('geographicaream49', 'timePointYears'),
               all = TRUE)

#check[ , is_rate := 1/CurrencyDollarXC_RateUSD]

check[dplyr::near(CurrencyDollarXC_RateUSD, Value), check := 'near']
check[Value > 1 & dplyr::near(CurrencyDollarXC_RateUSD, Value, tol = 0.01), check := 'near']
check[Value < 1 & dplyr::near(CurrencyDollarXC_RateUSD, Value, tol = 0.000001), check := 'near']

check[CurrencyDollarXC_RateUSD == Value, check := 'exact']
check[is.na(Value), check := 'Missing IMF']
check[is.na(CurrencyDollarXC_RateUSD), check := 'Missing IS']
check[,.N, check]

check[currency_code_iso == fk_CurrencyDollarXC_CurrencyCode_Currency, check_code := 'ok']
check[currency_code_iso != fk_CurrencyDollarXC_CurrencyCode_Currency, check_code := 'diff']
check[is.na(currency_code_iso), check_code := 'Missin IMF']
check[is.na(fk_CurrencyDollarXC_CurrencyCode_Currency), check_code := 'Missin IS']

check[,.N, check_code]

write.xlsx(check, 'IS-SWS_XRcomparison.xlsx')
