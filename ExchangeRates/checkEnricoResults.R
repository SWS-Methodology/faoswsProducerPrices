
ann <- fread('export_annual.csv', colClasses = c('character', 
                                          'character',
                                          'character',
                                          'character',
                                          'character',
                                          'numeric',
                                          'character',
                                          'character'))
names(ann)
comp <- merge(ann, erdt, by = c("geographicAreaM49",
                                "from_currency",
                                "to_currency",
                                "measuredElement",
                                "timePointYears"), all = T)


monthly <- fread('export_monthly.csv')
