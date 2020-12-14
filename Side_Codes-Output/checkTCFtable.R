domainPP <- 'prod_prices'
datasetVal <- 'annual_producer_prices_validated'
cpc <- GetCodeList(domain =domainPP,
                   dataset = datasetVal, 
                   dimension = "measuredItemCPC")[ , .(code, description)]
dt <- ReadDatatable('pp_tcf')


dt1 <- merge(dt, cpc, by.x = 'cpc2convert',
             by.y = 'code', all.x = T)

dt1[, check := '?']

dt1[nchar(cpc2convert) >= 5 & !is.na(description), check := 'ok' ]
dt1[nchar(cpc2convert) == 4 & is.na(description), cpc2convert := paste('0', cpc2convert, sep ='')]

dt1[nchar(cpc2convert) == 3, cpc2convert := paste('0', cpc2convert, sep ='')]


dt1 <- merge(dt1, cpc, by.x = 'cpc2convert',
             by.y = 'code', all.x = T)


dt1[is.na(description.y)]

dt1[nchar(cpc2convert) >= 6 & is.na(description.y),  cpc2convert := paste('0', cpc2convert, sep ='')]

dt1 <- merge(dt1, cpc, by.x = 'cpc2convert',
             by.y = 'code', all.x = T)
dt1[nchar(cpc2convert) >= 6 & is.na(description),  cpc2convert := paste(cpc2convert,'0', sep ='')]

dt1 <- merge(dt1, cpc, by.x = 'cpc2convert',
             by.y = 'code', all.x = T, suff = c('.z', ''))
dt1 <- as.data.table(dt1)
dt1[,names(dt1)[grepl('description.', names(dt1))]] <- NULL
setnames(dt1, 'description', 'item2convert_sws')

dt1 <- merge(dt1, cpc, by.x = 'cpc_reference',
             by.y = 'code', all.x = T)

dt1[, check := '?']

dt1[nchar(cpc_reference) >= 5 & !is.na(description), check := 'ok' ]
dt1[nchar(cpc_reference) == 4 & is.na(description), cpc_reference := paste('0', cpc_reference, sep ='')]

dt1[nchar(cpc_reference) == 3, cpc_reference := paste('0', cpc_reference, sep ='')]


dt1 <- merge(dt1, cpc, by.x = 'cpc_reference',
             by.y = 'code', all.x = T)


dt1[is.na(description.y)]

dt1[nchar(cpc_reference) >= 6 & is.na(description.y),  cpc_reference := paste('0', cpc_reference, sep ='')]

dt1 <- merge(dt1, cpc, by.x = 'cpc_reference',
             by.y = 'code', all.x = T)
dt1[nchar(cpc_reference) >= 6 & is.na(description),  cpc_reference := paste(cpc_reference,'0', sep ='')]

dt1 <- merge(dt1, cpc, by.x = 'cpc_reference',
             by.y = 'code', all.x = T, suff = c('.z', ''))
dt1 <- as.data.table(dt1)
dt1[,names(dt1)[grepl('description.', names(dt1))]] <- NULL
setnames(dt1, 'description', 'item2convert_sws')

dt1[cpc_reference == '1540', cpc_reference:= '0154']
dt1[cpc_reference == '2211', cpc_reference:= '02211']
dt1 <- merge(dt1, cpc, by.x = 'cpc_reference',
             by.y = 'code', all.x = T, suff = c('.z', ''))
dt1 <- as.data.table(dt1)
dt1[,c(10:11):=NULL, with = FALSE]
setnames(dt1, 'description', 'item_reference_sws')
dt1[,check :=NULL]

saveRDS(dt1, 'TCFdtChecked.rds')

dt2 <- copy(dt1[,.(cpc_reference,
                   cpc2convert,
                   country_code,
                   country,
                   item2convert,
                   item_reference,
                   tcf)])
chg <- Changeset('pp_tcf')
AddInsertions(chg, dt2)
Finalise(chg)
