library(openxlsx)
library(readxl)
current <- ReadDatatable('lcu_2_m49')
withdr <- read_xls('C:/Users/Taglionic/Downloads/list_three.xls', skip = 3)
m49 <- GetCodeList('common_domain', 'exchange_rates_common', 'geographicAreaM49')[,.(description, code)]

withdr$ENTITY <- tolower(withdr$ENTITY)
m49$description <- tolower(m49$description) 

all <- merge(m49, withdr, by.x = 'description', by.y = 'ENTITY', all.y = T)

all[is.na(code)]$description

all[description == "bolivia", code := '68']
all[description == "burma ", code := '104']
all[description == "czechoslovakia", code := '200']
all[description == "french  guiana", code := '254']
all[description == "french southern territories", code := '260']
#all[description == "german democratic republic", code := '68']
all[description == "holy see (vatican city state)", code := '336']
all[description == "lao", code := '418']
all[description == "moldova, republic of", code := '498']
all[description == "netherlands antilles", code := '530']
all[description == "saint martin", code := '663']
all[description == "saint-barthélemy", code := '652']
all[description == "serbia and montenegro", code := '891']
all[description == "southern rhodesia ", code := '716']
all[description == "swaziland", code := '748']
all[description == "union of soviet socialist republics", code := '810']
all[description == "united states", code := '840']
all[description == "venezuela", code := '862']
all[description == "yemen, democratic", code := '720']
all[description == "yugoslavia", code := '890']
all[description == "zaire", code := '180']
all[description == "vietnam", code := '704']
all$description <- NULL

m49 <- GetCodeList('common_domain', 'exchange_rates_common', 'geographicAreaM49')[,.(description, code)]
upload <- merge(all, m49, by = 'code')
upload$Funds <-NULL

dt <- ReadDatatable('lcu_withdrawal')
names(dt)

setnames(upload, c("code",
                   "Historic currency",
                   "Alphabetic Code",
                   "Withdrawal Date",
                   "description"),
         c("code_m49",
           "name_en_iso",
           "code_iso",
           "withdrawal_date",
           "name_en_m49"))

setcolorder(upload, c( "name_en_m49",
                       "code_m49" ,
                       "name_en_iso" , 
                       "code_iso" ,
                       "withdrawal_date"))
chng <- Changeset('lcu_withdrawal')
AddInsertions(chng, upload)
Finalize(chng)
