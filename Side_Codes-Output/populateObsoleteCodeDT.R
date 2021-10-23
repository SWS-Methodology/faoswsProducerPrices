exp <- read.csv('Migration2sws/ISP_DataHistory_202102081140_old_codes.csv', sep = ';')
library(data.table)
exp <- as.data.table(exp)
names(exp) <- tolower(names(exp))
write.csv(exp, 'tab2import.csv', row.names = F)

chg <- Changeset('pp_obsolete_codes_legacy')
AddInsertions(chg, exp)
Finalise(chg)
