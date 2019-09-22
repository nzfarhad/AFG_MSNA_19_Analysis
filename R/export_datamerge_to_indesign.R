
ind <- read_excel('input/indesign/afg_reach_woa_r_indesign_pdf_field_mapping_v1.xlsx', sheet = "FSA")
dm <- read.csv('input/indesign/fsa_datamerge_all.csv')
ind_all <- ind %>% filter(ind$`R item` != "NA")
dm1 <- dm[, ind_all$`R item`] 

names(dm1) <- ind_all$`data merge item`[ind_all$`R item` %in% names(dm1)]
write.csv(dm1, 'input/indesign/afg_reach_woa_fsa_datamerge.csv',row.names = FALSE)
