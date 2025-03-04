# preprocessing script to add OMIM to results and find the overlap between OUTRIDER and FRASER data

#Read in omim data
phe <- read.table("./data/omim-phenotype.txt", header=TRUE, fill=TRUE)
colnames(phe)[1]<-"OMIM"

#use org.Hs.eg.db to retrieve OMIM, match ensembl id to omim id and change column name 
columns(org.Hs.eg.db)
cols <- c("SYMBOL", "GENENAME", "OMIM")
omim<-select(org.Hs.eg.db, keys=geneIDs, columns=cols, keytype="ENSEMBL")
colnames(omim)[1]<-'ensembl'

#merge the omim data with the phenotype data
mim_phe<-merge(omim, phe, by="OMIM", all.x = TRUE)

#summarise omim numbers into a single column, so that each ensembl id is on only 1 row (check that the nrows match)
sum_ens <-mim_phe %>%
  group_by(SYMBOL) %>%
  summarise(Phenotypes=paste(Phenotypes, collapse = "; "))

#merge the OUTRIDER and FRASER data with the omim data, write new results tables

##OUTRIDER
ores_mim<-merge(outres, sum_ens, by.x="geneID", by.y="SYMBOL", all.x = TRUE)
ores_mim<-ores_mim[order(ores_mim$padjust),]
write.csv(ores_mim, file = "./data/OUTRIDER-results-MIM.csv", row.names=TRUE, col.names=FALSE)

##FRASER
fres_mim<-merge(frares, sum_ens, by.x="hgncSymbol", by.y="SYMBOL", all.x = TRUE)
fres_mim<-fres_mim[order(fres_mim$padjustGene),]
write.csv(fres_mim, file = "./data/FRASER-results-MIM.csv", row.names=TRUE, col.names=FALSE)

#Merge FRASER and OUTRIDER results
ores_mim<-as.data.frame(ores_mim)
merged<-merge(fres_mim, ores_mim, by.x=c("hgncSymbol","sampleID"), by.y=c("geneID","sampleID"), suffixes = c(".fra",".out"))
merged<-merged %>% dplyr::select(hgncSymbol:padjustGene, potentialImpact:padj_rank)
write.csv(merged, file = "./data/FRASER-OUTRIDER_merged-res.csv", row.names=TRUE, col.names=FALSE)
