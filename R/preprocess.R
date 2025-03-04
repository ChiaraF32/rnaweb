### PREPROCESSING SCRIPT for outrider and fraser data 

###########

## Read in outrider data set, add gene names

# remove versioning of gene IDs and merge with GRCh38 from annotables, obtaining HGNC symbols and other descriptions
geneIDs <- gsub("\\.[0-9]*(_[0-9]*)?.*$", "", rownames(ods))
map_ores <- merge(data.table(ensgene=geneIDs), grch38, sort=FALSE,
             all.x=TRUE)[!duplicated(ensgene),] 

# set new gene names only if hgnc symbol is present
if(!"geneID" %in% colnames(mcols(ods))){
  mcols(ods)$ENSG <- geneIDs
  rownames(ods) <- map_ores[,ifelse(
    is.na(symbol) | symbol == "" | duplicated(symbol), geneIDs, symbol)]
}

# generate results
outres<-OUTRIDER::results(ods_hgnc)

#save RDS
saveRDS(ods, file = "./data/muscle/ods-hgnc.Rds")
write.csv(outres, file = "./data/muscle/OUTRIDER_res.csv")

###########

## edit frares results table

# Read in the FRASER results file
frares <- read.table("data/muscle/FRASER_res.tsv", header = TRUE, sep = "\t", stringsAsFactors = TRUE)

# Remove the second-to-last column
frares <- frares[, -ncol(frares) + 1]

# Rename the column hgncSymbol to geneID
colnames(frares)[colnames(frares) == "hgncSymbol"] <- "geneID"

# Move the new geneID column to the first position
frares <- frares[, c("geneID", setdiff(colnames(frares), "geneID"))]

## add OMIM to results 

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

#OUTRIDER
ores_mim<-merge(outres, sum_ens, by.x="geneID", by.y="SYMBOL", all.x = TRUE)
ores_mim<-ores_mim[order(ores_mim$padjust),]
write.csv(ores_mim, file = "./data/muscle/OUTRIDER-results-MIM.csv", row.names=TRUE, col.names=FALSE)

#FRASER
fres_mim<-merge(frares, sum_ens, by.x="geneID", by.y="SYMBOL", all.x = TRUE)
fres_mim<-fres_mim[order(fres_mim$padjustGene),]
write.csv(fres_mim, file = "./data/muscle/FRASER-results-MIM.csv", row.names=TRUE, col.names=FALSE)

###########

## Merge FRASER and OUTRIDER results
ores_mim<-as.data.frame(ores_mim)
merged<-merge(fres_mim, ores_mim, by.x=c("geneID","sampleID"), by.y=c("geneID","sampleID"), suffixes = c(".fra",".out"))
merged<-merged %>% dplyr::select(geneID:sampleID)
write.csv(merged, file = "./data/muscle/FRASER-OUTRIDER_merged-res.csv", row.names=TRUE, col.names=FALSE)

