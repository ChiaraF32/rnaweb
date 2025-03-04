# exploratory RNAvariant calling
res_plot <- copy(vds)

# Not needed or plotting
res_plot[,GENE_ID := NULL]
res_plot[,GENE_NAME := NULL]
res_plot[,MAX_AF := NULL]
res_plot[,cohortFreq := NULL]
res_plot[,VARIANT := NULL]

vds$cohortFreq <- round(vds$cohortFreq,3)

#' ## Variant Calling Tables (first 1,000)

DT::datatable(
  head(vds[grepl("PASS", FILTER)], 1000),
  caption = 'Variants called from RNA (up to 1,000 rows shown)',
  options=list(scrollX=TRUE),
  filter = 'top')

if (!all(is.na(vds$MAX_AF))) {
  DT::datatable(
    head(vds[FILTER == "PASS_rare" & `D21-0076` %in% c("0/1", "1/1")], 1000),
    caption = 'Rare Variants called from RNA (up to 1,000 rows shown)',
    options=list(scrollX=TRUE),
    filter = 'top')
}

if (!all(is.na(vds$MAX_AF))) {
  DT::datatable(
    head(vds[FILTER == "PASS_rare"], 1000),
    caption = 'Rare Variants called from RNA (up to 1,000 rows shown)',
    options=list(scrollX=TRUE),
    filter = 'top')
}

# melt filters by GT. Exclude reference calls. Read in batches to avoid vector length errors for large datasets

# generate batch lists by config value: yieldSize
batches <- seq(0,nrow(res_plot),100000)
if(batches[length(batches)] < nrow(res_plot)) batches <- c(batches,nrow(res_plot))

# build dts by batches
out <- lapply(1:(length(batches)-1), function(i){
  melt(res_plot[(1 +batches[i]):batches[i+1]], #read through batches 1
       id.vars = "FILTER",value.name = "GT")[GT != "0/0",.N,by = c("FILTER","variable","GT")]
})

# combine batches and sum up the variables
res_plot <- rbindlist(out)[,.(N = sum(N)),by = c("FILTER","variable","GT")]

#' ## Table of variant calls by GT (first 1,000)
summary_dt <- dcast(res_plot, FILTER + GT ~ variable, value.var = "N")
DT::datatable(
  head(summary_dt,1000),
  caption = "Variant filters by GT", 
  options=list(scrollY=TRUE),
  filter = 'top')

#' ### Breakdown of variants by GT
ggplot(res_plot, aes(x = FILTER, y = N,col = GT)) +
  geom_boxplot() +
  geom_text(data = res_plot[,median(N),by=c("FILTER","GT")],
            mapping = aes(x=FILTER,y= V1,label = V1, vjust = -0.5),position = position_dodge(0.9),show.legend = F,size = 3.5) +
  ylab("Variants per sample") + scale_x_discrete(guide = guide_axis(n.dodge = 2)) + theme_bw()

# Split res
res_plot[grepl("PASS",FILTER),FILTER := "PASS"]
res_plot[!grepl("PASS",FILTER),FILTER := "FILTERED OUT"]

res_plot_summary <- res_plot[,sum(N),by = .(FILTER,variable,GT)]

#' ### Plot only Pass/Fail split
ggplot(res_plot_summary, aes(x = FILTER, y = V1,col = GT)) +
  geom_boxplot() +
  geom_text(data = res_plot_summary[,median(V1),by=c("FILTER","GT")],
            mapping = aes(x=FILTER,y= V1,label = V1, vjust = -0.5),position = position_dodge(0.9),show.legend = F,size = 3.5) +
  ylab("Variants per sample") + scale_x_discrete(guide = guide_axis(n.dodge = 2)) + theme_bw()

