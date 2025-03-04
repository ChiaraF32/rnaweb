#define the plotVolcano function here
plotVolcano.OUTRIDER <- function(object, sampleID, main, padjCutoff=0.05,
                                 zScoreCutoff=0, label="",
                                 xaxis=c("zscore", "log2fc", "fc"),
                                 pch=16, basePlot=FALSE, col=c("gray", "firebrick")){
  if(missing(sampleID)){
    stop("specify which sample should be plotted, sampleID = 'sample5'")
  }
  if(!all(c('padjust', 'zScore') %in% assayNames(object))){
    stop('Calculate Z-scores and P-values first.')
  }
  if(is.logical(sampleID)){
    sampleID <- which(sampleID)
  }
  if(is.numeric(sampleID)){
    if(!(is.numeric(sampleID) && max(sampleID) <= ncol(object))){
      stop('Sample index is out of bounds:',
           paste(sampleID, collapse=", "))
    }
    sampleID <- colnames(object)[sampleID]
  }
  if(!all(sampleID %in% colnames(object))){
    stop("Sample ID is not in the data set.")
  }
  if(length(sampleID) > 1){
    ans <- lapply(sampleID, plotVolcano, object=object, 
                  padjCutoff=padjCutoff,
                  zScoreCutoff=zScoreCutoff, basePlot=basePlot)
    return(ans)
  }
  if(missing(main)){
    main <- paste0("Volcano plot: ", sampleID)
  }
  
  if(is.null(rownames(object))){
    rownames(object) <- paste("feature", seq_len(nrow(object)), sep="_")
  }
  
  xaxis <- match.arg(xaxis)
  if(xaxis == "zscore"){
    if("zScore" %in% assayNames(object)){
      xaxis <- zScore(object)
      xaxis_name <- "Z-Score"
      base_x_lab <- xaxis_name
    } else{
      stop("Calculate zScores first or choose other xaxis type.")
    }
    
  } else if(xaxis == "fc"){
    if("l2fc" %in% assayNames(object)){
      xaxis <- 2^assay(object, "l2fc")
      xaxis_name <- "Fold change"
      base_x_lab <- xaxis_name
    } else{
      stop("Calculate log2fc first or choose other xaxis type.")
    } 
  } else if(xaxis == "log2fc"){
    if("l2fc" %in% assayNames(object)){
      xaxis <- assay(object, "l2fc")
      xaxis_name <- expression(paste(log[2], "(fold-change)"))
      base_x_lab <- paste("log<sub>2</sub>(fold-change)")
    } else{
      stop("Calculate log2fc first or choose other xaxis type.")
    } 
  } else {
    stop("Unknown xaxis type, choose one of zscore, fc, log2fc.")
  }
  
  dt <- data.table(
    GENE_ID   = rownames(object),
    pValue    = pValue(object)[,sampleID],
    padjust   = padj(object)[,sampleID],
    # zScore    = zScore(object)[,sampleID],
    xaxis        = xaxis[,sampleID],
    normCts   = counts(object, normalized=TRUE)[,sampleID],
    medianCts = rowMedians(counts(object, normalized=TRUE), useNames = FALSE),
    expRank   = apply(
      counts(object, normalized=TRUE), 2, rank)[,sampleID],
    aberrant  = aberrant(object, padjCutoff=padjCutoff,
                         zScoreCutoff=zScoreCutoff)[,sampleID],
    color     = col[1])
  dt[aberrant == TRUE, color:=col[2]]
  
  # remove the NAs from the zScores for plotting
  dt[is.na(xaxis),xaxis:=0]
  
  p <- ggplot(dt, aes(xaxis, -log10(pValue), color=color, label=GENE_ID, 
                      text=paste0(
                        "Gene ID: ", GENE_ID,
                        "<br>Sample ID: ", sampleID,
                        "<br>Median normcount: ", round(medianCts, 2),
                        "<br>normcount: ", round(normCts, 2),
                        "<br>expression rank: ", as.integer(expRank),
                        "<br>nominal P-value: ", signif(pValue,3),
                        "<br>adj. P-value: ", signif(padjust,3),
                        "<br>", xaxis_name, ": ", signif(xaxis,2)))) + 
    geom_point() + 
    theme_bw() + 
    xlab(xaxis_name) + 
    ylab(expression(paste(-log[10], "(", italic(P), "-value)"))) + 
    ggtitle(main) + 
    scale_color_identity() + 
    theme(legend.position = 'none')
  
  # Log scale if fold change is plotted
  if(isTRUE(basePlot)){
    if(!is(xaxis_name, "expression") && xaxis_name == 'Fold change'){
      p <- p + scale_x_log10(labels = scales::trans_format(
        "log10", scales::math_format(10^.x)))
    }
    if(!is.null(label)){
      if(label == "aberrant"){
        if(nrow(dt[aberrant == TRUE,]) > 0){
          p <- p + 
            geom_text_repel(data=dt[aberrant == TRUE,], 
                            aes(col=color), fontface='bold', 
                            hjust=-.2, vjust=.2)
        }
      }
      else{
        if(nrow(dt[GENE_ID %in% label]) > 0){
          p <- p + 
            geom_text_repel(data=subset(dt, GENE_ID %in% label), 
                            aes(col=color), fontface='bold', 
                            hjust=-.2, vjust=.2)
        }
        if(any(!(label %in% dt[,GENE_ID]))){
          warning("Did not find gene(s) ", 
                  paste(label[!(label %in% dt[,GENE_ID])], 
                        collapse=", "), " to label.")
        }
      }
    }
  }
  
  if(isFALSE(basePlot)){
    p <- p + ylab(paste("-log<sub>10</sub>(<i>P</i>-value)"))
    return(plotly::ggplotly(p, tooltip="text"))        
  }
  p
}