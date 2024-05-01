# --------------------------------------------------------------------------------------------
# Filtering of samples
# --------------------------------------------------------------------------------------------
outdqs.met           <- outdqs %>% dplyr::select(all_of(namesMetOK))
if (excludeOutofRange) 
{
  indSampletoFilter    <- which(rowSums(matrix(as.matrix(outdqs.met) %in% c(codesmissing, codesOutofRange), nrow = nrow(outdqs.met))) > threshold_ind * nrow(outdqs.met))
}else
{
  indSampletoFilter    <- which(rowSums(matrix(as.matrix(outdqs.met) %in% codesmissing, nrow = nrow(outdqs.met))) > threshold_ind * nrow(outdqs.met))
}

IdsFiltered <- NULL
if (length(indSampletoFilter)>0) 
{
  IdsFiltered  <- data.metabo[indSampletoFilter,] %>% dplyr::select(Idepic, Study)
  data.metabo  <- data.metabo[-indSampletoFilter,]
  outdqs       <- outdqs[-indSampletoFilter,]
  # context      <- context[-indSampletoFilter,]
  others       <- others[-indSampletoFilter,]
}

OutlierToEliminate <- NULL

if (filter.outlier) {
  
  
  unzero <- Vectorize(function(x) {ifelse(x==0, 1e-5, x)})
  
  data.metabomod <- data.metabo  %>% mutate_if(is.numeric, function(x){unzero(x)}) %>%
    mutate_if(is.numeric, function(x){log(x)}) %>%
    mutate_if(is.numeric, function(x){x - min(x, na.rm=T) + 0.5}) %>% dplyr::select(-all_of(forIdentifier))
  
  tempdata  <- data.metabomod 
  peakTable <- data.frame(t(tempdata[, -1]))
  peakTable <- cbind(1:nrow(peakTable), peakTable)
  AllObs    <- tempdata %>% dplyr::select(IdentifierPipeline) %>% pull
  colnames(peakTable) <- c("EIC", AllObs)
  # peakTable <- zeroFill(peakTable, obsNames = AllObs)
  NbBatch   <- length(levels(data.metabo$Batch))
  for (kb in levels(data.metabo$Batch))
  {
    ObsBatchk <- AllObs[which(grepl(kb, AllObs))]
    peakTablek <- zeroFill(peakTable[, c(1, which(grepl(kb, colnames(peakTable))))], obsNames = ObsBatchk)
    pcaOutResults_sampleskb <- pcaOutId(peakTablek, ObsBatchk, alfa = (1-.05/NbBatch), outTol=outTol,  
                                        center=T, scale='pareto')
    
    OutlierToEliminate <- c(OutlierToEliminate, ObsBatchk[! (ObsBatchk %in% colnames(pcaOutResults_sampleskb[[1]])) ])
    
  }
  
  data.metabo <- data.metabo %>% filter(!IdentifierPipeline %in%  OutlierToEliminate) %>% arrange(IdentifierPipeline) %>% dplyr::select(-IdentifierPipeline)
  outdqs      <- outdqs %>% filter(!IdentifierPipeline %in%  OutlierToEliminate) %>% arrange(IdentifierPipeline)  %>% dplyr::select(-IdentifierPipeline)
  others      <- others %>% filter(!IdentifierPipeline %in%  OutlierToEliminate) %>% arrange(IdentifierPipeline) %>% dplyr::select(-IdentifierPipeline)
  
}



return(list(data.metabo = data.metabo, outdqs = outdqs, others = others, aux = aux, SHOULDBENA = SHOULDBENA, MetsExcluded = MetsExcluded, IdsFiltered=IdsFiltered, 
            Outliers =  OutlierToEliminate))
}



