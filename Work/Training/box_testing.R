# the pmp SE object
SE = MTBLS79

# convert to DE
DE = as.DatasetExperiment(SE)
DE$name = 'MTBLS79'
DE$description = 'Converted from SE provided by the pmp package'

# add a column indicating the order the samples were measured in
DE$sample_meta$run_order = 1:nrow(DE)

# add a column indicating if the sample is biological or a QC
Type=as.character(DE$sample_meta$Class)
Type[Type != 'QC'] = 'Sample'
DE$sample_meta$Type = factor(Type)

# add a column for plotting batches
DE$sample_meta$batch_qc = DE$sample_meta$Batch
DE$sample_meta$batch_qc[DE$sample_meta$Type=='QC']='QC'

# convert to factors
DE$sample_meta$Batch = factor(DE$sample_meta$Batch)
DE$sample_meta$Type = factor(DE$sample_meta$Type)
DE$sample_meta$Class = factor(DE$sample_meta$Class)
DE$sample_meta$batch_qc = factor(DE$sample_meta$batch_qc)

# print summary
DE

### SINGAL DRIFT AND BATCH CORRECTION
M = # batch correction
  sb_corr(
    order_col='run_order',
    batch_col='Batch', 
    qc_col='Type', 
    qc_label='QC',
    spar_lim = c(0.6,0.8) 
  )

M = model_apply(M,DE)

C = feature_profile(
  run_order='run_order',
  qc_label='QC',
  qc_column='Type',
  colour_by='batch_qc',
  feature_to_plot='200.03196',
  plot_sd=FALSE
)

# plot and modify using ggplot2 
chart_plot(C,M,DE)+ylab('Peak area')+ggtitle('Before')

chart_plot(C,predicted(M))+ylab('Peak area')+ggtitle('After')

M2 = filter_na_count(
  threshold=3,
  factor_name='Batch'
)
M2 = model_apply(M2,predicted(M))

# calculate number of features removed
nc = ncol(DE) - ncol(predicted(M2))

cat(paste0('Number of features removed: ', nc))

M3 = kw_rank_sum(
  alpha=0.0001,
  mtc='none',
  factor_names='Batch',
  predicted='significant'
) +
  filter_by_name(
    mode='exclude',
    dimension = 'variable',
    seq_in = 'names', 
    names='seq_fcn', # this is a placeholder and will be replaced by seq_fcn
    seq_fcn=function(x){return(x[,1])}
  )
M3 = model_apply(M3, predicted(M2))

nc = ncol(predicted(M2)) - ncol(predicted(M3))
cat(paste0('Number of features removed: ', nc))