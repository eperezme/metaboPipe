filtered_experiment$sample_meta$order <- factor(filtered_experiment$sample_meta$order)
filtered_experiment$sample_meta$biol.batch <- factor(filtered_experiment$sample_meta$biol.batch)

# ST001764
# https://www.ebi.ac.uk/metabolights/editor/MTBLS7832/files

View(DE$sample_meta)
View(DE$variable_meta)
View(filtered_experiment$sample_meta)
View(filtered_experiment$variable_meta)

DE$sample_meta$run_order <- 1:nrow(DE$sample_meta)
DE$sample_meta$Type <- factor(DE$sample_meta$sample_type)
DE$sample_meta$batch_qc <- DE$sample_meta$biol.batch
DE$sample_meta$batch_qc[DE$sample_meta$Type == "QC"] <- "QC"
DE$sample_meta$batch_qc[DE$sample_meta$Type == "Blank"] <- "Blank"

# convert to factors
DE$sample_meta$biol.batch <- factor(DE$sample_meta$biol.batch)
DE$sample_meta$Type <- factor(DE$sample_meta$Type)
DE$sample_meta$Class <- factor(DE$sample_meta$Class)
DE$sample_meta$batch_qc <- factor(DE$sample_meta$batch_qc)


#### BATCH CORRECTION ####
M <- # batch correction
  sb_corr(
    order_col = "order",
    batch_col = "biol.batch",
    qc_col = "sample_type",
    qc_label = "QC",
    # spar_lim = c(0.6,0.8)
  )


#### BLANK FILTER ####
# HOW MANY FOLD CHANGES? DEPENDS ON SAMPLES? ON METABOLITES? ON PPM AND PEAK?
M <- blank_filter(
  fold_change = 20,
  blank_label = "Blank",
  qc_label = "QC",
  factor_name = "sample_type",
  fraction_in_blank = 0
)

M <- model_apply(M, filtered_experiment)

test <- predicted(M)

test


C <- blank_filter_hist()
chart_plot(C, M)


C <- feature_profile(
  run_order = "order",
  qc_label = "QC",
  qc_column = "sample_type",
  colour_by = "sample_type",
  feature_to_plot = "L.histidine",
  plot_sd = FALSE
)

chart_plot(C, M, QRILC_imputed) + ylab("PPM") + ggtitle("Before")
chart_plot(C, test) + ylab("PPM") + ggtitle("After")
