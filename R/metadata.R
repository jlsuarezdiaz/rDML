dimensionality_reduction_dmls <-
  c("PCA",
    "LDA",
    "ANMM")

knn_oriented_dmls <-
  c("LMNN",
    "NCA")

ncm_oriented_dmls <-
  c("NCMML",
    "NCMC")

information_theory_based_dmls <-
  c("ITML",
    "DMLMJ",
    "MCML")

generic_dmls <-
  c("LSI",
    "DML_eig",
    "LDML")

kernel_dmls <-
  c("KLMNN",
    "KANMM",
    "KDMLMJ",
    "KDA")

base_dmls <-
  c("Metric",
    "Transformer",
    "Euclidean")

all_dmls <-
  c(dimensionality_reduction_dmls,knn_oriented_dmls,ncm_oriented_dmls, information_theory_based_dmls, generic_dmls,kernel_dmls,base_dmls)

non_supervised_dmls <-
  c("PCA")

supervised_dmls <-
  c("LDA",
    "ANMM",
    "LMNN",
    "NCA",
    "NCMML",
    "NCMC",
    "ITML",
    "DMLMJ",
    "MCML",
    "LSI",
    "DML_eig",
    "LDML",
    "KLMNN",
    "KANMM",
    "KDMLMJ",
    "KDA")

classifiers <-
  c("kNN",
    "NCMC_Classifier",
    "MultiDML_kNN")

tuners <-
  c(#"cross_validate",
    "tune_knn",
    #"metadata_cross_validate",
    "tune")

plotters <-
  c("classifier_plot",
    "dml_plot",
    "knn_plot",
    "dml_multiplot",
    "classifier_pairplots",
    "dml_pairplots",
    "knn_pairplots",
    "classifier_plot_3d")


