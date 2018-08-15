#' The Distance Metric Learning algorithms oriented to dimensionality reduction.
dimensionality_reduction_dmls <-
  c("PCA",
    "LDA",
    "ANMM")

#' The Distance Metric Learning algorithms oriented to k-NN classifier.
knn_oriented_dmls <-
  c("LMNN",
    "NCA")

#' The Distance Metric Learning algorithms oriented to nearest centroids classifiers.
ncm_oriented_dmls <-
  c("NCMML",
    "NCMC")

#' The Distance Metric Learning algorithms based on information theory.
information_theory_based_dmls <-
  c("ITML",
    "DMLMJ",
    "MCML")

#' Generic Distance Metric Learning algorithms.
generic_dmls <-
  c("LSI",
    "DML_eig",
    "LDML")

#' Kernel Distance Metric Learning algorithms.
kernel_dmls <-
  c("KLMNN",
    "KANMM",
    "KDMLMJ",
    "KDA")

#' Base Distance Metric Learning algorithms.
base_dmls <-
  c("Metric",
    "Transformer",
    "Euclidean")

#' The complete Distance Metric Learning algorithms list.
all_dmls <-
  c(dimensionality_reduction_dmls,knn_oriented_dmls,ncm_oriented_dmls, information_theory_based_dmls, generic_dmls,kernel_dmls,base_dmls)

#' Non supervised Distance Metric Learning algorithms.
non_supervised_dmls <-
  c("PCA")

#' Supervised Distance Metric Learning algorithms.
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

#' Distance based classifiers.
classifiers <-
  c("kNN",
    "NCMC_Classifier",
    "MultiDML_kNN")

#' Distance Metric Learning tune algorithms.
tuners <-
  c(#"cross_validate",
    "tune_knn",
    #"metadata_cross_validate",
    "tune")

#' Plot functions for Distance Metric Learning.
plotters <-
  c(#"classifier_plot",
    #"dml_plot",
    "knn_plot",
    "dml_multiplot",
    #"classifier_pairplots",
    #"dml_pairplots",
    "knn_pairplots"
    #"classifier_plot_3d"
    )


