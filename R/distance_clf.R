# DISTANCE BASED CLASSIFIERS WRAPPER

source("R/filter.R")
source("R/utils.R")

#' R classifiers initializer.
#'
#' Initializes a classifier as a list from a pyDML classifier.
#'
#' @param clf The classifier object imported form pyDML.
#' @return a list with the classifier methods adapted to R.
#' @export
rinitialize_clf <- function(clf){
  rclf = list()
  for(key in names(clf)){
    rclf[[key]] = clf[[key]]
  }
  # Update any dml function here if a functionality update is needed respect to pyDML.
  # Example: rclf$fit = function(X,y){ ... new code for fit in R ... }

  return(rclf)
}

#' k-Nearest Neighbors (kNN).
#'
#' The nearest neighbors classifier adapted to be used with distance metric learning algorithms.
#'
#' @param n_neighbors Number of neighbors to consider in classification.
#' @param dml_algorithm The distance metric learning algorithm to use.
#' @return The kNN classifier, structured as a named list.
#' @export
kNN <- function(n_neighbors, dml_algorithm){
  knn = pre_classifiers_$kNN(n_neighbors = as.integer(n_neighbors), dml_algorithm = dml_algorithm$py)
  rinitialize_clf(knn)
}

#' Nearest Class with Multiple Centroids classifier.
#'
#' A classifier that makes its predictions by choosing the class who has a centroid the nearest to the point.
#' For each class, an arbitrary number of centroids can be set. This centroids are calculated using k-Means
#' over each class sub-dataset.
#'
#' @param centroids_num If it is a list, it must have the same size as the number of classes. In this case, i-th item will be the number of
#'        centroids to take in the i-th class. If it is an int, every class will have the same number of centroids.
#' @param ... Additional arguments for Scikit-Learn K-Means.
#' @return The NCMC classifier, structured as a named list.
#' @export
NCMC_Classifier <- function(centroids_num = 3, ...){
  ncmc_classifier = pre_classifiers_$NCMC_Classifier(centroids_num = as.integer(centroids_num), ...)
  rinitialize_clf(ncmc_classifier)
}

#' Multi-DML k-NN
#'
#' An interface that allows learning k-NN with different distance metric learners simultaneously.
#'
#' @param n_neighbors The number of neighbors for k-NN. Integer.
#' @param dmls A list of distance metric learning algorithms to be learned for k-NN. By default, euclidean distance will be added at the first
#'        place of the dml list.
#' @param verbose Boolean. If True, console message about the algorithms execution will be printed.
#' @param ... Additional arguments for Scikit-Learn k-Neighbors Classifier.
#' @return The MultiDML_kNN classifier, structured as a named list.
#' @export
MultiDML_kNN <- function(n_neighbors, dmls=NULL, verbose=FALSE, ...){
  dmls = sapply(dmls, function(x){ return(x$py)})
  mknn = pre_classifiers_$MultiDML_kNN(n_neighbors = as.integer(n_neighbors), dmls = dmls, verbose = verbose, ... = ...)
  rinitialize_clf(mknn)
}

#' The distance classifiers global list.
#'
#' This list contains all the distance classifier constructors.
distance_clf = list()
for(key in names(pre_classifiers_)){
  console.log(paste("Adding R-Distance Classifier ",toString(key),"..."))
  distance_clf[[key]] = get(key)
}
