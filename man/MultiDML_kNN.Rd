% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distance_clf.R
\name{MultiDML_kNN}
\alias{MultiDML_kNN}
\title{Multi-DML k-NN}
\usage{
MultiDML_kNN(n_neighbors, dmls = NULL, verbose = FALSE, ...)
}
\arguments{
\item{n_neighbors}{The number of neighbors for k-NN. Integer.}

\item{dmls}{A list of distance metric learning algorithms to be learned for k-NN. By default, euclidean distance will be added at the first
place of the dml list.}

\item{verbose}{Boolean. If True, console message about the algorithms execution will be printed.}

\item{...}{Additional arguments for Scikit-Learn k-Neighbors Classifier.}
}
\value{
The MultiDML_kNN classifier, structured as a named list.
}
\description{
An interface that allows learning k-NN with different distance metric learners simultaneously.
}
