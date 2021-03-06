% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tune.R
\name{tune_knn}
\alias{tune_knn}
\title{DML Tuning with k-NN.}
\usage{
tune_knn(dml, X, y, n_neighbors, dml_params, tune_args, n_folds = 5,
  n_reps = 1, verbose = FALSE, seed = NULL, ...)
}
\arguments{
\item{X}{array-like (N x d), training vector, where N is the number of samples, and d is the number of features.}

\item{y}{array-like (N), labels vector, where N is the number of samples.}

\item{n_neighbors}{Number of neighbors for k-NN.}

\item{dml_params}{Additional keyword parameters for the distance metric learning algorithm, specified in a named list.}

\item{tune_args}{A named list woth the parameters of the DML algorithm to tune. Each key has to be a keyword argument of the DML.
The associated values have to be lists containing all the desired values for the tuning parameters.}

\item{n_folds}{Number of folds for cross validation.}

\item{n_reps}{Number of cross validations to do.}

\item{verbose}{Boolean. If True, a console log will be printed.}

\item{seed}{Integer. The seed used by the random number generator}

\item{...}{Additional keyword arguments for k-NN.}

\item{The}{string with the name of the DML Algorithm to tune. Select it from the items in the list `dml`.}
}
\value{
A list with the following elements:
        - results: A dataframe whose entries are all the cases considered for the tune parameters, and with a single column
                   that shows the cross validation score for each case.
        - best_performance: A pair with the best case obtained, together with its corresponding score.
        - best_dml: The DML Algorithm object that obtained the best result in the tuning.
        - detailed_results: A named list whose keys are all the possible cases, and each entry is the cross validation table for the
          corresponding case, containing the scores for every fold.
}
\description{
A tune function for a distance metric learning algorithm, using k-NN score as metric.
}
