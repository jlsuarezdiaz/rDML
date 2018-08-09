# TUNE ALGORITHMS WRAPPER

source("R/filter.R")
source("R/utils.R")

# A tune function for a distance metric learning algorithm, using k-NN score as metric.
#
# @param The string with the name of the DML Algorithm to tune. Select it from the items in the list `dml`.
# @param X array-like (N x d), training vector, where N is the number of samples, and d is the number of features.
# @param y array-like (N), labels vector, where N is the number of samples.
# @param n_neighbors Number of neighbors for k-NN.
# @param dml_params Additional keyword parameters for the distance metric learning algorithm, specified in a named list.
# @param tune_args A named list woth the parameters of the DML algorithm to tune. Each key has to be a keyword argument of the DML.
#                  The associated values have to be lists containing all the desired values for the tuning parameters.
# @param n_folds Number of folds for cross validation.
# @param n_reps Number of cross validations to do.
# @param verbose Boolean. If True, a console log will be printed.
# @param seed Integer. The seed used by the random number generator
# @param ... Additional keyword arguments for k-NN.
tune_knn <- function(dml, X, y, n_neighbors, dml_params, tune_args, n_folds = 5, n_reps = 1, verbose=FALSE, seed=NULL, ...){
  return(pre_tuners_$tune_knn(dml = pydml[[dml]], X = X, y = y, n_neighbors = as.integer(n_neighbors), dml_params = dml_params, tune_args = tune_args,
                              n_folds = as.integer(n_folds), n_reps = as.integer(n_reps), verbose = verbose, seed = as.integer_or_null(seed), ... = ...))
}

# Tune function for a distance metric learning algorithm, allowing as metrics the algorithm metadata,
# times and k-NN scores.
#
# @param The string with the name of the DML Algorithm to tune. Select it from the items in the list `dml`.
# @param X array-like (N x d), training vector, where N is the number of samples, and d is the number of features.
# @param y array-like (N), labels vector, where N is the number of samples.
# @param dml_params Additional keyword parameters for the distance metric learning algorithm, specified in a named list.
# @param tune_args A named list woth the parameters of the DML algorithm to tune. Each key has to be a keyword argument of the DML.
#                  The associated values have to be lists containing all the desired values for the tuning parameters.
# @param The metrics to evaluate. If string, it must be a key of the metadata() function of the DML Algorithm,
#        or 'time'. In this last case, the elapsed fitting time will be returned as a metric.
#        If int, the metric will be the k-NN score, where k is the specified int.
# @param n_folds Number of folds for cross validation.
# @param n_reps Number of cross validations to do.
# @param verbose Boolean. If True, a console log will be printed.
# @param seed Integer. The seed used by the random number generator
# @param ... Additional keyword arguments for k-NN.
tune <- function(dml, X, y, dml_params, tune_args, metrics, n_folds = 5, n_reps = 1, verbose=FALSE, seed=NULL, ...){
  metrics = numeric.as.integer(metrics)
  return(pre_tuners_$tune_knn(dml = pydml[[dml]], X = X, y = y, dml_params = dml_params, tune_args = tune_args, metrics = metrics,
                              n_folds = as.integer(n_folds), n_reps = as.integer(n_reps), verbose = verbose,
                              seed = as.integer_or_null(seed), ... = ...))
}

# The tune algorithms global list. This list contains all the tune functions.
dml_tune = list()
for(key in names(pre_tuners_)){
  console.log(paste("Adding R-Tune ",toString(key),"..."))
  dml_tune[[key]] = get(key)
}
