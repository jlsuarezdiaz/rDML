# DMLS WRAPPER

#source("R/filter.R")
#source("R/utils.R")

#' R DML objects initializer.
#'
#' Initializes a DML as a list from the pyDML DML_Algorithm object.
#'
#' @param dml_alg The DML_Algorithm object imported form pyDML.
#' @return a list with the DML_Algorithm methods adapted to R.
#' @export
rinitialize <- function(dml_alg){
  rdml = list()
  for(key in names(dml_alg)){
    rdml[[key]] = dml_alg[[key]]
  }
  # Update any dml function here if a functionality update is needed respect to pyDML.
  # Example: rdml$fit = function(X,y){ ... new code for fit in R ... }
  rdml$py = dml_alg

  return(rdml)
}

#' Principal Component Analysis (PCA).
#'
#' A distance metric learning algorithm for unsupervised dimensionality reduction, obtaining orthogonal directions that maximize the variance.
#'
#' @param num_dims Number of components for dimensionality reduction. If NULL, all the principal components will be taken. Ignored if thres is provided. Integer.
#' @param Fraction of variability to keep, from 0 to 1. Data dimension will be reduced until the lowest dimension that keeps 'thres' explained variance. Float.
#' @return The PCA transformer, structured as a named list.
#' @export
PCA <- function(num_dims=NULL, thres=NULL){
  pca = pre_dml_$PCA(num_dims=as.integer_or_null(num_dims), thres=as.numeric_or_null(thres))
  return(rinitialize(pca))
}

#' Linear Discriminant Analysis (LDA).
#'
#' A distance metric learning algorithm for supervised dimensionality reduction, maximizing the ratio of variances between classes and within classes.
#'
#' @param num_dims Number of components (< n_classes - 1) for dimensionality reduction. If None, it will be taken as n_classes - 1. Ignored if thres is provided. Integer.
#' @param Fraction of variability to keep, from 0 to 1. Data dimension will be reduced until the lowest dimension that keeps 'thres' explained variance. Float.
#' @return The LDA transformer, structured as a named list.
#' @export
LDA <- function(num_dims=NULL, thres=NULL){
  lda = pre_dml_$LDA(num_dims=as.integer_or_null(num_dims), thres=as.numeric_or_null(thres))
  return(rinitialize(lda))
}

#' Average Neighborhood Margin Maximization (ANMM).
#'
#' A DML Algorithm that obtains a transformer that maximizes the distance between the nearest friends and the nearest enemies for each example.
#'
#' @param num_dims Dimension desired for the transformed data. Integer. If NULL, all features will be taken. Integer.
#' @param n_friends Number of nearest same-class neighbors to compute homogeneus neighborhood. Integer.
#' @param n_enemies Number of nearest different-class neighbors to compute heterogeneus neigborhood. Integer.
#' @return The ANMM transformer, structured as a named list.
#' @references Fei Wang and Changshui Zhang. “Feature extraction by maximizing the average neighborhood
#'             margin”. In: Computer Vision and Pattern Recognition, 2007. CVPR’07. IEEE Conference on.
#'             IEEE. 2007, pages 1-8.
#' @export
ANMM <- function(num_dims=NULL, n_friends=3, n_enemies=1){
  anmm = pre_dml_$ANMM(num_dims = as.integer_or_null(num_dims),n_friends = as.integer(n_friends),n_enemies = as.integer(n_enemies))
  return(rinitialize(anmm))
}

#' Large Margin Nearest Neighbors (LMNN).
#'
#' A distance metric learning algorithm that obtains a metric with target neighbors as near as possible and impostors as far as possible.
#'
#' @param num_dims Desired value for dimensionality reduction. Ignored if solver is 'SDP'. If NULL, all features will be kept. Integer.
#' @param learning_rate Type of learning rate update for gradient descent. Possible values are:
#'        - 'adaptive' : the learning rate will increase if the gradient step is succesful, else it will decrease.
#'        - 'constant' : the learning rate will be constant during all the gradient steps.
#' @param eta0 The initial value for learning rate.
#' @param initial_metric If array or matrix, and solver is SDP, it must be a positive semidefinite matrix with the starting metric (d x d) for gradient descent, where d is the number of features.
#'        If None, euclidean distance will be used. If a string, the following values are allowed:
#'        - 'euclidean' : the euclidean distance.
#'        - 'scale' : a diagonal matrix that normalizes each attribute according to its range will be used.
#'        If solver is 'SGD', then the array or matrix will represent a linear map (d' x d), where d' is the dimension provided in num_dims.
#' @param max_iter Maximum number of iterations of gradient descent. Integer.
#' @param prec Precision stop criterion (gradient norm). Float.
#' @param tol Tolerance stop criterion (difference between two iterations). Float.
#' @param k Number of target neighbors to take. If this algorithm is used for nearest neighbors classification, a good choice is
#'        to take k as the number of neighbors. Integer.
#' @param mu The weight of the push error in the minimization algorithm. The objective function is composed of a push error, given by the impostors,
#'        with weight mu, and a pull error, given by the target neighbors, with weight (1-mu). It must be between 0.0 and 1.0.
#' @param soft_comp_interval Intervals of soft computation. The soft computation relaxes the gradient descent conditions, but makes the algorithm more efficient.
#'        This value provides the length of a soft computation interval. After soft_comp_interval iterations of gradient descent, a complete
#'        gradient step is performed. Integer.
#' @param learn_inc Increase factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.
#' @param learn_dec Decrease factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.
#' @param eta_thres A learning rate threshold stop criterion. Float.
#' @param solver The algorithm used for minimization. Allowed values are:
#'        - 'SDP' : semidefinite programming, consisting of gradient descent with projections onto the positive semidefinite cone. It learns a metric.
#'        - 'SGD' : stochastic gradient descent. It learns a linear transformer.
#' @return The LMNN transformer, structured as a named list.
#' @references Kilian Q Weinberger and Lawrence K Saul. “Distance metric learning for large margin nearest
#'             neighbor classification”. In: Journal of Machine Learning Research 10.Feb (2009), pages 207-244.
#' @export
LMNN <- function(num_dims=NULL, learning_rate="adaptive", eta0=0.3, initial_metric=NULL, max_iter=100,prec=1e-8,
                 tol=1e-8, k=3, mu=0.5, soft_comp_interval=1, learn_inc=1.01, learn_dec=0.5, eta_thres=1e-14, solver="SDP"){

  lmnn = pre_dml_$LMNN(num_dims = as.integer_or_null(num_dims), learning_rate = learning_rate, eta0 = as.numeric(eta0), initial_metric = initial_metric,
                       max_iter = as.integer(max_iter), prec = as.numeric(prec), tol = as.numeric(tol), k = as.integer(k), mu = as.numeric(mu),
                       soft_comp_interval = as.integer(soft_comp_interval), learn_inc = as.numeric(learn_inc), learn_dec = as.numeric(learn_dec),
                       eta_thres = as.numeric(eta_thres), solver = solver)
  return(rinitialize(lmnn))
}

#' Neighborhood Component Analysis (NCA).
#'
#' A distance metric learning algorithm that tries to minimize kNN expected error.
#'
#' @param num_dims Desired value for dimensionality reduction. If None, the dimension of transformed data will be the same as the original. Integer.
#' @param learning_rate Type of learning rate update for gradient descent. Possible values are:
#'        - 'adaptive' : the learning rate will increase if the gradient step is succesful, else it will decrease.
#'        - 'constant' : the learning rate will be constant during all the gradient steps.
#' @param eta0 The initial value for learning rate.
#' @param initial_transform If array or matrix that will represent the starting linear map for gradient descent, where d is the number of features,
#'        and d' is the dimension specified in num_dims.
#'        If None, euclidean distance will be used. If a string, the following values are allowed:
#'        - 'euclidean' : the euclidean distance.
#'        - 'scale' : a diagonal matrix that normalizes each attribute according to its range will be used.
#' @param max_iter Maximum number of iterations of gradient descent. Integer.
#' @param prec Precision stop criterion (gradient norm). Float.
#' @param tol Tolerance stop criterion (difference between two iterations). Float.
#' @param descent_method The descent method to use. Allowed values are:
#'        - 'SGD' : stochastic gradient descent.
#'        - 'BGD' : batch gradient descent.
#' @param learn_inc Increase factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.
#' @param learn_dec Decrease factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.
#' @param eta_thres A learning rate threshold stop criterion. Float.
#' @return The NCA transformer, structured as a named list.
#' @references Jacob Goldberger et al. “Neighbourhood components analysis”. In: Advances in neural
#'             information processing systems. 2005, pages 513-520.
#' @export
NCA <- function(num_dims=NULL, learning_rate="adaptive", eta0=0.3, initial_transform=NULL, max_iter=100, prec=1e-8,
                tol=1e-8, descent_method="SGD", eta_thres=1e-14, learn_inc=1.01, learn_dec=0.5){
  nca = pre_dml_$NCA(num_dims = as.integer_or_null(num_dims), learning_rate = learning_rate, eta0 = as.numeric(eta0), initial_transform = initial_transform,
                     max_iter = as.integer(max_iter), prec = as.numeric(prec), tol = as.numeric(tol), descent_method = descent_method,
                     eta_thres = as.numeric(eta_thres), learn_inc = as.numeric(learn_inc), learn_dec = as.numeric(learn_dec))
  return(rinitialize(nca))
}

#' Nearest Class Mean Metric Learning (NCMML).
#'
#' A distance metric learning algorithm to improve the nearest class mean classifier.
#'
#' @param num_dims Desired value for dimensionality reduction. If None, the dimension of transformed data will be the same as the original. Integer.
#' @param learning_rate Type of learning rate update for gradient descent. Possible values are:
#'        - 'adaptive' : the learning rate will increase if the gradient step is succesful, else it will decrease.
#'        - 'constant' : the learning rate will be constant during all the gradient steps.
#' @param eta0 The initial value for learning rate.
#' @param initial_transform If array or matrix that will represent the starting linear map for gradient descent, where d is the number of features,
#'        and d' is the dimension specified in num_dims.
#'        If None, euclidean distance will be used. If a string, the following values are allowed:
#'        - 'euclidean' : the euclidean distance.
#'        - 'scale' : a diagonal matrix that normalizes each attribute according to its range will be used.
#' @param max_iter Maximum number of iterations of gradient descent. Integer.
#' @param prec Precision stop criterion (gradient norm). Float.
#' @param tol Tolerance stop criterion (difference between two iterations). Float.
#' @param descent_method The descent method to use. Allowed values are:
#'        - 'SGD' : stochastic gradient descent.
#'        - 'BGD' : batch gradient descent.
#' @param learn_inc Increase factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.
#' @param learn_dec Decrease factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.
#' @param eta_thres A learning rate threshold stop criterion. Float.
#' @return The NCMML transformer, structured as a named list.
#' @references Thomas Mensink et al. “Metric learning for large scale image classification: Generalizing to new
#'             classes at near-zero cost”. In: Computer Vision–ECCV 2012. Springer, 2012, pages 488-501.
#' @export
NCMML <- function(num_dims=NULL, learning_rate="adaptive", eta0=0.3, initial_transform=NULL, max_iter=100,
                  tol=1e-15, prec=1e-15, descent_method="SGD", eta_thres=1e-14, learn_inc=1.01,learn_dec=0.5){
  ncmml = pre_dml_$NCMML(num_dims = as.integer_or_null(num_dims), learning_rate = learning_rate, eta0= as.numeric(eta0), initial_transform = initial_transform,
                         max_iter = as.integer(max_iter), tol = as.numeric(tol), prec = as.numeric(prec), descent_method = descent_method,
                         eta_thres = as.numeric(eta_thres), learn_inc = as.numeric(learn_inc), learn_dec = as.numeric(learn_dec))
  return(rinitialize(ncmml))
}

#' Nearest Class with Multiple Centroids distance metric learner (NCMC).
#'
#' A distance metric learning algorithm to improve the nearest class with multiple centroids classifier.
#'
#' @param num_dims Desired value for dimensionality reduction. If None, the dimension of transformed data will be the same as the original. Integer.
#' @param centroids_num If it is a list, it must have the same size as the number of classes. In this case, i-th item will be the number of
#'        centroids to take in the i-th class. If it is an int, every class will have the same number of centroids.
#' @param learning_rate Type of learning rate update for gradient descent. Possible values are:
#'        - 'adaptive' : the learning rate will increase if the gradient step is succesful, else it will decrease.
#'        - 'constant' : the learning rate will be constant during all the gradient steps.
#' @param eta0 The initial value for learning rate.
#' @param initial_transform If array or matrix that will represent the starting linear map for gradient descent, where d is the number of features,
#'        and d' is the dimension specified in num_dims.
#'        If None, euclidean distance will be used. If a string, the following values are allowed:
#'        - 'euclidean' : the euclidean distance.
#'        - 'scale' : a diagonal matrix that normalizes each attribute according to its range will be used.
#' @param max_iter Maximum number of iterations of gradient descent. Integer.
#' @param prec Precision stop criterion (gradient norm). Float.
#' @param tol Tolerance stop criterion (difference between two iterations). Float.
#' @param descent_method The descent method to use. Allowed values are:
#'        - 'SGD' : stochastic gradient descent.
#'        - 'BGD' : batch gradient descent.
#' @param learn_inc Increase factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.
#' @param learn_dec Decrease factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.
#' @param eta_thres A learning rate threshold stop criterion. Float.
#' @param ... Aditional argument for Scikit-Learn K-Means.
#' @return The NCMC transformer, structured as a named list.
#' @references Thomas Mensink et al. “Metric learning for large scale image classification: Generalizing to new
#'             classes at near-zero cost”. In: Computer Vision–ECCV 2012. Springer, 2012, pages 488-501.
#' @export
NCMC <- function(num_dims=NULL, centroids_num=3, learning_rate="adaptive", eta0=0.3, initial_transform=NULL, max_iter=300,
                 tol=1e-15, prec=1e-15, descent_method="SGD", eta_thres=1e-14, learn_inc=1.01,learn_dec=0.5,...){
  ncmc = pre_dml_$NCMC(num_dims = as.integer_or_null(num_dims), centroids_num = as.integer(centroids_num), learning_rate = learning_rate, eta0 = as.numeric(eta0),
                       initial_transform = initial_transform, max_iter = as.integer(max_iter), tol = as.numeric(tol), prec = as.numeric(prec),
                       descent_method = descent_method, eta_thres=as.numeric(eta_thres), learn_inc = as.numeric(learn_inc), learn_dec = as.numeric(learn_dec),
                       ... = ...)
  return(rinitialize(ncmc))
}

#' Information Theoretic Metric Learning (ITML).
#'
#' A DML algorithm that learns a metric associated to the nearest gaussian distribution satisfying similarity constraints.
#' The nearest gaussian distribution is obtained minimizing the Kullback-Leibler divergence.
#'
#' @param initial_metric A positive definite matrix that defines the initial metric used to compare.
#' @param upper_bound Bound for dissimilarity constraints. If NULL, it will be estimated from upper_perc. Float.
#' @param lower_bound Bound for similarity constraints. If NULL, it will be estimated from lower_perc. Float.
#' @param num_constraints Number of constraints to generate. If None, it will be taken as 40 * k * (k-1), where k is the number of classes. Integer.
#' @param gamma The gamma value for slack variables. Float.
#' @param tol Tolerance stop criterion for the algorithm. Float.
#' @param max_iter Maximum number of iterations for the algorithm. Integer.
#' @param low_perc Lower percentile (from 0 to 100) to estimate the lower bound from the dataset. Ignored if lower_bound is provided. Integer.
#' @param up_perc Upper percentile (from 0 to 100) to estimate the upper bound from the dataset. Ignored if upper_bound is provided. Integer.
#' @return The ITML transformer, structured as a named list.
#' @references Jason V Davis et al. “Information-theoretic metric learning”. In: Proceedings of the 24th
#'             international conference on Machine learning. ACM. 2007, pages. 209-216.
#' @export
ITML <- function(initial_metric=NULL, upper_bound=NULL, lower_bound=NULL, num_constraints=NULL, gamma=1.0, tol = 0.001, max_iter = 100000,
                 low_perc = 5, up_perc = 95){
  itml = pre_dml_$ITML(initial_metric = initial_metric, upper_bound = as.numeric_or_null(upper_bound), lower_bound = as.numeric_or_null(lower_bound),
                       num_constraints = as.integer_or_null(num_constraints), gamma = as.numeric_or_null(gamma), tol = as.numeric_or_null(tol),
                       low_perc = as.integer(low_perc), up_perc = as.integer(up_perc))
  return(rinitialize(itml))
}

#' Distance Metric Learning through the Maximization of the Jeffrey divergence (DMLMJ).
#'
#' A DML Algorithm that obtains a transformer that maximizes the Jeffrey divergence between
#' the distribution of differences of same-class neighbors and the distribution of differences between
#' different-class neighbors.
#'
#' @param num_dims Dimension desired for the transformed data. If NULL, dimension will be the number of features.
#' @param n_neighbors Number of neighbors to consider in the computation of the difference spaces.
#' @param alpha Regularization parameter for inverse matrix computation.
#' @param reg_tol Tolerance threshold for applying regularization. The tolerance is compared with the matrix determinant.
#' @return The DMLMJ transformer, structured as a named list.
#' @references Bac Nguyen, Carlos Morell and Bernard De Baets. “Supervised distance metric learning through
#'             maximization of the Jeffrey divergence”. In: Pattern Recognition 64 (2017), pages 215-225.
#' @export
DMLMJ <- function(num_dims=NULL, n_neighbors=3, alpha=0.001, reg_tol=1e-10){
  dmlmj = pre_dml_$DMLMJ(num_dims = as.integer_or_null(num_dims), n_neighbors = as.integer(n_neighbors), alpha = as.numeric(alpha),
                         reg_tol = as.numeric(reg_tol))
  return(rinitialize(dmlmj))
}

#' Maximally Collapsing Metric Learning (MCML).
#'
#' A distance metric learning algorithm that learns minimizing the KL divergence to the maximally collapsing distribution.
#'
#' @param num_dims Number of dimensions for dimensionality reduction. Not supported yet.
#' @param learning_rate Type of learning rate update for gradient descent. Possible values are:
#'        - 'adaptive' : the learning rate will increase if the gradient step is succesful, else it will decrease.
#'        - 'constant' : the learning rate will be constant during all the gradient steps.
#' @param eta0 The initial value for learning rate. Float.
#' @param initial_metric If array or matrix, it must be a positive semidefinite matrix with the starting metric for gradient descent, where d is the number of features.
#'        If None, euclidean distance will be used. If a string, the following values are allowed:
#'        - 'euclidean' : the euclidean distance.
#'        - 'scale' : a diagonal matrix that normalizes each attribute according to its range will be used.
#' @param max_iter Maximum number of iterations of gradient descent. Integer.
#' @param prec Precision stop criterion (gradient norm). Float.
#' @param tol Tolerance stop criterion (difference between two iterations). Float.
#' @param descent_method The descent method to use. Allowed values are:
#'        - 'SDP' : semidefinite programming, consisting of gradient descent with projections onto the PSD cone.
#' @param eta_thres A learning rate threshold stop criterion.
#' @param learn_inc Increase factor for learning rate. Ignored if learning_rate is not 'adaptive'.
#' @param learn_dec Decrease factor for learning rate. Ignored if learning_rate is not 'adaptive'.
#' @return The MCML transformer, structured as a named list.
#' @references Amir Globerson and Sam T Roweis. “Metric learning by collapsing classes”. In: Advances in neural
#'             information processing systems. 2006, pages 451-458.
#' @export
MCML <- function(num_dims=NULL, learning_rate="adaptive", eta0=0.01, initial_metric=NULL, max_iter=20, prec=0.01,
                 tol=0.01, descent_method="SDP", eta_thres=1e-14, learn_inc=1.01, learn_dec=0.5){
  mcml = pre_dml_$MCML(num_dims = as.integer_or_null(num_dims), learning_rate = learning_rate, eta0 = as.numeric(eta0), initial_metric = initial_metric,
                       max_iter = as.integer(max_iter), prec = as.numeric(prec), tol = as.numeric(tol), descent_method = descent_method,
                       eta_thres = as.numeric(eta_thres), learn_inc = as.numeric(learn_inc), learn_dec = as.numeric(learn_dec))
  return(rinitialize(mcml))
}

#' Learning with Side Information (LSI).
#'
#' A distance metric learning algorithm that minimizes the sum of distances between similar data, with non similar
#' data constrained to be separated.
#'
#' @param initial_metric If array or matrix, it must be a positive semidefinite matrix with the starting metric for gradient descent, where d is the number of features.
#' @param If None, euclidean distance will be used. If a string, the following values are allowed:
#'        - 'euclidean' : the euclidean distance.
#'        - 'scale' : a diagonal matrix that normalizes each attribute according to its range will be used.
#' @param learning_rate Type of learning rate update for gradient descent. Possible values are:
#'        - 'adaptive' : the learning rate will increase if the gradient step is succesful, else it will decrease.
#'        - 'constant' : the learning rate will be constant during all the gradient steps.
#' @param eta0 The initial value for learning rate. Float.
#' @param max_iter Number of iterations for gradient descent. Integer.
#' @param max_proj_iter Number of iterations for iterated projections. Integer.
#' @param itproj_err Convergence error criterion for iterated projections. Float.
#' @param err Convergence error stop criterion for gradient descent.
#' @param supervised If True, the algorithm will accept a labeled dataset (X,y). Else, it will accept the dataset and the similarity sets, (X,S,D).
#' @return The LSI transformer, structured as a named list.
#' @references Eric P Xing et al. “Distance metric learning with application to clustering with side-information”.
#'             In: Advances in neural information processing systems. 2003, pages 521-528.
#' @export
LSI <- function(initial_metric=NULL, learning_rate="adaptive",eta0=0.1, max_iter=100, max_proj_iter=5000, itproj_err=1e-3, err=1e-3,supervised=FALSE){
  lsi = pre_dml_$LSI(initial_metric = initial_metric, learning_rate = learning_rate, eta0 = as.numeric(eta0), max_iter = as.integer(max_iter),
                     max_proj_iter = as.integer(max_proj_iter), itproj_err = as.numeric(itproj_err), err = as.numeric(err), supervised = supervised)
  return(rinitialize(lsi))
}

#' Distance Metric Learning with Eigenvalue Optimization (DML-eig).
#'
#' A DML Algorithm that learns a metric that minimizes the minimum distance between different-class points
#' constrained to the sum of distances at same-class points be non higher than a constant.
#'
#' @param mu Smoothing parameter. Float.
#' @param tol Tolerance stop criterion (difference between two point iterations at gradient descent). Float.
#' @param eps Precision stop criterion (norm of gradient at gradient descent). Float.
#' @param max_it Number of iterations at gradient descent. Integer.
#' @return The LSI transformer, structured as a named list.
#' @references Yiming Ying and Peng Li. “Distance metric learning with eigenvalue optimization”. In: Journal of
#'             Machine Learning Research 13.Jan (2012), pages 1-26.
#' @export
DML_eig <- function(mu=1e-4, tol=1e-5, eps=1e-10, max_it=25){
  dml_eig = pre_dml_$DML_eig(mu = as.numeric(mu), tol = as.numeric(tol), eps = as.numeric(eps), max_it = as.numeric(max_it))
  return(rinitialize(dml_eig))
}

#' Logistic Discriminant Metric Learning (LDML).
#'
#' Distance Metric Learning through the likelihood maximization of a logistic based probability distribution.
#'
#' @param num_dims Number of dimensions for dimensionality reduction. Not supported yet.
#' @param b Logistic function positive threshold.
#' @param learning_rate Type of learning rate update for gradient descent. Possible values are:
#'        - 'adaptive' : the learning rate will increase if the gradient step is succesful, else it will decrease.
#'        - 'constant' : the learning rate will be constant during all the gradient steps.
#' @param eta0 The initial value for learning rate. Float.
#' @param initial_metric If array or matrix, it must be a positive semidefinite matrix with the starting metric for gradient descent, where d is the number of features.
#'        If None, euclidean distance will be used. If a string, the following values are allowed:
#'        - 'euclidean' : the euclidean distance.
#'        - 'scale' : a diagonal matrix that normalizes each attribute according to its range will be used.
#' @param max_iter Maximum number of iterations of gradient descent. Integer.
#' @param prec Precision stop criterion (gradient norm). Float.
#' @param tol Tolerance stop criterion (difference between two iterations). Float.
#' @param descent_method The descent method to use. Allowed values are:
#'        - 'SDP' : semidefinite programming, consisting of gradient descent with projections onto the PSD cone.
#' @param eta_thres A learning rate threshold stop criterion.
#' @param learn_inc Increase factor for learning rate. Ignored if learning_rate is not 'adaptive'.
#' @param learn_dec Decrease factor for learning rate. Ignored if learning_rate is not 'adaptive'.
#' @return The MCML transformer, structured as a named list.
#' @references Matthieu Guillaumin, Jakob Verbeek and Cordelia Schmid. “Is that you? Metric learning approaches
#'             for face identification”. In: Computer Vision, 2009 IEEE 12th international conference on. IEEE.
#'             2009, pages 498-505.
#' @export
LDML <- function(num_dims = NULL, b=1e-3, learning_rate="adaptive", eta0=0.3, initial_metric=NULL, max_iter=10, prec=1e-3, tol=1e-3,
                 descent_method="SDP", eta_thres=1e-14, learn_inc=1.01, learn_dec=0.5){
  ldml = pre_dml_$LDML(num_dims = as.integer_or_null(num_dims), b = as.numeric(b), learning_rate = learning_rate, eta0 = as.numeric(eta0),
                       initial_metric = initial_metric, max_iter = as.integer(max_iter), prec = as.numeric(prec), tol = as.numeric(tol),
                       descent_method = descent_method, eta_thres = as.numeric(eta_thres), learn_inc = as.numeric(learn_inc),
                       learn_dec = as.numeric(learn_dec))
  return(rinitialize(ldml))
}

#' The kernelized version of LMNN.
#'
#' @param num_dims Desired value for dimensionality reduction. Ignored if solver is 'SDP'. If NULL, all features will be kept. Integer.
#' @param learning_rate Type of learning rate update for gradient descent. Possible values are:
#'        - 'adaptive' : the learning rate will increase if the gradient step is succesful, else it will decrease.
#'        - 'constant' : the learning rate will be constant during all the gradient steps.
#' @param eta0 The initial value for learning rate.
#' @param initial_metric If array or matrix, and solver is SDP, it must be a positive semidefinite matrix with the starting metric (d x d) for gradient descent, where d is the number of features.
#'        If None, euclidean distance will be used. If a string, the following values are allowed:
#'        - 'euclidean' : the euclidean distance.
#'        - 'scale' : a diagonal matrix that normalizes each attribute according to its range will be used.
#'        If solver is 'SGD', then the array or matrix will represent a linear map (d' x d), where d' is the dimension provided in num_dims.
#' @param max_iter Maximum number of iterations of gradient descent. Integer.
#' @param prec Precision stop criterion (gradient norm). Float.
#' @param tol Tolerance stop criterion (difference between two iterations). Float.
#' @param k Number of target neighbors to take. If this algorithm is used for nearest neighbors classification, a good choice is
#'        to take k as the number of neighbors. Integer.
#' @param mu The weight of the push error in the minimization algorithm. The objective function is composed of a push error, given by the impostors,
#'        with weight mu, and a pull error, given by the target neighbors, with weight (1-mu). It must be between 0.0 and 1.0.
#' @param learn_inc Increase factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.
#' @param learn_dec Decrease factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.
#' @param eta_thres A learning rate threshold stop criterion. Float.
#' @param target_selection How to find the target neighbors. Allowed values are:
#'        - 'kernel' : using the euclidean distance in the kernel space.
#'        - 'original' : using the euclidean distance in the original space.
#' @param kernel Kernel to use. Allowed values are: "linear" | "poly" | "rbf" | "sigmoid" | "cosine" | "precomputed".
#' @param gamma Kernel coefficient for rbf, poly and sigmoid kernels. Ignored by other
#'        kernels. Default value is 1/n_features. Float.
#' @param degree Degree for poly kernels. Ignored by other kernels. Integer.
#' @param coef0 Independent term for poly and sigmoid kernels. Ignored by other kernels. Float.
#' @param kernel_params Parameters (keyword arguments) and values for kernel passed as
#'        callable object. Ignored by other kernels.
#' @return The KLMNN transformer, structured as a named list.
#' @references Kilian Q Weinberger and Lawrence K Saul. “Distance metric learning for large margin nearest
#'             neighbor classification”. In: Journal of Machine Learning Research 10.Feb (2009), pages 207-244.
#' @references Lorenzo Torresani and Kuang-chih Lee. “Large margin component analysis”. In: Advances in neural
#'             information processing systems. 2007, pages 1385-1392.
#' @export
KLMNN <- function(num_dims = NULL, learning_rate = "adaptive", eta0=0.3, initial_metric=NULL, max_iter=100, prec=1e-8, tol=1e-8, k=3,
                  mu=0.5, learn_inc=1.01, learn_dec=0.5, eta_thres=1e-14, kernel="linear", gamma=NULL, degree=3, coef0=1, kernel_params=NULL,
                  target_selection="kernel"){
  klmnn = pre_dml_$KLMNN(num_dims = as.integer_or_null(num_dims), learning_rate = learning_rate, eta0 = eta0, initial_metric = initial_metric,
                         max_iter = as.integer(max_iter), prec = as.numeric(prec), tol = as.numeric(tol), k = as.integer(k), mu = as.numeric(mu),
                         learn_inc = as.numeric(learn_inc), learn_dec = as.numeric(learn_dec), eta_thres = as.numeric(eta_thres),
                         kernel = kernel, gamma = as.numeric_or_null(gamma), degree = as.integer(degree), coef0 = as.numeric(coef0),
                         kernel_params = kernel_params, target_selection = target_selection)
  return(rinitialize(klmnn))
}

#' The kernelized version of ANMM.
#'
#' @param num_dims Dimension desired for the transformed data. Integer. If NULL, all features will be taken. Integer.
#' @param n_friends Number of nearest same-class neighbors to compute homogeneus neighborhood. Integer.
#' @param n_enemies Number of nearest different-class neighbors to compute heterogeneus neigborhood. Integer.
#' @param kernel Kernel to use. Allowed values are: "linear" | "poly" | "rbf" | "sigmoid" | "cosine" | "precomputed".
#' @param gamma Kernel coefficient for rbf, poly and sigmoid kernels. Ignored by other
#'        kernels. Default value is 1/n_features. Float.
#' @param degree Degree for poly kernels. Ignored by other kernels. Integer.
#' @param coef0 Independent term for poly and sigmoid kernels. Ignored by other kernels. Float.
#' @param kernel_params Parameters (keyword arguments) and values for kernel passed as
#'        callable object. Ignored by other kernels.
#' @return The KANMM transformer, structured as a named list.
#' @references Fei Wang and Changshui Zhang. “Feature extraction by maximizing the average neighborhood
#'             margin”. In: Computer Vision and Pattern Recognition, 2007. CVPR’07. IEEE Conference on.
#'             IEEE. 2007, pages 1-8.
#' @export
KANMM <- function(num_dims = NULL, n_friends = 3, n_enemies = 1, kernel="linear", gamma=NULL, degree=3, coef0=1, kernel_params=NULL){
  kanmm = pre_dml_$KANMM(num_dims = as.integer_or_null(num_dims), n_friends = as.integer(n_friends), n_enemies = as.integer(n_enemies),
                         kernel = kernel, gamma = as.numeric_or_null(gamma), degree = as.integer(degree), coef0 = as.numeric(coef0),
                         kernel_params = kernel_params)
  return(rinitialize(kanmm))
}

#' The kernelized version of DMLMJ.
#'
#' @param num_dims Dimension desired for the transformed data. If NULL, dimension will be the number of features.
#' @param n_neighbors Number of neighbors to consider in the computation of the difference spaces.
#' @param alpha Regularization parameter for inverse matrix computation.
#' @param reg_tol Tolerance threshold for applying regularization. The tolerance is compared with the matrix determinant.
#' @param kernel Kernel to use. Allowed values are: "linear" | "poly" | "rbf" | "sigmoid" | "cosine" | "precomputed".
#' @param gamma Kernel coefficient for rbf, poly and sigmoid kernels. Ignored by other
#'        kernels. Default value is 1/n_features. Float.
#' @param degree Degree for poly kernels. Ignored by other kernels. Integer.
#' @param coef0 Independent term for poly and sigmoid kernels. Ignored by other kernels. Float.
#' @param kernel_params Parameters (keyword arguments) and values for kernel passed as
#'        callable object. Ignored by other kernels.
#' @return The KDMLMJ transformer, structured as a named list.
#' @references Bac Nguyen, Carlos Morell and Bernard De Baets. “Supervised distance metric learning through
#'             maximization of the Jeffrey divergence”. In: Pattern Recognition 64 (2017), pages 215-225.
#' @export
KDMLMJ <- function(num_dims=NULL, n_neighbors=3, alpha=0.001, reg_tol=1e-10, kernel="linear", gamma=NULL, degree=3, coef0=1, kernel_params=NULL){
  kdmlmj = pre_dml_$KDMLMJ(num_dims = as.integer_or_null(num_dims), n_neighbors = as.integer(n_neighbors), alpha = as.numeric(alpha),
                           reg_tol = as.numeric(reg_tol), kernel = kernel, gamma = as.numeric_or_null(gamma), degree = as.integer(degree),
                           coef0 = as.numeric(coef0), kernel_params = kernel_params)
  return(rinitialize(kdmlmj))
}

#' Kernel Discriminant Analysis (KDA).
#'
#' Discriminant analysis in high dimensionality using the kernel trick.
#'
#' @param solver Solver to use, posible values:
#'        - 'eigen': Eigenvalue decomposition.
#' @param n_components Number of components (lower than number of classes -1) for dimensionality reduction. If NULL, classes - 1 is used. Integer.
#' @param tol Singularity toleration level. Float.
#' @param kernel Kernel to use. Allowed values are: "linear" | "poly" | "rbf" | "sigmoid" | "cosine" | "precomputed".
#' @param gamma Kernel coefficient for rbf, poly and sigmoid kernels. Ignored by other
#'        kernels. Default value is 1/n_features. Float.
#' @param degree Degree for poly kernels. Ignored by other kernels. Integer.
#' @param coef0 Independent term for poly and sigmoid kernels. Ignored by other kernels. Float.
#' @param kernel_params Parameters (keyword arguments) and values for kernel passed as
#'        callable object. Ignored by other kernels.
#' @return The KDA transformer, structured as a named list.
#' @references Sebastian Mika et al. “Fisher discriminant analysis with kernels”. In: Neural networks for signal
#'             processing IX, 1999. Proceedings of the 1999 IEEE signal processing society workshop. Ieee. 1999,
#'             pages 41-48.
#' @export
KDA <- function(solver='eigen', n_components=NULL, tol=1e-4, kernel="linear", gamma=NULL, degree=3, coef0=1, kernel_params=NULL){
  kda = pre_dml_$KDA(solver = solver, n_components = as.integer_or_null(n_components), tol = as.numeric(tol), kernel = kernel,
                     gamma = as.numeric_or_null(gamma), degree = as.integer(degree), coef0 = as.numeric(coef0), kernel_params = kernel_params)
  return(rinitialize(kda))
}

#' Basic learning from a metric.
#'
#' A DML algorithm that defines a distance given a PSD metric matrix.
#'
#' @param metric (d x d) matrix. A positive semidefinite matrix, to define a pseudodistance in euclidean d-dimensional space.
#' @return @return The Metric transformer, structured as a named list.
#' @export
Metric <- function(metric){
  metric = pre_dml_$Metric(metric = metric)
  return(rinitialize(metric))
}

#' Basic learning from a transformer.
#'
#' A DML algorithm that defines a distance given a linear transformation.
#'
#' @param metric d' x d) matrix, representing a linear transformacion from d-dimensional euclidean space
#'        to d'-dimensional euclidean space.
#' @return The Transformer transformer, structured as a named list.
#' @export
Transformer <- function(transformer){
  transformer = pre_dml_$Transformer(transformer = transformer)
  return(rinitialize(transformer))
}

#' Euclidean distance.
#'
#' A basic transformer that represents the euclidean distance.
#'
#' @return The Euclidean (identity) transformer, structured as a named list.
#' @export
Euclidean <- function(){
  euclidean = pre_dml_$Euclidean()
  return(rinitialize(euclidean))
}

#' The DML global list.
#'
#' This list contains all the DML Algorithm constructors.
dml <- NULL

.init_dml <- function(){
  dml_ <- list()
  for(key in names(pre_dml_)){
    console.log(paste("Adding R-DML algorithm ",toString(key),"..."))
    dml_[[key]] = get(key)
  }
  dml <<- dml_
}
