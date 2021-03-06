% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dml.R
\name{NCA}
\alias{NCA}
\title{Neighborhood Component Analysis (NCA).}
\usage{
NCA(num_dims = NULL, learning_rate = "adaptive", eta0 = 0.3,
  initial_transform = NULL, max_iter = 100, prec = 1e-08,
  tol = 1e-08, descent_method = "SGD", eta_thres = 1e-14,
  learn_inc = 1.01, learn_dec = 0.5)
}
\arguments{
\item{num_dims}{Desired value for dimensionality reduction. If None, the dimension of transformed data will be the same as the original. Integer.}

\item{learning_rate}{Type of learning rate update for gradient descent. Possible values are:
- 'adaptive' : the learning rate will increase if the gradient step is succesful, else it will decrease.
- 'constant' : the learning rate will be constant during all the gradient steps.}

\item{eta0}{The initial value for learning rate.}

\item{initial_transform}{If array or matrix that will represent the starting linear map for gradient descent, where d is the number of features,
and d' is the dimension specified in num_dims.
If None, euclidean distance will be used. If a string, the following values are allowed:
- 'euclidean' : the euclidean distance.
- 'scale' : a diagonal matrix that normalizes each attribute according to its range will be used.}

\item{max_iter}{Maximum number of iterations of gradient descent. Integer.}

\item{prec}{Precision stop criterion (gradient norm). Float.}

\item{tol}{Tolerance stop criterion (difference between two iterations). Float.}

\item{descent_method}{The descent method to use. Allowed values are:
- 'SGD' : stochastic gradient descent.
- 'BGD' : batch gradient descent.}

\item{eta_thres}{A learning rate threshold stop criterion. Float.}

\item{learn_inc}{Increase factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.}

\item{learn_dec}{Decrease factor for learning rate. Ignored if learning_rate is not 'adaptive'. Float.}
}
\value{
The NCA transformer, structured as a named list.
}
\description{
A distance metric learning algorithm that tries to minimize kNN expected error.
}
\references{
Jacob Goldberger et al. “Neighbourhood components analysis”. In: Advances in neural
            information processing systems. 2005, pages 513-520.
}
