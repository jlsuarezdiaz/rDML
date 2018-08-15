library(datasets)
library(rDML)

# Loading dataset
data(iris)
X = iris[1:4]
y = iris[5][,1]

# DML construction
nca = dml$NCA()

# Fitting algorithm
nca$fit(X,y)

# We can look at the algorithm metadata after fitting it
meta = nca$metadata()
meta
# We can see the metric the algorithm has learned
M = nca$metric()
M
#Equivalent, we can see the learned linear map
L = nca$transformer()
L
# Finally, we can obtain the transformed data ...
Lx = nca$transform()
Lx[1:5,]

# ... or transform new data.
X_ = matrix(nrow = 3, ncol = 4, data = c(1,0,0,0,
                                         1,1,0,0,
                                         1,1,1,0))
Lx_ = nca$transform(X_)
Lx_
