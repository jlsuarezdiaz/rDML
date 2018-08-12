context("Tune testing")

X = iris[1:4]
y = iris[5][,1]

test_that("k-NN Tune working",{
  expect_error({
    l = dml_tune$tune_knn(dml = "NCA",X = X,y = y, dml_params = list(descent_method = "SGD"), tune_args = list(eta0 = c(0.3,0.1,0.01), num_dims = as.integer(c(2,4))),n_neighbors = 3,
                      n_folds = 5,n_reps = 2, verbose = TRUE, seed = 28)
    print(l[[1]])
    print(l[[2]])
    print(l[[3]])
    print(l[[4]])
  },NA)
})

test_that("Metadata Tune working",{
  expect_error({
    l = dml_tune$tune(dml = "NCA",X = X,y = y, dml_params = list(descent_method = "SGD"), tune_args = list(eta0 = c(0.3,0.1,0.01), num_dims = as.integer(c(2,4))),
                      metrics = list("final_expectance", 3,5),n_folds = 5,n_reps = 2, verbose = TRUE, seed = 28)
    print(l[[1]])
    print(l[[2]])
    print(l[[3]])
    print(l[[4]])
  },NA)
})
