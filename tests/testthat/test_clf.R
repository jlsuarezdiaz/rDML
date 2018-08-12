context("Classifiers testing")

X = iris[1:4]
y = iris[5][,1]

exec_clf <- function(myclf,X,y){
  myclf$fit(X,y)
  print(myclf$predict(X))
  print(myclf$score(X,y))
  return(list(myclf$predict(X),myclf$score(X,y)))
}

test_that("K-NN working",{
  nca = dml$NCA()
  nca$fit(X,y)
  knn = distance_clf$kNN(n_neighbors = 5,nca)
  expect_error(exec_clf(knn,X,y),NA)
})

test_that("NCMC working",{
  ncmc = distance_clf$NCMC_Classifier(centroids_num = c(2,3,2))
  expect_error(exec_clf(ncmc,X,y),NA)
})

test_that("MultiDML-kNN working",{
  nca = dml$NCA()
  lda = dml$LDA()
  mknn = distance_clf$MultiDML_kNN(n_neighbors = 7, dmls = list(lda,nca), verbose = TRUE)
  expect_error({
    # PROBLEM WITH DEFAULT PARAMETERS
    mknn$fit(X,y)
    print(mknn$predict_all(X))
    print(mknn$predict_proba_all(X))
    print(mknn$score_all(X,y))
    print(mknn$elapsed())
  },NA)
})
