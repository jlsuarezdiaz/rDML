context("DML Algorithms testing")

X = iris[1:4]
y = iris[5][,1]

exec_dml <- function(mydml,X,y){
  mydml$fit(X,y)
  #print(mydml$metric())
  #print(mydml$transformer())
  #print(mydml$transform(X))
  return(list(mydml$metric(),mydml$transformer(),mydml$transform(X)))
}

expect_error_list <- function(l,X,y){
  for(alg in l){
    expect_error(exec_dml(alg,X,y),NA)
  }
}

test_that("PCA working",{
  pca1 = dml$PCA()
  pca2 = dml$PCA(num_dims = 2)
  pca3 = dml$PCA(thres = 0.6)
  expect_error_list(list(pca1,pca2,pca3),X,y)
})

test_that("LDA working",{
  lda1 = dml$LDA()
  lda2 = dml$LDA(num_dims = 1)
  lda3 = dml$LDA(thres = 0.6)
  expect_error_list(list(lda1,lda2,lda3),X,y)
})

test_that("ANMM working",{
  anmm1 = dml$ANMM()
  anmm2 = dml$ANMM(num_dims = 2)
  anmm3 = dml$ANMM(n_friends = 5, n_enemies = 3)
  expect_error_list(list(anmm1,anmm2,anmm3),X,y)
})

test_that("LMNN working",{
  lmnn1 = dml$LMNN()
  lmnn2 = dml$LMNN(solver = "SGD",num_dims = 2)
  lmnn3 = dml$LMNN(k = 5, eta0 = 0.1, max_iter = 5)
  expect_error_list(list(lmnn1,lmnn2,lmnn3),X,y)
})

test_that("NCA working",{
  nca1 = dml$NCA()
  nca2 = dml$NCA(descent_method = "BGD", num_dims = 2)
  nca3 = dml$NCA(eta0 = 0.1, max_iter = 5)
  expect_error_list(list(nca1,nca2,nca3),X,y)
})

test_that("NCMML working",{
  ncmml1 = dml$NCMML()
  ncmml2 = dml$NCMML(num_dims = 2, descent_method = "BGD")
  ncmml3 = dml$NCMML(eta0 = 0.1, max_iter = 10)
  expect_error_list(list(ncmml1,ncmml2,ncmml3),X,y)
})

test_that("NCMC working",{
  ncmc1 = dml$NCMC()
  ncmc2 = dml$NCMC(num_dims = 2, descent_method = "BGD", eta0 = 0.1, max_iter = 10)
  ncmc3 = dml$NCMC(centroids_num = c(3,2,3))
  expect_error_list(list(ncmc1,ncmc2,ncmc3),X,y)
})

test_that("ITML working",{
  itml1 = dml$ITML()
  itml2 = dml$ITML(num_constraints = 20, max_iter = 200)
  itml3 = dml$ITML(gamma = 0.01, tol=1e-10)
  expect_error_list(list(itml1,itml2,itml3),X,y)
})

test_that("DMLMJ working",{
  dmlmj1 = dml$DMLMJ()
  dmlmj2 = dml$DMLMJ(num_dims = 2, n_neighbors = 7)
  dmlmj3 = dml$DMLMJ(alpha = 0.0001, reg_tol = 1e-8)
  expect_error_list(list(dmlmj1,dmlmj2,dmlmj3),X,y)
})

test_that("MCML working",{
  mcml1 = dml$MCML()
  mcml2 = dml$MCML(learning_rate = "constant", max_iter = 18 )
  expect_error_list(list(mcml1,mcml2),X,y)
})

test_that("LSI working",{
  lsi1 = dml$LSI(supervised=TRUE)
  lsi2 = dml$LSI(max_iter = 10, max_proj_iter = 200,supervised=TRUE)
  lsi3 = dml$LSI(itproj_err = 0.01, err = 0.01, learning_rate = "constant",supervised=TRUE)
  expect_error_list(list(lsi1,lsi2,lsi3),X,y)
})

test_that("DML-eig working",{
  dml_eig1 = dml$DML_eig()
  dml_eig2 = dml$DML_eig(mu = 0.2, max_it = 12, eps = 0.001)
  expect_error_list(list(dml_eig1,dml_eig2),X,y)
})

test_that("LDML working",{
  ldml1 = dml$LDML()
  ldml2 = dml$LDML(b = 0.5, max_iter = 22)
  expect_error_list(list(ldml1,ldml2),X,y)
})

test_that("KLMNN working",{
  klmnn1 = dml$KLMNN()
  klmnn2 = dml$KLMNN(kernel = 'rbf',gamma=1.0, target_selection = "original")
  klmnn3 = dml$KLMNN(kernel = 'poly', degree = 2, coef0 = 1.2)
  expect_error_list(list(klmnn1,klmnn2,klmnn3),X,y)
})

test_that("KANMM working",{
  kanmm1 = dml$KANMM()
  kanmm2 = dml$KANMM(kernel = 'rbf',gamma=1.0)
  kanmm3 = dml$KANMM(kernel = 'poly', degree = 2, coef0 = 1.2)
  expect_error_list(list(kanmm1,kanmm2,kanmm3),X,y)
})

test_that("KDMLMJ working",{
  kdmlmj1 = dml$KDMLMJ()
  kdmlmj2 = dml$KDMLMJ(kernel = 'rbf',gamma=1.0)
  kdmlmj3 = dml$KDMLMJ(kernel = 'poly', degree = 2, coef0 = 1.2)
  expect_error_list(list(kdmlmj1,kdmlmj2,kdmlmj3),X,y)
})

test_that("KDA working",{
  kda1 = dml$KDA()
  kda2 = dml$KDA(kernel = 'rbf', gamma=1.0)
  kda3 = dml$KDA(kernel = 'poly', degree = 2, coef0 = 1.2)
  expect_error_list(list(kda1,kda2,kda3),X,y)
})

test_that("Metric working",{
  M = matrix(data = c(2.0,0.0,0.0,0.0,0.0,5.0,0.0,0.0,0.0,0.0,3.0,0.0,0.0,0.0,0.0,1.0),nrow = 4,ncol = 4)
  metric = dml$Metric(metric = M)
  expect_error_list(list(metric),X,y)
})

test_that("Transformer working",{
  L = matrix(data = c(2.0,0.0,0.0,0.0,0.0,5.0,0.0,0.0,0.0,0.0,3.0,0.0),nrow = 3,ncol = 4)
  transformer = dml$Transformer(transformer = L)
  expect_error_list(list(transformer),X,y)
})

test_that("Euclidean working",{
  euclidean = dml$Euclidean()
  expect_error_list(list(euclidean),X,y)
})


