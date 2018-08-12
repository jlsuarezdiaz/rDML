context("Plot testing")

X = iris[1:4]
y = iris[5][,1]
nca = dml$NCA()
lda = dml$LDA()

test_that("knn_plot works",{
  expect_error({
    f = dml_plotter$knn_plot(X[1:2],y,k = 3,dml = nca, cmap = "rainbow")
    f$savefig("./plot_knn.png")
  }, NA)
})

test_that("dml_multiplot works",{
  expect_error({
    f = dml_plotter$dml_multiplot(X[1:2],y,nrow = 4, ncol = 3,ks = c(3,3,3,3,3,3,7,7,7,7,7,7),
                                  dmls = list(NULL,nca,nca,NULL,lda,lda,NULL,nca,nca,NULL,lda,lda), transforms = list(T,F,T,T,F,T,T,F,T,T,F,T),
                                  cmap = "rainbow",figsize = c(24,18), title = "Plots", subtitles = c("3-NN","3-NN-NCA","3-NN-NCA-TRF","3-NN","3-NN-LDA","3-NN-LDA-TRF",
                                                                                                       "5-NN","5-NN-NDA","5-NN-NCA-TRF","5-NN","5-NN-NDA","5-NN-NCA-TRF"))
    f$savefig("./dml_multiplot.png")
  }, NA)
})

test_that("knn_pairplots works",{
  expect_error({
    f = dml_plotter$knn_pairplots(X,y,dml = nca, cmap = "rainbow", k = 5,  figsize = c(24,24))
    f$savefig("./knn_pairplots.png")
  },NA)
})
