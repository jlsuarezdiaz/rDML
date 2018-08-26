.onLoad <- function(lib,pkg,..){

  #Load pyDML library
  .init_py()

  #Filter objects
  .filter_dmls()
  .filter_clfs()
  .filter_tuners()
  .filter_plotters()

  #Init rDML tools
  .init_dml()
  .init_clf()
  .init_tune()
  .init_plot()
}
