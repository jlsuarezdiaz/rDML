#source("R/metadata.R")
#source("R/init_py.R")

pre_dml_ = NULL

.filter_dmls <- function(){
  pre_dml__ <- list()
  console.log("Filtering DML Algorithms...")
  for(alg in all_dmls){
    console.log(paste("* Filtering ",toString(alg)," ..."))
    pre_dml__[[alg]] = pydml[[alg]]
  }
  pre_dml_ <<- pre_dml__
}

pre_classifiers_ = list()

.filter_clfs <- function(){
  pre_classifiers__ <<- pre_classifiers_
  console.log("Filtering classifiers...")
  for(clf in classifiers){
    console.log(paste("* Filtering ",toString(clf)," ..."))
    pre_classifiers__[[clf]] = pydml[[clf]]
  }
  pre_classifiers_ <<- pre_classifiers__
}

pre_tuners_ = list()

.filter_tuners <- function(){
  pre_tuners__ <<- pre_tuners_
  console.log("* Filtering tuners...")
  for(tun in tuners){
    console.log(paste("* Filtering ",toString(tun)," ..."))
    pre_tuners__[[tun]] = pydml[[tun]]
  }
  pre_tuners_ <<- pre_tuners__
}

pre_plotters_ = list()

.filter_plotters <- function(){
  pre_plotters__ <<- pre_plotters_
  console.log("* Filtering plotters")
  for(plt in plotters){
    console.log(paste("* Filtering ",toString(plt)," ..."))
    pre_plotters__[[plt]] = pydml[[plt]]
  }
  pre_plotters_ <<- pre_plotters__
}



