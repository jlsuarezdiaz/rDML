source("R/metadata.R")
source("R/init_py.R")

pre_dml_ = list()

console.log("Filtering DML Algorithms...")
for(alg in all_dmls){
  console.log(paste("* Filtering ",toString(alg)," ..."))
  pre_dml_[[alg]] = pydml[[alg]]
}

pre_classifiers_ = list()

console.log("Filtering classifiers...")
for(clf in classifiers){
  console.log(paste("* Filtering ",toString(clf)," ..."))
  pre_classifiers_[[clf]] = pydml[[clf]]
}

pre_tuners_ = list()

console.log("* Filtering tuners...")
for(tun in tuners){
  console.log(paste("* Filtering ",toString(tun)," ..."))
  pre_tuners_[[tun]] = pydml[[tun]]
}

pre_plotters_ = list()

console.log("* Filtering plotters")
for(plt in plotters){
  console.log(paste("* Filtering ",toString(plt)," ..."))
  pre_plotters_[[plt]] = pydml[[plt]]
}

