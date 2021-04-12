library(reticulate)

#install_miniconda()
conda_create("my_reticulate")
use_virtualenv("my_reticulate" = NULL, required = FALSE)

py_install("pandas")
py_install("numpy")
py_install("requests")
py_install("bs4")
py_install("geoplot")
py_install("geopandas")
py_install("matplotlib")
py_install("seaborn")
py_install("lxml")
py_install("mapclassify")

conda_install(
  envname = "my_reticulate",
  "plydata",
  forge=FALSE,
  pip = TRUE) ## tidyverse port into python!


##clean up at the end
conda_remove("my_reticulate")
