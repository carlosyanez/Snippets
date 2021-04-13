library(reticulate)

#install_miniconda()
conda_create("my_reticulate")
use_virtualenv("my_reticulate", required = FALSE)

py_install("pandas")
py_install("numpy")
py_install("requests")
py_install("bs4")
py_install("geoplot")
py_install("geopandas")

py_install("matplotlib") #try different charting libraries
py_install("seaborn")
py_install("plotnine")
py_install("altair")

py_install("lxml")
py_install("mapclassify")



##clean up at the end
conda_remove("my_reticulate")
