install.packages(c("usethis", "devtools", "roxygen2", "testthat"))
install.packages("pkgdown")  # for documentation website later
usethis::create_package("T:/Data Warehouse/Global R Scripts/cdetidy")

devtools::load_all()
devtools::document()
devtools::install()
library(cdetidy)
