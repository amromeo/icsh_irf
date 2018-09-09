## This makes sure that R loads the workflowr package
## automatically, everytime the project is loaded
library(here,lib.loc = "~/icsh_rf/library")
if (requireNamespace("workflowr", quietly = TRUE)) {
  message("Loading .Rprofile for the current workflowr project")
#  .libPaths("library")
  library("workflowr")

} else {
  message("workflowr package not installed, please run devtools::install_github('jdblischak/workflowr') to use the workflowr functions")
}
