library(testthat)
library(weartime)
conda_env = "weartime"
rtp = Sys.getenv("RETICULATE_PYTHON")
Sys.unsetenv("RETICULATE_PYTHON")
remove_condaenv = FALSE
if (!have_weartime_condaenv()) {
  remove_condaenv = TRUE
  conda_create_weartime(envname = conda_env)
}
use_weartime_condaenv()
reticulate::py_config()

test_check("weartime")
Sys.setenv("RETICULATE_PYTHON" = rtp)
if (remove_condaenv) {
  reticulate::conda_remove(envname = conda_env)
}


