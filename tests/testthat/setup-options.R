# Isolate the tests from the developer's own tabtools defaults: clear every
# tabtools.* option loaded from a saved profile and point the config
# directory at a temporary one for the whole run.
local({
  opts <- grep("^tabtools\\.", names(options()), value = TRUE)
  withr::local_options(stats::setNames(rep(list(NULL), length(opts)), opts),
                       .local_envir = testthat::teardown_env())
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir(.local_envir = testthat::teardown_env()),
                      .local_envir = testthat::teardown_env())
})
