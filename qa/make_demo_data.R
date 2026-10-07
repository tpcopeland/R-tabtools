#!/usr/bin/env Rscript
# Source-only projection. No arguments preserve the legacy maker.
# Native: --mode=native --fixtures-dir=qa/demo/data/native-2.5.1 --output=FILE
# Column labels/classes survive; dataset-level labels/notes are omitted by the
# existing data-frame projection. Native .dta originals retain that provenance.
demo_make_main <- function(args, root) {
  source(file.path(root, "qa", "tools", "demo_data.R"), local = environment())
  opt <- demo_options(args, "maker")
  if (!file.exists(file.path(root, "DESCRIPTION")) ||
      !identical(unname(read.dcf(file.path(root, "DESCRIPTION"), "Package")[1, 1]), "tabtools")) demo_abort("Run from the tabtools package root.")
  mode <- opt[["mode"]]
  fx <- if (is.null(opt[["fixtures-dir"]])) file.path(root, "tests", "testthat", "golden", "fixtures") else opt[["fixtures-dir"]]
  data <- if (mode == "native") demo_read_data(fx) else stats::setNames(lapply(demo_rds_names, function(nm) as.data.frame(haven::read_dta(file.path(fx, paste0(nm, ".dta"))))), demo_rds_names)
  projected <- demo_rds_projection(data, mode)
  dest <- if (is.null(opt[["output"]])) file.path(root, "qa", "demo", "demo_tabtools.rds") else opt[["output"]]
  parent <- demo_canonical_dir(dirname(dest))
  dest <- file.path(parent, basename(dest))
  if (demo_is_link(dest) || dir.exists(dest)) demo_abort("Output must be a regular RDS path.")
  tmp <- tempfile(".demo-rds-", tmpdir = parent)
  on.exit(try(unlink(tmp), silent = TRUE), add = TRUE)
  saveRDS(projected, tmp, compress = "xz", version = 3)
  demo_compare_rds(readRDS(tmp), projected)
  if (!file.rename(tmp, dest)) demo_abort("Cannot publish the requested RDS output: ", dest)
  message("wrote ", dest, " from ", mode, " snapshots")
  invisible(dest)
}
if (sys.nframe() == 0L) demo_make_main(commandArgs(trailingOnly = TRUE), normalizePath(getwd()))
