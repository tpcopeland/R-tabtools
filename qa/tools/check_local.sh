#!/usr/bin/env bash
# qa/tools/check_local.sh - the macOS R-CMD-check CI job, run on this machine.
#
# Use it on a Mac in place of the macOS Actions job, which bills 10x Linux
# minutes. It runs the same steps as that release job:
#   1. install the package's dependencies (all of Suggests, plus rcmdcheck),
#      upgrading any that are older than CRAN's, as CI installs the latest;
#   2. R CMD build, then R CMD check --as-cran --no-manual on the tarball,
#      failing on any WARNING or ERROR (CI's error-on: "warning");
#   3. qa/run_all.R quick and interaction against a fresh install.
#
# Usage, from anywhere inside the repo:
#   bash qa/tools/check_local.sh                 # CI parity
#   bash qa/tools/check_local.sh --full          # step 3 runs the full QA lane
#   bash qa/tools/check_local.sh --source-tests  # also the ubuntu job's source-tree tests
#   bash qa/tools/check_local.sh --no-deps       # skip step 1
#
# --full also installs ipw (validation_wttab.R needs it) and cobalt and
# twang (crossval_smd_balance.R needs them); Suggests lists none of them. --source-tests runs the test suite from the source tree, where
# the StataCorp fixtures that .Rbuildignore drops exist, as the ubuntu
# release job does (CI never runs those tests on macOS).
#
# Needs R, pandoc (to build the vignettes) and a C compiler (on macOS:
# `xcode-select --install`). Everything runs on a copy of the working tree
# (uncommitted changes included; .git and trailer/ left out) in a temporary
# directory, whose path is printed first; the source tree is not modified.
# Run the script from its place in the repo, not through a symlink.
set -euo pipefail

full=false
deps=true
source_tests=false
for arg in "$@"; do
  case "$arg" in
    --full) full=true ;;
    --no-deps) deps=false ;;
    --source-tests) source_tests=true ;;
    *) echo "unknown option: $arg" >&2; exit 64 ;;
  esac
done

repo="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
out="$(mktemp -d "${TMPDIR:-/tmp}/tabtools-check.XXXXXX")"
export NOT_CRAN=true _R_CHECK_FORCE_SUGGESTS_=true
step() { printf '\n==> %s\n' "$*"; }

step "Output directory: $out"
step "tabtools working tree at $(git -C "$repo" rev-parse --short HEAD) on $(uname -sm), $(Rscript -e 'cat(R.version.string)')"

# A copy of the working tree: R CMD build copies its whole source directory
# first, and trailer/ holds gigabytes of untracked media tooling.
src="$out/source"
rsync -a --exclude .git --exclude trailer "$repo/" "$src/"

# An installed copy can shadow the one under test.
Rscript -e 'for (lib in .libPaths()) if (dir.exists(file.path(lib, "tabtools"))) remove.packages("tabtools", lib = lib)'

if $deps; then
  step "Installing dependencies"
  Rscript -e 'options(repos = c(CRAN = "https://cloud.r-project.org"))
    for (p in c("remotes", "rcmdcheck")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
    remotes::install_deps(commandArgs(TRUE)[1], dependencies = TRUE, upgrade = "always")
    if (commandArgs(TRUE)[2] == "true") for (p in c("ipw", "cobalt", "twang")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)' \
    "$src" "$full"
fi

step "R CMD build"
(cd "$out" && R CMD build "$src")
tarball="$(ls "$out"/tabtools_*.tar.gz)"

step "R CMD check --as-cran"
Rscript -e 'rcmdcheck::rcmdcheck(commandArgs(TRUE)[1], args = c("--no-manual", "--as-cran"),
                                  check_dir = commandArgs(TRUE)[2], error_on = "warning")' \
  "$tarball" "$out"

if $source_tests; then
  step "Source-tree tests"
  Rscript -e 'testthat::test_local(commandArgs(TRUE)[1], stop_on_failure = TRUE, stop_on_warning = TRUE)' "$src"
fi

if $full; then
  step "QA lane: full"
  Rscript "$src/qa/run_all.R" full
else
  step "QA lane: quick"
  Rscript "$src/qa/run_all.R" quick
  step "QA lane: interaction"
  Rscript "$src/qa/run_all.R" interaction
fi

step "PASS: every step succeeded. Check output: $out"
