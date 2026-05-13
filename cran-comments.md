## Resubmission

This package was archived on CRAN on 2015-06-09. It has since been completely rewritten, incorporated additional functionality, and continues
to be actively maintained. All previous check errors have been resolved. Major changes include integration of the `popgraph` package, comprehensive test coverage, a vignette, implementation of a simulation framework for individual-based forward time simulations, and conversion to selective namespace imports.

This submission (v1.13) adds `graph_asymmetries()` for directional introgression estimation, a `binary=` parameter to `dist_amova()` for AFLP-style markers, and removes a stray non-CRAN-compliant file from a prior submission.

## Test environments

* macOS Sequoia 15 (aarch64-apple-darwin20), R 4.4.x
* Windows Server 2022 x64 (win-builder), R-release
* Windows Server 2022 x64 (win-builder), R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New submission. Package was archived on CRAN.

This is a resubmission of a fully rewritten package addressing all original issues that led to archival.
