## Resubmission

This package was archived on CRAN on 2015-06-09 (last CRAN version 1.3). It has
since been completely rewritten and is actively maintained; all check errors
that led to archival have been resolved.

This submission (v1.15.0) consolidates the interface:

* Analyses go through four gateway functions (`genetic_diversity()`,
  `genetic_structure()`, `genetic_distance()`, `genetic_relatedness()`); the
  former standalone estimators are internal.
* Tests return standard `htest` objects, and result tables follow one column
  convention (`Stratum`, `from`/`to`, `statistic`, `p.value`), so output works
  with `broom::tidy()` and dplyr pipelines, including `group_by()`.
* New genetic-gravity tools for inferring the direction of gene flow from a
  Population Graph (`gravity_field()`, `source_sink_test()`, `ibgd()`,
  `genetic_distance(mode = "pgd")`), with a new vignette and example data.
* `plot()` methods for Population Graphs and gravity fields return ggplot
  objects.

The interface changes are listed in NEWS as [BREAKING] entries.

## Test environments

* macOS (aarch64-apple-darwin20), R 4.6.0: `devtools::check(cran = TRUE)`
* win-builder, R-release: TODO (`devtools::check_win_release()`)
* win-builder, R-devel: TODO (`devtools::check_win_devel()`)

## R CMD check results

0 errors | 0 warnings | 0 notes (local)

Expected on CRAN incoming checks:

* checking CRAN incoming feasibility ... NOTE
  New submission. Package was archived on CRAN.

## Reverse dependencies

There are no reverse dependencies: the package is not currently on CRAN, and
`tools::package_dependencies("gstudio", reverse = TRUE)` returns none.
