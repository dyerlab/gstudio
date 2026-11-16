# gstudio ↔ DyerlabFoundation parity notes

Written 2026-11-16 while bringing DyerlabFoundation's analyses in line with gstudio 1.15.5 (working tree, including
the uncommitted `structure_components.R` / `gst_prime.R` / `dest.R` / `genetic_structure.R` changes). Each item is a
place where the two give different answers, with what Swift does and what would bring them together. The Swift
fixtures live in `DyerlabFoundation/Tests/*/Fixtures/generate_*.R`.

## In parity (no action needed)

These match R to 1e-10 or better, and the seeded ones draw for draw (`set.seed()` → `RMersenneTwister`):

- `genetic_diversity()`: A, Ae, A95, He, Ho, Fis, Pe, Sh, Pex and Dm (`generate_diversity_fixtures.R`).
- `hwe.test()` per stratum and locus, with the continuity correction (`generate_hwe_fixtures.R`).
- `rarefaction.test()`: every mode, smallest-stratum and fixed `size`, `na.rm`, every resampled value
  (`generate_strata_rarefaction_fixtures.R`).
- `transition_probability()`: compatibility, `freqs =` (one parent with a random mate) and two parents, per locus and
  multilocus (`generate_transition_fixtures.R`).
- Missing-mother reconstruction for 2Gener (`.infer_maternal`, likelihood ratio ≥ 20)
  (`generate_maternal_inference_fixtures.R`).
- `congruence_topology()` with both graphs' attributes (`name.1`/`name.2` where they differ).
- `gravity_congruence()`: node and edge tables, sign test, and the ρ permutation draws. ρ itself differs where S has
  near-ties; see below.
- Gst, G′st and Dest per locus: the working tree's unweighted p̄ for Ht and per-locus k and ñ agree with mmod/hierfstat
  to 1e-15 on arapat.

## 1. Multilocus G′st and Dest: default `multilocus = "harmonic"`

Swift uses the mmod/hierfstat convention: the estimator is applied to the mean Hs and Ht across loci. That is
`multilocus = "average"` in the working tree, which matches mmod exactly. The default is still `"harmonic"` (the
harmonic mean of per-locus estimates, with monomorphic loci and negative estimates dropped). On arapat:

| | `"harmonic"` (default) | `"average"` = mmod = Swift |
|---|---|---|
| G′st | 0.6934 | 0.6789 |
| Dest | 0.3785 | 0.4121 |
| Gst | 0.4538 | 0.4538 |

**To align:** make `"average"` the default, or document in `?genetic_structure` that only `"average"` reproduces
mmod/hierfstat and the Swift apps.

Also note: mmod's `Gst_Hedrick()` is Meirmans & Hedrick's (2011) G″st, k(Ht − Hs)/((k·Ht − Hs)(1 − Hs)), not
Hedrick's (2005) G′st. Any docs or tests that compare `Gst_prime()` with `mmod::Gst_Hedrick()` are comparing two
different statistics.

## 2. `hwe.test(stratum =)` stops at an ungenotyped stratum × locus

```r
hwe.test(arapat_df, stratum = "Population")
#> Error in data.frame(Genotype = names(t), Observed = as.numeric(t), Expected = 0, :
#>   arguments imply differing number of rows: 0, 1
```

`genotype_frequencies()` can't build its table from no genotypes. One stratum with nothing typed at one locus is
enough to stop the whole call. Swift skips that cell (`HardyWeinbergAnalysis` returns `nil`). The Swift fixture works
around this by testing each cell on its own.

**To align:** skip (or return NA for) cells with no genotypes.

## 3. `hwe.test()` with a single allele gives df = 0, P = 0

At a locus monomorphic in a stratum, the test reports `statistic 0.083, df 0, p.value 0`. A P of 0 reads as strong
departure from HWE. Swift returns no test (`nil`); there is nothing to test.

**To align:** NA, or drop the row.

## 4. Multilocus Pex when a locus is untyped

`genetic_diversity(mode = "Pex")` gives NA for the multilocus 1 − Π(1 − Pex) when any locus is NA in that stratum.
Swift's `StratumReport` combines the loci that are typed. `rarefaction.test(na.rm = TRUE)` already does what Swift
does.

**To align:** an `na.rm` for `genetic_diversity()`'s multilocus Pex, or accept the difference (it is documented in
Swift).

## 5. `gravity_congruence()`: ρ ranks floating-point noise

Structurally equal source-sink scores (equal-degree nodes along a chain, for example) differ in their last bits, so
`cor(S1, S2, method = "spearman")` ranks them by rounding noise. In the Swift fixture's `gravity` case (Population
Graphs of loci 1–10 vs 11–20), seven populations of graph 2 share S₂ = −0.0568027977709145 ± 1e-16:

| | ρ | P (199 perms, seed 3) |
|---|---|---|
| gstudio | −0.0846 | 0.685 |
| near-ties merged | −0.1050 | 0.730 |

`source_sink_test()` already merges these with `.merge_ties()`. Swift's `GravityCongruenceAnalysis` does the same
before ranking.

**To align:** `S1 <- .merge_ties(S1); S2 <- .merge_ties(S2)` before the correlation in `gravity_congruence()`.

## 6. 2Gener / PhiFT data handling

Swift follows Smouse et al. (2001) and the decisions recorded with it. Four rules still differ from gstudio:

| Situation | gstudio | Swift |
|---|---|---|
| Ambiguous paternal gamete (offspring and mother share a heterozygote) | weighted by the gamete allele frequencies | weighted by an EM estimate of the study-wide pollen pool (`.paternalAverage` gives the original ½/½) |
| Offspring incompatible with its mother at a locus | that locus set to missing | the offspring dropped entirely |
| Offspring genotype missing at a locus | dropped | imputed as the pollen-pool frequency vector |
| Mother missing and not reconstructable (likelihood ratio < 20) | her offspring set to missing at that locus | her offspring imputed as above, with a warning |

These are judgement calls, not bugs. Pick one convention per row, then change whichever side disagrees.

## Not yet ported to Swift

Pedigree tools and Bruvo distance (deferred on purpose). `gravity_field()` and the plotting functions are R-only.
