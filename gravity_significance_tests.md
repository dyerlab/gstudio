# Gravity significance tests: Existence and Location

**Date:** 2026-09-27
**Package:** gstudio 1.14.4. The runs used the build installed before 2026-09-27. Spot checks (§4) re-ran samples with the current build (commit `c68f96f`, installed 2026-09-27 09:51) and reproduced them exactly.
**Purpose:** a false-positive check of the two gravity asymmetry tests in `gstudio`, prompted by divMigrate never having had one. This file records the results so the theory and expectations behind the tests can be revisited.

**Short version:**
- **Existence test** (`asymmetry_network`, graph level): never rejects, in 1,001 censuses including strongly asymmetric ones. Its null of degree-preserving random rewiring has *more* asymmetry than any real stepping-stone graph, and it often can't be generated at all for dense graphs.
- **Location test** (`asymmetry_permutation`, per edge): works, but is anti-conservative. In an equilibrated symmetric window across 50 replicates: 3.2% / 8.2% / 12.6% of edges significant at α = 0.01 / 0.05 / 0.10. The p-values are not uniform under the null, and the rate varies strongly by lineage.

---

## 1. Existence test (replicate 39, all censuses)

**Test:** `asymmetry_significance(graph, mode = "existence")` → `asymmetry_network(rewire = "degree")`.
- **Statistic:** mean |Δ| over the graph's edges.
- **Null:** `randomize_graph(mode = "degree")` rewiring (same degree sequence), with the observed edge weights shuffled onto the rewired edges.
- **p-value:** one-sided, (1 + #{null ≥ obs}) / (1 + B).

**Run:** replicate 39, all 1,001 censuses: burn-in 0–1999 (401 censuses) plus 2004–2999 for Isotropic, Redistributed and Obstructed (200 each); 99 permutations per census. Two single-census checks at 999 permutations (generation 1999 and Redistributed 2504) gave the same result, p = 1.
- **Script:** `R/gravity_existence_profile.R` (original repo). It is gstudio's null loop unchanged, except that it returns the null values and records `graph_asymmetries()` failures as NA; the current gstudio build now does the same itself.
- **Output:** `data/divMigrate/firstsig/gravity.existence_profile.39.{rda,csv}`

### 1a. The null usually can't be generated
Every failed draw came from `randomize_graph(mode = "degree")` giving up ("Over 100 iterations for finding permutations without duplication or self-loops"). How often it succeeds depends almost entirely on graph density (Spearman ρ between edge count and valid draws = −0.93):

| Graph edges (25 nodes) | Valid null draws |
|---|---|
| ≤ 60 | 100% |
| 61–80 | 89% |
| 81–100 | 24% |
| > 100 | 0.3% |

| Series | Censuses | Valid draws | Censuses with 0 valid draws |
|---|---|---|---|
| Burn-in 0–999 | 201 | 0.2% | 182 |
| Burn-in 1000–1999 | 200 | 4.4% | 127 |
| Isotropic 2004–2499 / 2500–2999 | 100 / 100 | 5.6% / 41.0% | 46 / 4 |
| Redistributed 2004–2499 / 2500–2999 | 100 / 100 | 23.5% / 73.2% | 12 / 1 |
| Obstructed 2004–2499 / 2500–2999 | 100 / 100 | 8.1% / 45.1% | 36 / 10 |

- **Old behaviour:** a census with no valid draws returned (1 + 0)/(1 + 0) = **p = 1**, which is indistinguishable from "no asymmetry".
- **Current behaviour (c68f96f):** it now **stops with an error** ("None of the 99 rewired graphs yielded a valid asymmetry statistic"). That's correct, but those 418 of 1,001 censuses are now untestable.
- **Small point:** the error message suggests `rewire = "degree"`, but these failures already *are* degree-mode rewiring.

### 1b. Where the null can be generated, the test never rejects
Censuses with ≥ 20 valid draws (279 of 1,001):

| Series | Censuses | Rejections (α = 0.05) | Observed below null median | Median z |
|---|---|---|---|---|
| Burn-in 1000–1999 | 14 | 0 | 100% | −3.28 |
| Isotropic 2004–2499 / 2500–2999 | 7 / 62 | 0 / 0 | 100% / 98% | −2.58 / −2.59 |
| Redistributed 2004–2499 / 2500–2999 | 42 / 83 | 0 / 0 | 100% / 100% | −2.60 / −3.35 |
| Obstructed 2004–2499 / 2500–2999 | 11 / 60 | 0 / 0 | 100% / 98% | −2.56 / −2.39 |

**0 rejections in all 1,001 censuses.** Where the null can be computed, the observed mean |Δ| sits about 2.4–3.4 SD *below* the null mean, whatever the migration regime (z = (obs − null mean)/null SD).

Example null distributions (99 draws):

| Census | Observed | Null median (range) |
|---|---|---|
| Redistributed 2904, degree rewiring | 0.048 | 0.094 (0.074–0.108) |
| Burn-in 1999, full rewiring | 0.029 | 0.040 (0.028–0.056) |
| Isotropic 2504, full rewiring | 0.043 | 0.065 (0.043–0.085) |

### 1c. Why (points for the theory)
- **Stepping-stone graphs are locally smooth.** Neighbouring populations have similar edge weights and so similar bandwidths, which keeps Δ small. Degree-preserving rewiring keeps each node's degree but joins nodes whose neighbourhoods have unrelated weight scales. The weight shuffle then puts short and long edges side by side, inflating |Δ| roughly twofold.
- **So the null is "random topology with these weights", not "symmetric migration on this landscape".** Under symmetric migration the observed graph is *less* asymmetric than the null, so a one-sided upper-tail test can't reject.
- **Asymmetric migration raises the observed mean |Δ|**, from about 0.026 in the burn-in to about 0.054 late in Redistributed. **But the null rises with it,** because it resamples the same edge weights. The standardized position never moves toward the rejection region; late Redistributed is the *most* negative (z ≈ −3.35).
- **Implication:** a graph-level Existence null needs to keep the landscape's spatial and weight structure. Options: node-label permutation with the graph fixed (as in Location), a spatially constrained rewiring, or a null built from the Location test's per-edge results (§3).

---

## 2. Location test (replicate 39, all censuses)

Context for §3: this was the first run of the Location test, done before restricting to the equilibrium window.

**Test:** `asymmetry_significance(graph, data = to_mv(genotypes), groups = Population, mode = "location", nperm = 499)` → `asymmetry_permutation`.
- **Null:** individuals' population labels are permuted, a Population Graph is refit with α = 1, and its weights are carried onto the observed graph's fixed edges.
- **p-value:** per edge, two-sided on |Δ|.
- **Inputs:** identical to how the simulation built each graph (`R/simulate.R build_graph`; same call as `fpr_one_snapshot()` in `R/specificity_analysis.R`).

**Run:** replicate 39, all 1,001 censuses, 499 permutations; 0 failures.
- **Script:** `R/gravity_location_profile.R`
- **Output:** `gravity.location_edges.39.rda`, `gravity.location_profile.39.{rda,csv}`

| Series / window | Edges (median) | Edges p < 0.05 | Censuses with ≥ 1 edge significant after BH |
|---|---|---|---|
| Burn-in 0–49 | 191 | 0.020 | 0% |
| Burn-in 50–999 | 121 | 0.064 | 4% |
| Burn-in 1000–1999 | 108 | 0.067 | 4% |
| Isotropic 2004–2249 / 2254–2499 / 2504–2749 / 2754–2999 | 108 / 98 / 90 / 88 | 0.058 / 0.084 / 0.099 / 0.070 | 4% / 14% / 10% / 4% |
| Redistributed, same windows | 93 / 91 / 75 / 72 | 0.118 / **0.157** / 0.093 / 0.108 | 26% / **64%** / 16% / 28% |
| Obstructed, same windows | 106 / 95 / 93 / 81 | 0.097 / 0.137 / **0.189** / 0.141 | 12% / 42% / **72%** / 60% |

- Under asymmetric migration the significant share rises to 2–3× the symmetric level.
- **The timing follows the reorganize / accumulate / fall-apart framing.** Redistributed peaks during accumulation (2254–2499) and falls back toward the symmetric level (~9%) as it falls apart. Obstructed builds more slowly and peaks later (2504–2749).
- **Pendant edges don't matter here:** interior-only rates equal the all-edge rates.

---

## 3. Location test false-positive rate (equilibrated symmetric window, 50 replicates)

**Rationale (user):** burn-in before about generation 1500 is still equilibrating and shouldn't be trusted; after about 2300 the asymmetric treatments begin to fall apart. The equilibrated, pre-treatment window **1900–2000** is used as the false-positive reference: migration is symmetric, so every significant edge is a false positive.

**Run:** all 50 replicates × the 20 censuses from 1904 to 1999 = 1,000 censuses; 499 permutations; 0 failures; 95,903 edges tested.
- **Script:** `gravity_location_window()` in `R/gravity_location_profile.R` (`Rscript R/gravity_location_profile.R window 1900 2000`)
- **Output:** `data/divMigrate/firstsig/gravity.location_window.burnin_1900_2000.{rda,csv}`

### 3a. Per-edge false-positive rate

| α | Observed | × nominal |
|---|---|---|
| 0.01 | 3.15% | 3.2× |
| 0.05 | **8.19%** | 1.6× |
| 0.10 | 12.63% | 1.3× |

This agrees with the manuscript's Tab_FPRCalibration (Isotropic: 3.6% and 8.8% at 0.01 and 0.05), now across 50 replicates.

**Across replicates (per-edge rate at α = 0.05):**

| Min | 10% | 25% | Median | 75% | 90% | Max |
|---|---|---|---|---|---|---|
| 0.042 | 0.053 | 0.062 | 0.077 | 0.093 | 0.112 | 0.169 |

10 of 50 replicates exceed 10%.

**By graph size:**

| Edges | Censuses | Rate at α = 0.05 | Censuses with ≥ 1 BH edge |
|---|---|---|---|
| ≤ 90 | 326 | 0.089 | 0.144 |
| 91–105 | 475 | 0.082 | 0.128 |
| 106–120 | 178 | 0.075 | 0.079 |
| > 120 | 21 | 0.060 | 0 |

Pendant edges: 1 of 95,903.

### 3b. The null is not uniform (points for the theory)
p-value distribution, pooled (uniform = 0.10 per bin):

| p bin | 0–0.1 | 0.1–0.2 | 0.2–0.3 | 0.3–0.4 | 0.4–0.5 | 0.5–0.6 | 0.6–0.7 | 0.7–0.8 | 0.8–0.9 | 0.9–1.0 |
|---|---|---|---|---|---|---|---|---|---|---|
| Share | **0.128** | 0.081 | 0.079 | 0.077 | 0.074 | 0.076 | 0.077 | 0.085 | 0.099 | **0.224** |

- **Both tails are too heavy.** There's an excess near 0, and 22% of edges have p > 0.9, where observed |Δ| is smaller than nearly every permuted value. Under symmetric migration, the observed |Δ| is **more dispersed** than the label-permutation null: some edges are more asymmetric than label-shuffled data would produce, and many are less.
- **Likely reason:** permuting individuals among populations removes all population structure, so the refit null graph describes panmictic data mapped onto the observed edges. It doesn't reproduce the drift-and-migration equilibrium of a stepping-stone landscape. The Methods text says this permutation reproduces the geometric component of Δ in each edge's null, but these p-values show the null distribution still differs in spread from the observed one.
- **The anti-conservatism is worst in the tail** (3× at α = 0.01).

### 3c. Graph-level behaviour

| Rule | Share of symmetric censuses flagged |
|---|---|
| ≥ 1 edge at p < 0.05 | **98.9%** (useless with ~100 edges) |
| ≥ 1 edge significant after BH (q = 0.05) | **12.2%** pooled |

- **Strongly lineage-dependent:** across replicates the BH rate has quartiles 0 / 0 / 0.05 / 0.15, with a maximum of 0.85. 16 replicates never trigger; 2 trigger in at least half their censuses.
- When a census does trigger, a median of 5 edges pass BH (max 18).
- **Interpretation:** some drift-shaped lineages carry persistent realized asymmetry that the test reads as directional. This is the same lineage effect seen with divMigrate, but much weaker.

### 3d. Calibrated thresholds from this window
- **Per edge, 5% true false-positive rate:** p ≤ **0.02**.
- **Per edge, 1%:** p ≤ 0.002, which is the smallest p attainable with 499 permutations; 1.6% of edges already sit at that floor. **A 1% calibration needs more permutations** (≥ 1,999 suggested).
- **Graph level:** the 95th percentile of the per-census share of edges at p < 0.05 is **16.5%**, usable as an Existence-style threshold.

### 3e. Comparison with divMigrate at a symmetric census (generation 1999)

| | divMigrate, unmodified test | Gravity Location test |
|---|---|---|
| Pairs / edges flagged at nominal 5% | ≈ 20% of pairs (median 61 of 300) | ≈ 8% of edges |
| Replicates with any flag | 50 / 50 | ~99% of censuses with any edge at p < 0.05; ~12% after BH |

Gravity's test is clearly better calibrated, but not at nominal level.

---

## 4. Verification after the gstudio changes (2026-09-27)

**gstudio changes** between `464d202` and `c68f96f` in the tested code:
- `asymmetry_network`: `graph_asymmetries()` failures now recorded as NA; **stops if no valid draws** (was p = 1).
- `asymmetry_permutation`: failed permutations recorded as NA and dropped from B (was an abort); new check that `groups` covers every graph node; `alpha` passed via `...` is ignored with a warning.
- `to_mv` / `column_class`: error handling only.
- `randomize_graph`, `popgraph` and `graph_asymmetries` are unchanged.

**Spot checks with the current build, using the original seeds:**

| Test | Sample | Result |
|---|---|---|
| Location (§3 window) | 12 random censuses across replicates, 1,176 edges | **Every Δ and p identical**; edge sets identical; no errors |
| Existence (§1) | 12 censuses of replicate 39 across all series and null sizes | **Identical observed statistic and p** in the 10 with ≥ 1 valid draw. The 2 with zero valid draws now error (intended change; previously p = 1). |

**Conclusion:** none of the gstudio changes alters any reported number. The only behavioural difference is that zero-draw Existence censuses (418 of 1,001 in replicate 39) now fail explicitly instead of returning p = 1.

---

## 5. Open questions for the theory

1. **Existence null:** what is the right graph-level null for "directional structure beyond symmetric migration on this landscape"? Degree-preserving rewiring doesn't qualify: it breaks spatial smoothness and co-moves with asymmetry.
2. **Degree rewiring on dense graphs:** `randomize_graph(mode = "degree")` fails above ~80 edges on 25 nodes. If it's kept, a robust sampler would be needed, e.g. `igraph::rewire(keeping_degseq())` or Viger–Latapy.
3. **Location null dispersion:** why is observed |Δ| more dispersed than the label-permutation null under symmetric migration (excess at both p ≈ 0 and p ≈ 1)? What does label permutation actually hold fixed, compared with what the Methods claim (the geometric component reproduced identically)?
4. **Lineage heterogeneity:** false-positive rates range from 4% to 17% across replicates, and some lineages trigger BH persistently. Is this drift-generated realized asymmetry that a within-snapshot test cannot, in principle, separate from migration asymmetry?
5. **Graph size:** the false-positive rate falls as graphs gain edges (8.9% → 6.0%). Is that from finer conditioning, or from smaller per-edge weight differences?
6. **Calibration:** should reported tests use empirically calibrated thresholds from symmetric simulations (p ≤ 0.02 per edge; > 16.5% of edges at the graph level), or should the null itself be redesigned?
