gstudio
=======

An R package for the spatial analysis of population genetic marker data, including population graph construction, genetic diversity and structure estimation, directional gene flow analysis, and interactive visualization.

For complete documentation see: http://dyerlab.github.io/gstudio/

## Installation

```r
remotes::install_github("dyerlab/gstudio")
```

## Tidy data philosophy

All analysis functions return standard `data.frame` objects that plug directly into ggplot2. The package does not wrap ggplot2 internally — you get the data back and plot it yourself. Population graphs are visualized with the [ggraph](https://ggraph.data-imaginist.com) package.

```r
library(gstudio)
library(ggplot2)
data(arapat)

# Allele frequencies → hand off to ggplot
frequencies(arapat, loci = "LTRS", stratum = "Species") |>
  ggplot(aes(x = Allele, y = Frequency, fill = Stratum)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()
```

## Key functions

**Locus data**
- `locus()` / `as.locus()` — create diploid locus objects from various input formats
- `frequencies()` — tidy allele frequency table (zero-fills missing allele/stratum combinations when `stratum=` is supplied)
- `genetic_diversity()` — per-locus diversity statistics (He, Ho, Ae, Fis, …)
- `genetic_distance()` — pairwise distance matrices (AMOVA, Euclidean, Nei, cGD, …)
- `genetic_structure()` — Gst, Gst′, Dest, Fst

**Population graphs**
- `population_graph(data, stratum)` — build a conditional-independence graph from multilocus data
- `decorate_graph(graph, data, stratum)` — attach data frame columns as vertex attributes
- `asymmetric_weights(graph)` — tSNE-inspired directional edge weights; returns a tidy data frame with `dij`, `dji`, and `Delta` per edge
- `asymmetric_popgraph(graph)` — convert an undirected popgraph to a directed one using the asymmetric weights

**Interactive maps**
- `strata_coordinates(data)` — population centroid data frame (`Stratum`, `Longitude`, `Latitude`)
- `addAlleleFrequencies(map, freqs)` — add allele-frequency pie charts to a leaflet map following the pipe convention:

```r
library(leaflet)
library(leaflet.minicharts)

freqs <- frequencies(arapat, loci = "LTRS", stratum = "Species")

strata_coordinates(arapat, stratum = "Species") |>
  leaflet() |>
  addTiles() |>
  addAlleleFrequencies(freqs)
```

**Simulation**
- `simulate_pop()` / `make_population()` / `mate()` / `migrate()` — forward-time individual-based simulation

**I/O**
- `read_population()` / `write_population()` — read/write genotype data
- `to_sf()` — convert popgraph nodes or edges to an `sf` spatial object
- `to_genepop()` / `to_structure()` / `to_json()` — export to common formats

## Deprecated

- `to_SpatialPoints()` / `to_SpatialLines()` — use `to_sf()` instead (sp package superseded by sf)

## Contributing

Questions, bug reports, and contributions welcome. Contact [Rodney J. Dyer](mailto:rjdyer@vcu.edu) or visit http://dyerlab.org
