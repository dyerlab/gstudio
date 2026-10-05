gstudio
=======

An R package for the spatial analysis of population genetic marker data: a
`locus` data type, genetic diversity, structure, distance and relatedness,
Population Graphs, directional gene flow (genetic gravity), and simulation.

Full documentation: <https://dyerlab.github.io/gstudio/>

## Installation

```r
remotes::install_github("dyerlab/gstudio")
```

## Design

**Four gateway functions** cover the analyses, each choosing an estimator with
`mode =`, so there is one name to remember per question.  Every function takes
its data first, so analyses read as pipelines:

```r
library(gstudio)
library(dplyr)
data(arapat)

arapat |>
  genetic_diversity(mode = "He")                       # A, Ae, He, Ho, Hi, Fis, ...

arapat |>
  genetic_structure(stratum = "Species", mode = "Gst") # Gst, Gst_prime, Dest, Fst

arapat |>
  genetic_distance(stratum = "Species", mode = "cGD")  # AMOVA, Nei, cGD, pGD, ...

arapat |>
  slice(1:10) |>
  genetic_relatedness(mode = "Nason")                  # Nason, LynchRitland, ...
```

The same calls work without pipes, with every option passed as an argument,
e.g. `genetic_distance(arapat, stratum = "Species", mode = "cGD")`.

**Tidy results.** Analyses return standard `data.frame`s (or matrices) that you
plot yourself with ggplot2:

```r
library(ggplot2)

arapat |>
  frequencies(loci = "LTRS", stratum = "Species") |>
  ggplot(aes(x = Allele, y = Frequency, fill = Stratum)) +
  geom_col(position = "dodge") +
  theme_minimal()
```

The exception is graphs: `plot()` on a Population Graph or a gravity field
returns a ggplot from one shared backend, which you can extend with `+`.  For
fully custom graph figures, use [ggraph](https://ggraph.data-imaginist.com).

## Population Graphs

```r
data(lopho)

lopho |>
  plot()                                   # nodes sized by 'size', filled by 'Region'

arapat |>
  population_graph() |>
  plot(layout = strata_coordinates(arapat))
```

- `population_graph()` / `popgraph()`: build a conditional-independence graph
  from multilocus data.
- `decorate_graph()`: attach data columns (e.g. coordinates, region) as vertex
  attributes.
- `plot()`: options `layout`, `node_size`, `node_labels`, `node_fill`, `edges`
  and `edge_width`.  Directed graphs (`asymmetric_popgraph()`) are drawn with
  arrows.
- `animate_popgraphs()`: animate a sequence of graphs on fixed node positions.
- `to_sf()`, `to_json()`, `write_popgraph()`, ...: export.

## Genetic gravity: direction from an undirected graph

Each Population Graph edge is symmetric, but its two populations are compared
against different neighbourhoods.  Genetic gravity reads the direction of gene
flow from that difference.

```r
data(gravity)                              # simulated genotypes, flow toward Pop25

graph <- gravity |>
  population_graph()

deme <- as.integer(sub("Pop", "", igraph::V(graph)$name))

graph |>
  source_sink_scores(x = deme)             # gravity and source-sink score S

graph |>
  source_sink_test(x = deme)               # gradient in S along an axis

graph |>
  ibgd(x = deme)                           # isolation by graph distance (Mantel test)

graph |>
  ibgd(x = deme, mode = "pgd")             # does a directional model fit better?

gravity |>
  genetic_distance(mode = "pgd")           # partitioned (directed) cGD

graph |>
  gravity_field() |>
  plot()                                   # map of sources and sinks
```

See `vignette("genetic_gravity")` for the model and a full walkthrough.

## Interactive maps

`strata_coordinates()` gives population centroids, and `addAlleleFrequencies()`
adds allele-frequency pie charts to a leaflet map:

```r
library(leaflet)
library(leaflet.minicharts)

freqs <- arapat |>
  frequencies(loci = "LTRS", stratum = "Species")

arapat |>
  strata_coordinates(stratum = "Species") |>
  leaflet() |>
  addTiles() |>
  addAlleleFrequencies(freqs)
```

## Simulation and I/O

- `make_populations()`, `migration_matrix()`, `simulate_pop()`: forward-time,
  individual-based simulation, including directional stepping stones.
- `read_population()` / `write_population()`: read and write genotype files.
- `to_genepop()`, `to_structure()`, ...: export to other programs.

## Changes in 1.15

Version 1.15 consolidates the interface, and some changes break code written
for 1.14: the standalone estimators (`He()`, `Fis()`, `Fst()`, `Gst()`,
`dist_nei()`, `rel_nason()`, ...) are reached through the gateway functions, `sp`-based
exports are replaced by `to_sf()`, tests return `htest` objects, and table
columns follow one naming convention (`Stratum`, `from`/`to`, `statistic`,
`p.value`).  See `NEWS` for the full list, with old and new names.

## Citation

Dyer RJ (2009) GStudio: a suite of tools for the spatial analysis of genetic
marker data. *Molecular Ecology Resources* **9**: 110--113.

## Contributing

Questions, bug reports and contributions are welcome: open an issue at
<https://github.com/dyerlab/gstudio/issues>, or contact
[Rodney J. Dyer](mailto:rjdyer@vcu.edu).
