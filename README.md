[![Oblique three-dimensional view of Baja California: the source-sink surface of Araptus attenuatus rises in red over the central sites and falls to blue in the south, with the Population Graph and arrows of gene flow above it](https://dyerlab.github.io/gstudio/media/gravity-source-sink-3d.png)](https://dyerlab.github.io/gstudio/genetic-gravity.html#sec-gravity-3d)

gstudio
=======

An R package for the spatial analysis of population genetic marker data: a
`locus` data type, genetic diversity, structure, distance and relatedness,
Population Graphs, directional gene flow (genetic gravity), and simulation.

Full documentation: <https://dyerlab.github.io/gstudio/>

*Above: genetic gravity in the beetle* Araptus attenuatus *(the `arapat` data).
The surface is red around sources and blue around sinks, and arrows point with
gene flow.
[How it was made](https://dyerlab.github.io/gstudio/genetic-gravity.html#sec-gravity-3d).*

## Installation

The current version is 1.16.1, for R 4.1 or later:

```r
remotes::install_github( "dyerlab/gstudio" )
```

## Design

**Four gateway functions** cover the core analyses, each choosing an estimator
with `mode =`, so there is one name to remember per question.  Every function
takes its data first, so analyses read as pipelines:

```r
library( gstudio )
library( dplyr )
data( arapat )

arapat |>
  genetic_diversity( mode = "He" )               # A, Ae, He, Ho, Fis, Sh, ...

arapat |>
  genetic_structure( stratum = "Species",
                     mode = "Gst" )              # Gst_prime, Dest, Fst, PhiST, PhiFT

arapat |>
  genetic_distance( stratum = "Species",
                    mode = "cGD" )               # AMOVA, Nei, Bruvo, dcGD, ...

arapat |>
  slice( 1:10 ) |>
  genetic_relatedness( mode = "Nason" )          # LynchRitland, Ritland, Queller
```

Two more follow the same pattern: `genetic_isolation()` (isolation by distance,
by graph distance, or directional) and `genetic_pedigree()` (inbreeding, kinship
and relatedness from a known pedigree).  Tests are named like those in base R,
e.g. `hwe.test()` and `rarefaction.test()`.

The same calls work without pipes, with every option passed as an argument,
e.g. `genetic_distance( arapat, stratum = "Species", mode = "cGD" )`, and
`group_by()` supplies the stratum when it is not given:

```r
arapat |>
  group_by( Species ) |>
  genetic_diversity( mode = c( "A", "Ae", "He" ) )
```

**Tidy results.** Analyses return standard `data.frame`s (or matrices) that you
plot yourself with ggplot2, and tests return `htest` objects, or tables with
[broom](https://broom.tidymodels.org) column names:

```r
library( ggplot2 )

arapat |>
  frequencies( loci = "LTRS", stratum = "Species" ) |>
  ggplot( aes( x = Allele, y = Frequency, fill = Stratum ) ) +
  geom_col( position = "dodge" ) +
  theme_minimal()
```

The main exception is graphs: `plot()` on a Population Graph or a gravity field
returns a ggplot from one shared backend, which you can extend with `+`.  For
fully custom graph figures, use [ggraph](https://ggraph.data-imaginist.com).

## Population Graphs

```r
data( lopho )

lopho |>
  plot()                            # nodes sized by 'size', filled by 'region'

arapat |>
  population_graph() |>
  plot( layout = strata_coordinates( arapat ) )
```

- `population_graph()` / `popgraph()`: build a conditional-independence graph
  from multilocus data.
- `decorate_graph()`: attach data columns (e.g. coordinates, region) as vertex
  attributes.
- `plot()`: options `layout`, `node_size`, `node_labels`, `node_fill`, `edges`
  and `edge_width`.  Directed graphs (`asymmetric_popgraph()`) are drawn with
  arrows.
- `animate_popgraphs()`: animate a sequence of graphs on fixed node positions.
- `popgraph_diagnostics()`: is the sampling design adequate for a Population
  Graph?  `popgraph_null()`: a symmetric null for the directional tests, matched
  to the design by simulation.
- `to_sf()`, `to_json()`, `write_popgraph()`, ...: export.

## Genetic gravity: direction from an undirected graph

Each Population Graph edge is symmetric, but its two populations are compared
against different neighbourhoods.  Genetic gravity reads the direction of gene
flow from that difference.

```r
data( gravity )                     # simulated genotypes, flow toward Pop25

graph <- gravity |>
  population_graph()

deme <- as.integer( sub( "Pop", "", igraph::V( graph )$name ) )

graph |>
  source_sink_scores( x = deme )    # gravity and source-sink score S

graph |>
  source_sink_test( x = deme )      # gradient in S along an axis

graph |>
  ibgd( x = deme )                  # isolation by graph distance (Mantel test)

graph |>
  ibgd( x = deme, mode = "dcGD" )   # does a directional model fit better?

gravity |>
  genetic_distance( mode = "dcGD" ) # directional conditional genetic distance
```

Is a directional result more than the sampling design would give?  Check the
design first; `popgraph_null()` then simulates its symmetric null:

```r
gravity |>
  popgraph_diagnostics( stratum = "Population" )
```

### Maps of sources and sinks

`plot()` draws the source-sink surface with the graph over it.  The surface
fills the convex hull of each connected component, with a margin around its
outer populations:

```r
graph |>
  gravity_field() |>
  plot()                            # map of sources and sinks

graph |>
  gravity_field() |>
  plot( show_graph = FALSE )        # the surface without the graph

graph |>
  gravity_field() |>
  gravity_surface() |>
  head()                            # the surface as a table
```

- Over a base map, `plot( base = , clip_to_base = TRUE )` crops the surface to
  land, as `gravity_surface( clip = )` does.
- `gravity_surface( output = "raster" )` returns a `terra` raster, for GIS or
  other renderers.  The figure at the top was made this way with
  [rayshader](https://www.rayshader.com); the
  [script](https://dyerlab.github.io/gstudio/media/gravity-rayshader.R) is an
  example to adapt, and not part of the package.
- `gravity_congruence()` compares the direction in two graphs of the same
  populations, and has its own `plot()`.

The [Genetic Gravity](https://dyerlab.github.io/gstudio/genetic-gravity.html)
chapter has the model and a full walkthrough.

## Interactive maps

`strata_coordinates()` gives population centroids, and `addAlleleFrequencies()`
adds allele-frequency pie charts to a leaflet map:

```r
library( leaflet )
library( leaflet.minicharts )

freqs <- arapat |>
  frequencies( loci = "LTRS", stratum = "Species" )

arapat |>
  strata_coordinates( stratum = "Species" ) |>
  leaflet() |>
  addTiles() |>
  addAlleleFrequencies( freqs )
```

## Simulation and I/O

- `make_populations()`, `migration_matrix()`, `simulate_pop()`: forward-time,
  individual-based simulation, including directional stepping stones, fast
  enough for thousands of generations
  ([chapter](https://dyerlab.github.io/gstudio/simulation.html)).
- `paternity()`, `parent_finder()`, `transition_probability()`: parentage
  ([chapter](https://dyerlab.github.io/gstudio/parentage.html)).
- `read_population()` / `write_population()`: read and write genotype files.
- `to_genepop()`, `to_structure()`, ...: export to other programs.

## What's new in 1.16

- `gravity_surface()`: the source-sink surface as a table or raster, clipped to
  land if you like.
- Surfaces fill the hull of each component, with a line where two components
  meet, since $S$ is comparable only within one.
- `popgraph_diagnostics()` and `popgraph_null()`: is the design adequate, and
  what would it give under symmetric gene flow?
- `simulate_pop()` is about 50 times faster, and the documentation has an
  Individual-Based Simulation chapter.
- Resampling tests default to 999 replicates, and `genetic_structure()` returns
  `p.value` only when `nperm` > 0.

### Coming from 1.14 or earlier

Version 1.15 consolidated the interface, and these changes break older code:

| 1.14 | Now |
|---|---|
| `He()`, `Fis()`, ... | `genetic_diversity( mode = )` |
| `Fst()`, `Gst()`, `Dest()`, ... | `genetic_structure( mode = )` |
| `dist_nei()`, ... | `genetic_distance( mode = )` |
| `rel_nason()`, ... | `genetic_relatedness( mode = )` |
| `hwe()`, `rarefaction()` | `hwe.test()`, `rarefaction.test()` |
| `multilocus_diversity()`, `exclusion_probability()` | `genetic_diversity( mode = "Dm" )`, `genetic_diversity( mode = "Pex" )` |
| `to_SpatialPoints()`, `to_SpatialLines()` | `to_sf()` |

Single tests return `htest` objects, and table columns follow one naming
convention (`Stratum`, `from`/`to`, `statistic`, `p.value`).  See
[`NEWS`](NEWS) for the full list.

## Citation

Dyer RJ (2009) GStudio: a suite of tools for the spatial analysis of genetic
marker data. *Molecular Ecology Resources* **9**: 110--113.

## Contributing

Questions, bug reports and contributions are welcome: open an issue at
<https://github.com/dyerlab/gstudio/issues>, or contact
[Rodney J. Dyer](mailto:rjdyer@vcu.edu).
