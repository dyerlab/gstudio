gstudio
=======

An R package for the spatial analysis of population genetic data, including Population Graph construction, simulation, and directional gene flow analysis.

For a complete overview of the package, see the full documentation at http://dyerlab.github.io/gstudio/

To install from GitHub, fire up R and run:

```r
remotes::install_github("dyerlab/popgraph")
remotes::install_github("dyerlab/gstudio")
```

## Key functions

**Population simulation**
- `make_populations()` / `migrate()` / `mixed_mating()` — forward-time individual-based simulation
- `partition()` / `Fst()` — summary statistics

**Population Graph construction**
- `popgraph(x, groups)` — constructs a Markov Random Field graph from multilocus genetic data; returns an undirected weighted `popgraph` / `igraph` object

**Directional gene flow (genetic gravity)**
- `graph_asymmetries(graph)` — takes any weighted undirected `popgraph` and adds three new attributes without altering the topology:
  - `V(graph)$bandwidth` — per-node local bandwidth $b_i$ = mean edge weight (strength / degree)
  - `E(graph)$w_away`, `E(graph)$w_to` — directional Gaussian kernel weights for each edge
  - `E(graph)$delta` — asymmetry index $\Delta_{ij} = w_{i \to j} - w_{j \to i}$; positive values indicate net gene flow from the first to the second endpoint

The bandwidth estimator is grounded in the Markov structure of the Population Graph: conditional independence across non-adjacent nodes means the neighborhood edge weights are sufficient statistics for the decay of genetic signal at each node, so no global distance matrix or iterative calibration is required.

## Contributing

If you would like to contribute to this package or have any questions regarding its construction, use, or additional functionality, please contact [Rodney J. Dyer](mailto:rjdyer@vcu.edu) or visit his lab page at http://dyerlab.org
