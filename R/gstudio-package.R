#############################################
#        _                 _       _        #
#     __| |_   _  ___ _ __| | __ _| |__     #
#    / _` | | | |/ _ \ '__| |/ _` | '_ \    #
#   | (_| | |_| |  __/ |  | | (_| | |_) |   #
#    \__,_|\__, |\___|_|  |_|\__,_|_.__/    #
#          |___/                            #
#                                           #
#############################################

#' Routines used in spatial analysis of genetic marker data
#' 
#' gstudio is a suite of tools used in the spatial analysis
#'  of population genetic data created for the book 
#'  "Applied Landscape Genetics" by R.J. Dyer.   This version
#'  of the package (1.X) is a significant deviant from the 
#'  previous (versions 0.X) resulting from an entire re-write
#'  of the package to maximize ease of use and algorithm quickness.
#'  As such, previous scripts using gstudio (vers. 0.X) will need
#'  to be refactored a bit.  While this can be a bit of a pain, the
#'  larger utility of the new format will pay off in spades.
#'
#' \tabular{ll}{
#' Package: \tab gstudio\cr
#' Type: \tab Package\cr
#' Version: \tab 1.12\cr
#' Date: \tab 2026-02-14\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' There are some very good examples of the components of this package are used
#'	in the vignettes for this package.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @docType package
#' @keywords package
#' @importFrom ggplot2 aes ggplot geom_violin geom_line geom_point position_dodge element_blank
#' @importFrom graphics plot.default lines points text
#' @importFrom igraph V E "V<-" "E<-" as_edgelist as_adjacency_matrix as_data_frame vertex_attr edge_attr set_vertex_attr set_edge_attr induced_subgraph degree ends graph_from_adjacency_matrix gsize vertex_attr_names edge_attr_names layout_with_fr distances write.graph
#' @importFrom dplyr group_vars
#' @importFrom methods is
#' @importFrom stats pchisq cor.test cov prcomp qchisq var
#' @importFrom utils read.csv read.table write.csv
#' @importFrom MASS ginv
#' @rdname gstudio-package
#'
"_PACKAGE"

#' Data for Araptus attenuatus
#' 
#' This is a EPIC genetic dataset for the Sonoran desert
#'  beetle Araptus attenuatus
#' @name arapat
#' @docType data
#' @keywords data
NULL


#' AFLP example data set
#' 
#' This is an example of the AFLP data input types.
#' @name data_aflp
#' @docType data
#' @keywords data
NULL



#' Two Column Example Data
#' 
#' Example data for 2-column data types
#' @name data_2_column
#' @docType data
#' @keywords data
NULL



#' Separated Example Data
#' 
#' Example data for separated data types
#' @name data_separated
#' @docType data
#' @keywords data
NULL


#' SNP Example Data
#' 
#' Example data for SNP data types
#' @name data_snp
#' @docType data
#' @keywords data
NULL


#' Zyme-like Example Data
#' 
#' Example data like zyme genotypes
#' @name data_zymelike
#' @docType data
#' @keywords data
NULL

#' Cornus mom/offspring data set
#' 
#' Example data set for demonstrating parent/offspring data
#' @name cornus_florida
#' @docType data
#' @keywords data
NULL

#' Cornus adult data
#' 
#' Example data set with adult multilocus genotypes from
#' Cornus florida at the Rice Center
#' @name cornus
#' @docType data
#' @keywords data
NULL

#' CDPop Data
#' 
#' Example data set for CDPop input
#' @name grid.csv
#' @docType data
#' @keywords data
NULL

#' SNP probability data
#' 
#' Example data for probabilistically calling snps
#' @name snp_prob.csv
#' @docType data
#' @keywords data
NULL

#' AFLP Arapat data
#'
#' A set of genotypes for arapat from an AFLP dataset
#' @name aflp_arapat
#' @docType data
#' @keywords data
NULL

#' Metadata for Baja Populations.
#'
#' This is metadata associated with the sampling locations
#'  for the Lophocereus and Upiga data sets.
#' @name baja
#' @docType data
#' @keywords data
NULL

#' Lophocereus population graph
#'
#' The Population Graph of the senita cactus, \emph{Lophocereus schottii}, from
#'  Dyer & Nason (2004): 21 populations (12 peninsular Baja California, 9
#'  continental Sonora and southern Arizona) estimated from the allozyme data
#'  of Nason et al. (2002).  The 50 edges are those marked significant in
#'  Table 1 of Dyer & Nason (2004), weighted by the multivariate statistical
#'  distances \eqn{d_{ij}} in the same table.  Vertex attributes are
#'  \code{size} and \code{region} (\code{"Baja"}, \code{"Sonora"}); sampling
#'  coordinates are in \code{\link{baja}}.  Rebuilt from the published table
#'  by \code{data-raw/lopho.R} in the package sources.
#' @references Dyer, R. J., & Nason, J. D. (2004). Population graphs: the
#'  graph theoretic shape of genetic structure. \emph{Molecular Ecology}, 13,
#'  1713--1727.
#'
#'  Nason, J. D., Hamrick, J. L., & Fleming, T. H. (2002). Gene migration,
#'  range expansion, and phylogeography of the columnar cactus genus
#'  \emph{Lophocereus}. \emph{Evolution}, 56, 2214--2226.
#' @name lopho
#' @docType data
#' @keywords data
NULL

#' Upiga population graph
#'
#' This is the population graph for the Upiga data
#'  that is currently unpublished.
#' @name upiga
#' @docType data
#' @keywords data
NULL























#' Pedigree of House Targaryen
#'
#' The pedigree of House Targaryen from George R.R. Martin's fiction, from the
#'  parents of Aegon the Conqueror to Daenerys Targaryen, Jon Snow and the
#'  Baratheon brothers: 101 individuals across 16 generations, 17 of them
#'  founders.  Generations of sibling, half-sibling and uncle-niece marriages
#'  make it a worked example of inbreeding from a pedigree; see
#'  \code{\link{genetic_pedigree}}.
#'
#' Parentage follows the books (the official, legitimate parentage), not the
#'  television adaptations, except that Jon Snow is entered as the son of
#'  Rhaegar Targaryen and Lyanna Stark, which the books imply and the
#'  television series confirms.  Spouses who married into the family are
#'  entered as unrelated founders, although some had Targaryen ancestors, so
#'  F is underestimated for their descendants.  Some children who died
#'  young, and most members of cadet branches, are omitted.
#'
#' @format A \code{data.frame} with 101 rows and 7 columns:
#'  \describe{
#'    \item{ID}{Unique key (e.g. \code{"Aegon_I"}, \code{"Daenerys_S"}).
#'      Names repeat across generations, so a suffix distinguishes them.}
#'    \item{Name}{Display name.}
#'    \item{Sex}{\code{"M"} or \code{"F"}.}
#'    \item{Father, Mother}{\code{ID} of each parent; \code{NA} for founders.}
#'    \item{AltFather}{\code{ID} of a rumoured father where the books raise
#'      doubt: Harwin Strong for the three sons of Rhaenyra Targaryen and
#'      Laenor Velaryon, and Aemon the Dragonknight for Daeron II.
#'      \code{NA} otherwise.}
#'    \item{Notes}{Epithets and short notes.}
#'  }
#' @source Compiled by R.J. Dyer from the books of George R.R. Martin:
#'  \emph{A Song of Ice and Fire} (Bantam, 1996--2011), \emph{The World of Ice
#'  & Fire} (with E.M. Garcia Jr. and L. Antonsson; Bantam, 2014) and
#'  \emph{Fire & Blood} (Bantam, 2018).  Parent links were cross-checked
#'  against the father (P22) and mother (P25) statements in Wikidata
#'  (\url{https://www.wikidata.org}, CC0).  Built by
#'  \code{data-raw/targaryen.R} in the package sources.
#'
#'  This is an unofficial compilation for teaching. It is not affiliated with
#'  or endorsed by George R.R. Martin, his publishers, or HBO.  The characters
#'  and their histories remain the property of their author; only the
#'  arrangement of the table is covered by the package licence.
#' @seealso \code{\link{genetic_pedigree}}
#' @examples
#' data( targaryen )
#' head( targaryen )
#' if( requireNamespace("kinship2", quietly = TRUE) ) {
#'   head( genetic_pedigree( targaryen, mode = "table" ) )
#' }
#' @name targaryen
#' @docType data
#' @keywords data
NULL
