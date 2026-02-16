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
#' @importFrom sampling strata
#' @importFrom methods is
#' @importFrom stats pchisq cor.test cov prcomp qchisq var
#' @importFrom utils read.csv read.table write.csv
#' @importFrom sp Lines Line SpatialPoints SpatialLines bbox
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

#' Sonoran desert altitude.
#'
#' This is a raster file for altitude in the Sonoran desert
#'  region coincident with the Lophocereus and Upiga data
#'  sets.
#' @name alt
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
#' This is the population graph for the Lophocereus data
#'  that is discussed in Dyer & Nason (2004).
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






















