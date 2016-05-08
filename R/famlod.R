read_merlin_lod <- function(fname) {
  npar <- read.table("~/Desktop/C10orf2/merlin_output/joint_merlin/merlin_10_c10orf_extended.lod",
                     header = FALSE, stringsAsFactors = FALSE,
                     row.names = NULL, sep = "", skip = 1,
                     col.names = c( "FAMILY", "DISEASE", "TRAIT", "LOCATION",
                                    "ZSCORE", "pLOD", "DELTA", "LOD"))

}

