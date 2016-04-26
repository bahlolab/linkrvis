data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"

par <- linkrvis::read_merlin_partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
npar <- linkrvis::read_merlin_npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
dat <- linkrvis::read_merlin_dat(file.path(data_path, "merlin_10_c10orf_extended.dat"))
map <- linkrvis::read_merlin_map(file.path(data_path, "merlin_10_c10orf_extended.map"))
ped <- linkrvis::read_merlin_ped(file.path(data_path, "merlin_10_c10orf_extended.ped"))

str(npar)
str(par)
str(dat)
str(map)
str(ped)

npartbl <- function(npartbl, ...) {
  chrom <- get_chrom(npartbl)
  max_lod <- get_max_lod(npartbl)
  tab <- list(lods = npartbl$LOD,
              snpname = npartbl$MARKER,
              max_lod = max_lod,
              chrom = chrom)
  class(tab) <- "npartbl"
  tab
}