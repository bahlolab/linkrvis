## First try joint


par <- linkrvis::read_merlin_partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
dat <- linkrvis::read_merlin_dat(file.path(data_path, "merlin_10_c10orf_extended.dat"))
map <- linkrvis::read_merlin_map(file.path(data_path, "merlin_10_c10orf_extended.map"))
ped <- linkrvis::read_merlin_ped(file.path(data_path, "merlin_10_c10orf_extended.ped"))

data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
npar <- linkrvis::read_merlin_npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
npar2 <- linkrvis::npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
str(npar)
str(npar2)
str(par)
str(dat)
str(map)
str(ped)

print.npartbl <- function(npartbl) {
  cat("MERLIN parametric tbl with:\n",
      "n_markers: ", npartbl$n_markers, "\n",
      "analysis: ", paste(npartbl$analysis, collapse = " and "), "\n",
      "chrom: ", npartbl$chrom, "\n",
      "max_lods:\n", sep = "")
  print(npartbl$max_lods)
}

print.npartbl(npar2)
