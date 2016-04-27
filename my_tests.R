## First try joint


par <- linkrvis::read_merlin_partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
dat <- linkrvis::read_merlin_dat(file.path(data_path, "merlin_10_c10orf_extended.dat"))
map <- linkrvis::read_merlin_map(file.path(data_path, "merlin_10_c10orf_extended.map"))
ped <- linkrvis::read_merlin_ped(file.path(data_path, "merlin_10_c10orf_extended.ped"))
# npar <- linkrvis::read_merlin_npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))

data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
npar <- linkrvis::npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))

print(npar)
summary(npar)
