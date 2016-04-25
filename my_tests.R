data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"

par <- linkrvis::read_merlin_partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
npar <- linkrvis::read_merlin_npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
dat <- linkrvis::read_merlin_dat(file.path(data_path, "merlin_10_c10orf_extended.dat"))
map <- linkrvis::read_merlin_map(file.path(data_path, "merlin_10_c10orf_extended.map"))
ped <- read.table(file.path(data_path, "merlin_10_c10orf_extended.ped"),
                  header = FALSE, stringsAsFactors = FALSE)

str(npar)
str(par)
str(dat)
str(map)
str(ped)
