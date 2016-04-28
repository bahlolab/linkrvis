## First try joint
# dat <- linkrvis::read_merlin_dat(file.path(data_path, "merlin_10_c10orf_extended.dat"))
# map <- linkrvis::read_merlin_map(file.path(data_path, "merlin_10_c10orf_extended.map"))
# ped <- linkrvis::read_merlin_ped(file.path(data_path, "merlin_10_c10orf_extended.ped"))
# par <- linkrvis::read_merlin_partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
# npar <- linkrvis::read_merlin_npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))

## npartbl
npar <- linkrvis::npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
methods(class = "npartbl")
print(npar)
summary(npar)


## partbl
rm(list = ls())
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
partbl <- linkrvis::partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))

### Plotting
require(ggplot2)
require(dplyr)
require(tidyr)

plot_partbl <- function(partbl, variables = c("lod", "alpha", "hlod"))
partbl$partbl %>%
  dplyr::select(pos, c("lod", "alpha", "hlod"))
  dplyr::select(pos, lod, alpha, hlod)
  tidyr::gather(variable, value, lod:hlod) %>%
  dplyr::filter(value > 0) %>%
  ggplot(aes(x = pos, y = value, colour = variable)) +
  geom_line() +
  facet_wrap( ~ variable, ncol = 1, scales = "free_y")


ggplot(data = DF, aes(x = pos, y = value, colour = key)) +
  geom_line() +

object.size(DF)
object.size(partbl$partbl)

