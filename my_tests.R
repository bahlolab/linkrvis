## First try joint
# dat <- linkrvis::read_merlin_dat(file.path(data_path, "merlin_10_c10orf_extended.dat"))
# map <- linkrvis::read_merlin_map(file.path(data_path, "merlin_10_c10orf_extended.map"))
# ped <- linkrvis::read_merlin_ped(file.path(data_path, "merlin_10_c10orf_extended.ped"))
# par <- linkrvis::read_merlin_partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
# npar <- linkrvis::read_merlin_npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))

## npartbl
# npar <- linkrvis::npartbl(file.path(data_path, "merlin_10_c10orf_extended-nonparametric.tbl"))
# methods(class = "npartbl")
# print(npar)
# summary(npar)


## partbl
rm(list = ls())
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
partbl <- linkrvis::partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))

### Plotting
require(ggplot2)
require(tidyr)
require(dplyr)


g_data <- function(df, vars = c("lod", "alpha", "hlod")) {
  dplyr::select_(df, lazyeval::interp(~one_of(x), x = c("pos", vars))) %>%
    tidyr::gather_("variable", "value", vars) %>%
    dplyr::filter_(lazyeval::interp(~x > 0, x = as.name("value")))
}

DF <- g_data(partbl$partbl)
DF %>%  ggplot(aes(x = pos, y = value, colour = variable)) +
  geom_line() +
  theme_bw() +
  facet_wrap( ~ variable, ncol = 1, scales = "free_y")


