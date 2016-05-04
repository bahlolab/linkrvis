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
par <- linkrvis::partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
methods(class = "partbl")
print(par)
summary(par)

plot(par)
plot(par, vars = c("lod"))
plot(par, vars = c("hlod"))
vars = c("lod", "alpha", "hlod")
DF <- par$partbl %>%
  dplyr::select_( ~ (one_of(c("pos", vars)))) %>%
  tidyr::gather_("variable", "value", vars)

p <- DF %>% ggplot(aes(x = pos, y = value, colour = variable))
p +
  geom_line() +
  theme_bw() +
  coord_cartesian(xlim = par$pos_range, ylim = c(0, NA)) +
  facet_wrap( ~ variable, ncol = 1, scales = "free_y")
p


