rm(list = ls())
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
par <- linkrvis::partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
methods(class = "partbl")
print(par)
summary(par)

plot(par)
plot(par, vars = c("lod"))
plot(par, vars = c("hlod"))
plot(par, vars = c("alpha"))

set.seed(123)
(multipar <- sample(list.files(data_path, pattern = "-parametric.tbl", full.names = TRUE), 5))


partbl_list <- lapply(multipar, linkrvis::partbl)
partbl_list <- lapply(partbl_list, "[[", "partbl")
DF <- dplyr::bind_rows(partbl_list)
vars <- c("lod", "alpha", "hlod")

DF %>%
  dplyr::select_( ~ (one_of(c("chr", "pos", vars)))) %>%
  tidyr::gather_("variable", "value", vars)
