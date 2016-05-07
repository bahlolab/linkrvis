rm(list = ls())
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"

# Single
par <- linkrvis::partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
par
summary(par)
plot(par, vars = c("lod", "hlod"))
plot(par, vars = c("alpha", "lod", "hlod"), threshold = 5)


# Multi
par_fnames <- list.files(data_path, pattern = "-parametric.tbl", full.names = TRUE)
par <- linkrvis::partbl(par_fnames)
par

summary(par)
plot(par, c("lod", "hlod"))
plot(par, c("alpha"), c(0, 1))



