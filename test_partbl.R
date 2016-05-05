rm(list = ls())
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"
methods(class = "partbl")

par <- linkrvis::partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
DF <- partbl2ggdata(par)
str(DF)
set.seed(123)
(multipar <- sample(list.files(data_path, pattern = "-parametric.tbl", full.names = TRUE), 5))
partbl_list <- lapply(multipar, linkrvis::partbl)
DF2 <- partbl2ggdata(partbl_list)
str(DF2)

DF_list <- lapply(partbl_list, "[[", "partbl")
vars <- c("lod", "alpha", "hlod")
