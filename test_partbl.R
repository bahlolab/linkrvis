rm(list = ls())
data_path <- "~/Desktop/c10orf2/merlin_output/joint_merlin"

# Single
par <- linkrvis::partbl(file.path(data_path, "merlin_10_c10orf_extended-parametric.tbl"))
par
summary(par)

# Multi
par_fnames <- list.files(data_path, pattern = "-parametric.tbl", full.names = TRUE)
par <- linkrvis::partbl(par_fnames)

summary(par)

vars <- c("lod", "alpha", "hlod")
DF <- partbl2ggdata(par, vars)
p <- DF %>%
  ggplot(aes(x = pos, y = value, colour = variable)) +
  geom_line() +
  theme_bw()
p + facet_grid(variable ~ chr, scales = "free_y") +
  theme(panel.margin.x = unit(0, "lines"))
  # ggtitle(paste0("Chromosome ", partbl$chrom))
p
