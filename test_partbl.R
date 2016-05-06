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

plot(par, c("lod", "hlod"))

vars <- c("lod", "alpha", "hlod")
DF <- partbl2ggdata(par, vars)
p <- DF %>%
  ggplot(aes(x = pos, y = value, colour = variable)) +
  geom_line()
p <- p + facet_grid(variable ~ chr, scales = "free_y") +
  theme(
    panel.margin.x = unit(0.02, "lines"),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_rect(fill = NA, colour = "white", linetype = "dashed"),
    panel.background = element_rect(fill = 'grey95'))
p + coord_cartesian(ylim = c(0, max(DF$value)))



