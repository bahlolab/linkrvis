#' Plot MERLIN PerFam Parametric LOD Scores
#'
#' Plots the LOD scores in MERLIN's \code{fam.par} file
#'
#' @param fampar S3 object of class \code{fampar}.
#' @param ylim Character vector of length 2 indicating the range of values to plot.
#'
#' @return ggplot object, plotting the selected variables
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 element_blank
#' @importFrom dplyr %>%
#'
#' @examples
#' fp <- fampar("merlin_10_famA.par")
#' plot(fp)
#'
#' @export
plot.fampar <- function(fampar,
                        ylim = c(0, max(fampar$max_lods$max_lod))) {
  stopifnot(inherits(fampar, "fampar"))
  stopifnot(is.numeric(ylim), length(ylim) == 2)
  p <- fampar$fampar %>%
    ggplot(ggplot2::aes_(x = ~pos, y = ~lod, colour = ~family)) +
    geom_line()
  p <- p + facet_grid(family ~ chr, scales = "free_y") +
    theme(
      panel.margin.x = ggplot2::unit(0.02, "lines"),
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      # axis.ticks = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      # axis.text.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, colour = "white", linetype = "dashed"),
      panel.background = ggplot2::element_rect(fill = 'grey95'))
  p + coord_cartesian(ylim = ylim)

}
