#' Plot MERLIN PerFam Nonparametric LOD Scores
#'
#' Plots the LOD scores in MERLIN's \code{fam.lod} file
#'
#' @param famlod S3 object of class \code{famlod}.
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
#' fl <- famlod("merlin_10_famA.lod")
#' plot(fl)
#'
#' @export
plot.famlod <- function(famlod,
                        ylim = c(0, max(famlod$max_lods[c("max_lod", "max_plod")]))) {
  stopifnot(inherits(famlod, "famlod"))
  stopifnot(is.numeric(ylim), length(ylim) == 2)
  DF <- famlod2ggdata(famlod)
  p <- DF %>%
    ggplot(ggplot2::aes_(x = ~pos, y = ~value, colour = ~analysis, linetype = ~variable)) +
    geom_line()
  p <- p + facet_grid(family ~ chr, scales = "free_y") +
    theme(
      panel.margin.x = ggplot2::unit(0.02, "lines"),
      # strip.background = ggplot2::element_blank(),
      # strip.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, colour = "white", linetype = "dashed"),
      panel.background = ggplot2::element_rect(fill = 'grey95'))
  p + coord_cartesian(ylim = ylim)

}

#' \code{famlod} GG Data Preparation
#'
#' @param famlod S3 object of class \code{famlod}.
#'
#' @return A data.frame with 6 columns: chr (num), pos (num), family (chr),
#'         analysis (chr), variable (chr) and value (num).
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' fl <- famlod("foo.lod")
#' fl_ggdata <- famlod2ggdata(pt)
#' head(fl_ggdata)
#'
famlod2ggdata <- function(famlod) {
  stopifnot(inherits(famlod, "famlod"))
  famlod$famlod %>%
    tidyr::gather_("variable", "value", c("lod", "plod"))
}