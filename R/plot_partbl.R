#' Plot MERLIN Parametric Joint LOD Scores
#'
#' Plots the LOD, ALPHA and HLOD scores in MERLIN's \code{parametric.tbl} file
#'
#' @param partbl S3 object of class \code{partbl}.
#' @param vars Character vector indicating which variable(s) to plot. Available
#'        variables from the \code{partbl} data.frame are: "lod", "alpha" and "hlod".
#' @param ylim Character vector of length 2 indicating the range of values to plot.
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
#' pt <- partbl("foo-parametric.tbl")
#' plot(pt)
#'
#' @export
plot.partbl <- function(partbl, vars = c("lod", "alpha", "hlod"),
                        ylim = c(0, max(partbl$max_lods[c("max_lod", "max_hlod")]))) {
  stopifnot(inherits(partbl, "partbl"))
  stopifnot(vars %in% c("lod", "alpha", "hlod"))
  stopifnot(is.numeric(ylim), length(ylim) == 2)
  DF <- partbl2ggdata(partbl, vars)
  p <- DF %>%
    ggplot(ggplot2::aes_(x = ~pos, y = ~value, colour = ~variable)) +
    geom_line()
  p <- p + facet_grid(variable ~ chr, scales = "free_y") +
    theme(
      panel.margin.x = ggplot2::unit(0.02, "lines"),
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, colour = "white", linetype = "dashed"),
      panel.background = ggplot2::element_rect(fill = 'grey95'))
  p + coord_cartesian(ylim = ylim)

}



#' \code{partbl} GG Data Preparation
#'
#' @param partbl S3 object of class \code{partbl}.
#' @param vars A character vector indicating which variable(s) to plot. Available
#'        variables from the \code{partbl} file are: "lod", "alpha" and "hlod".
#'
#' @return A data.frame with three columns: pos (num), variable (chr) and value (num).
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' pt <- partbl("foo-parametric.tbl")
#' pt_ggdata <- partbl2ggdata(pt)
#' head(pt_ggdata)
#'
#' @export
partbl2ggdata <- function(partbl, vars = c("lod", "alpha", "hlod")) {
  stopifnot(inherits(partbl, c("partbl")),
            vars %in% c("lod", "alpha", "hlod"))

  partbl$partbl %>%
    dplyr::select_( ~ (one_of(c("chr", "pos", vars)))) %>%
    tidyr::gather_("variable", "value", vars)
}
