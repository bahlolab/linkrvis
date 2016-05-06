#' Plot MERLIN Parametric LOD Scores
#'
#' Plots the LOD, ALPHA and HLOD scores in MERLIN's \code{famA-parametric.tbl} file
#'
#' @param partbl An object of class \code{partbl}.
#' @param vars A character vector indicating which variable(s) to plot. Available
#'        variables from the \code{partbl} file are: "lod", "alpha" and "hlod".
#' @param threshold Numeric of length one. Only plot values above or equal to this.
#'
#' @return A ggplot object, plotting the selected variables
#'
#' @importFrom ggplot2 ggplot geom_line theme facet_grid coord_cartesian
#' @importFrom dplyr %>%
#'
#' @examples
#' pt <- partbl("foo-parametric.tbl")
#' plot(pt)
#'
#' @export
plot.partbl <- function(partbl, vars = c("lod", "alpha", "hlod")) {
  stopifnot(inherits(partbl, "partbl"))
  DF <- partbl2ggdata(partbl, vars)
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

}



#' \code{partbl} GG Data Preparation
#'
#' @param partbl An object of class \code{partbl} or a list of \code{partbl} objects.
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
