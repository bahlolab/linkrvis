#' Plot MERLIN Nonparametric LOD Scores
#'
#' Plots the LOD and ExLOD scores in MERLIN's \code{nonparametric.tbl} file
#'
#' @param npartbl S3 object of class \code{npartbl}.
#' @param vars Character vector indicating which variable(s) to plot. Available
#'        variables from the \code{npartbl} data.frame are: "lod" and "exlod".
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
#' npt <- npartbl("foo-nonparametric.tbl")
#' plot(npt)
#'
#' @export
plot.npartbl <- function(npartbl, vars = c("lod", "exlod"),
                        ylim = c(0, max(npartbl$max_lods[c("max_lod", "max_exlod")]))) {
  stopifnot(inherits(npartbl, "npartbl"))
  stopifnot(vars %in% c("lod", "exlod"))
  stopifnot(is.numeric(ylim), length(ylim) == 2)

  DF <- npartbl2ggdata(npartbl, vars)
  p <- DF %>%
    ggplot(ggplot2::aes_(x = ~pos, y = ~value, colour = ~variable)) +
    geom_line()
  p <- p + facet_grid(analysis ~ chr, scales = "free_y") +
    theme(
      panel.margin.x = ggplot2::unit(0.02, "lines"),
      # strip.background = ggplot2::element_blank(),
      # strip.text.y = ggplot2::element_blank(),
      # axis.ticks = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      # axis.text.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, colour = "white", linetype = "dashed"),
      panel.background = ggplot2::element_rect(fill = 'grey95'))
  p + coord_cartesian(ylim = ylim)

}



#' \code{npartbl} GG Data Preparation
#'
#' @param npartbl S3 object of class \code{partbl}.
#' @param vars A character vector indicating which variable(s) to plot. Available
#'        variables from the \code{partbl} file are: "lod", "alpha" and "hlod".
#'
#' @return A data.frame with three columns: pos (num), analysis (chr),
#'         variable (chr) and value (num).
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' npt <- npartbl("foo-nparametric.tbl")
#' npt_ggdata <- npartbl2ggdata(pt)
#' head(npt_ggdata)
#'
npartbl2ggdata <- function(npartbl, vars = c("lod", "exlod")) {
  stopifnot(inherits(npartbl, c("npartbl")),
            vars %in% c("lod", "exlod"))

  if ("exlod" %in% vars && npartbl$merlin_options["exp"] == FALSE) {
    vars <- "lod"
  }

  npartbl$npartbl %>%
    tidyr::gather_("variable", "value", vars)
}
