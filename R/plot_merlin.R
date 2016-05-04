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
#' @importFrom ggplot2 ggplot geom_line theme_bw facet_wrap
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
    geom_line() +
    theme_bw() +
    facet_wrap( ~ variable, ncol = 1, scales = "free_y")
  p

}



#' \code{partbl} GG Data Preparation
#'
#' @param partbl An object of class \code{partbl}.
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
  stopifnot(inherits(partbl, "partbl"),
            vars %in% c("lod", "alpha", "hlod"))


  partbl$partbl %>%
    dplyr::select_( ~ (one_of(c("pos", vars)))) %>%
    tidyr::gather_("variable", "value", vars)
}

