#' Output Linkage Regions
#'
#' Given a data.frame \code{d} with 'position' and 'value' columns
#' output those contiguous regions where 'value' > \code{t}.
#'
#' For objects of class \code{\link{partbl}}, \code{link{npartbl}},
#' \code{link{famlod}} or \code{link{fampar}}, the main data frame
#' containing the LOD scores per marker is used.
#'
#' @param d An object of class \code{\link{partbl}}, \code{link{npartbl}},
#'        \code{link{famlod}} or \code{link{fampar}}.
#'        Can also be a data.frame with two numeric columns
#'        that make sense as 'position' and 'value'.
#' @param t A length-one numeric vector specifying the threshold to be used.
#' @param valcol A length-one character vector giving the name of the column in
#'        \code{d} containing the values.
#' @param annot A data.frame with chr, cm and bp columns.
#'
#' @return A data.frame where each row contains summary information
#'         about contiguous regions in \code{d} where 'value' >= \code{t}.
#'         If no regions achieve the specified
#'         \code{t}, returns NULL.
#'
#' @examples
#' partbl <- partbl::partbl("merlin_10_famA-parametric.tbl")
#' get_peaks(partbl)
#'
#' @export
get_peaks <- function(d, t = 0, valcol = "lod", annot) {
  if (inherits(d, "partbl")) {
    d <- d$partbl
    split_by <- c("chr")
  } else if (inherits(d, "npartbl")) {
    d <- d$npartbl
    split_by <- c("chr", "analysis")
  } else if (inherits(d, "fampar")) {
    d <- d$fampar
    split_by <- c("chr", "family")
  } else if (inherits(d, "famlod")) {
    d <- d$famlod
    split_by <- c("chr", "analysis", "family")
  }
  stopifnot(is.data.frame(d))
  stopifnot("chr" %in% names(d))
  stopifnot(is.numeric(t), length(t) == 1)
  stopifnot(is.character(valcol), length(valcol) == 1)
  stopifnot(all(c(valcol, "pos") %in% names(d)))
  stopifnot(is.data.frame(annot), nrow(annot) > 1)
  stopifnot(all(names(annot) == c("chr", "bp", "cm")))


  # return a list of dfs where each chunk of consecutive rows has valcol >= t
  get_peaksPerChrom <- function(dchr) {
    stopifnot(is.data.frame(dchr),
              nrow(dchr) > 0,
              !is.unsorted(dchr[["pos"]]))
    achieve_thresh <- dchr[[valcol]] >= t
    if (!any(achieve_thresh)) return(NULL)
    peak_list <- split(dchr[achieve_thresh, ],
                         cumsum(!achieve_thresh)[achieve_thresh])

  }

  peaks <- unlist(plyr::dlply(d, split_by, get_peaksPerChrom),
                  recursive = FALSE, use.names = FALSE)
  peaks_cm <- summarise_peaks(peaks, valcol)
  cm2bp(peaks_cm, annot = annot)
}


#' Summarise LOD score peaks
#'
#' Which regions achieve a LOD score above a given threshold?
#'
#' @param ld A list of data.frames containing markers achieving high LOD scores.
#' @param valcol A length-one character vector giving the name of the column in
#'        \code{d} containing the values.
#'
#' @return A data.frame with chr, start_cm, end_cm, etc.
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' partbl <- partbl::partbl("merlin_10_famA-parametric.tbl")
#' list_of_peaks <- get_peaks(partbl)
#' summarise_peaks(list_of_peaks)
#'
summarise_peaks <- function(ld, valcol) {
  stopifnot(is.list(ld))
  peak_summary <- lapply(ld, function(df) {
    n <- nrow(df)
    chr <- df$chr[1]
    start_cm <- df$pos[1]
    end_cm <- df$pos[n]
    max_val <- round(max(df[[valcol]]), 2)
    min_val <- round(min(df[[valcol]]), 2)
    analysis <- ifelse("analysis" %in% names(df),
                       paste0("npara_", df$analysis[1]),
                       "para")
    family <- ifelse("family" %in% names(df),
                     df$family[1],
                     "-")

    data.frame(fam = family, analysis = analysis,
               chr = chr, start_cm = start_cm, end_cm = end_cm,
               tot_cm = "-",
               val = valcol, max_val = max_val, min_val = min_val,
               stringsAsFactors = FALSE)
  })
  peaks <- dplyr::bind_rows(peak_summary) %>%
    dplyr::arrange_(~fam, ~analysis, ~chr, ~start_cm, ~end_cm)


  e <- 0.00001
  peaks$start_cm <- peaks$start_cm - (ifelse(peaks$start_cm - 0.0 < e, 0, 0.3))
  peaks$end_cm <- peaks$end_cm + 0.3
  peaks$tot_cm <- peaks$end_cm - peaks$start_cm
  peaks

}

#' Convert centimorgan to bp
#'
#' Converts genetic distance in centimorgans to genetic position in bp.
#'
#' @param peaks A data.frame with chr, start_cm and end_cm information.
#' @param annot A data.frame with chr, cm and bp columns.
#'
#' @return The peaks data.frame with the start_bp and end_bp columns inserted.
#'
#' @examples
#' par <- partbl(list.files(pattern = "-parametric.tbl$")
#' peaks <- peaks(par)
#' annot <- read_annot("annotHapMap2U.txt")
#' cm2bp(peaks, annot)
#'
#' @export
cm2bp <- function(peaks, annot) {
  stopifnot(is.data.frame(peaks), nrow(peaks) > 0)
  stopifnot(is.data.frame(annot), nrow(annot) > 0)
  stopifnot(all(names(annot) == c("chr", "bp", "cm")))
  stopifnot(all(c("chr", "start_cm", "end_cm") %in% names(peaks)))
  cm2bp_chr <- function(peaks_chr, annot){
    chr <- unique(peaks_chr$chr)
    stopifnot(length(chr) == 1, chr %in% 1:23)
    chr <- paste0("chr", chr)
    # subset annot based on chr
    annot <- annot[annot$chr == chr, ]

    start_bp <- c()
    end_bp <- c()
    min_diffs_start <- c()
    min_diffs_end <- c()
    peaks_start_cm <- peaks_chr$start_cm
    peaks_end_cm <- peaks_chr$end_cm

    # specify UCSC hg19 chromosome lengths for chr1-chr23
    chr_len <- setNames(c(249250621, 243199373, 198022430, 191154276, 180915260,
                          171115067, 159138663, 146364022, 141213431, 135534747,
                          135006516, 133851895, 115169878, 107349540, 102531392,
                          90354753, 81195210, 78077248, 59128983, 63025520,
                          48129895, 51304566, 155270560),
                        paste0("chr", seq_len(23)))


    for (pcm in peaks_start_cm) {
      diffs <- annot$cm - pcm
      # difference must be 0 or negative
      m <- max(diffs[diffs <= 0])
      idx <- which(diffs == m)
      dat <- annot[idx, ]
      if (nrow(dat) == 0) { # start of chr, return 1
        start_bp <- c(start_bp, 1)
      } else if (nrow(dat) == 1) { # only one closest match, return it
        start_bp <- c(start_bp, dat$bp)
      } else if (nrow(dat) > 1) { # multiple markers have the closest cM position
        if (m == 0) {
          # multiple exact cM matches, return match with lowest bp
          start_bp <- c(start_bp, dat$bp[1])
        } else {
          # multiple closest markers with smaller cM values, so
          # return one with highest bp
          start_bp <- c(start_bp, dat$bp[nrow(dat)])
        }
      } else {
        stop("This should never happen!!")
      }
      min_diffs_start <- c(min_diffs_start, m)
    } # for end

    for (pcm in peaks_end_cm) {
      diffs <- annot$cm - pcm
      # difference must be 0 or positive
      m <- min(diffs[diffs >= 0])
      idx <- which(diffs == m)
      dat <- annot[idx, ]
      if (nrow(dat) == 0) { # end of chr, return chr_len
        end_bp <- c(end_bp, chr_len[chr])
      } else if (nrow(dat) == 1) { # only one closest match, return it
        end_bp <- c(end_bp, dat$bp)
      } else if (nrow(dat) > 1) { # multiple markers have the closest cM position
        if (m == 0) {
          # multiple exact cM matches, return match with highest bp
          end_bp <- c(end_bp, dat$bp[nrow])
        } else {
          # multiple closest markers with higher cM values, so
          # return one with lowest bp
          end_bp <- c(end_bp, dat$bp[1])
        }
      } else {
        stop("This should never happen!!")
      }
      min_diffs_end <- c(min_diffs_end, m)
    } # for end
    cbind(peaks_chr, start_bp = start_bp, end_bp = end_bp, tot_bp = end_bp - start_bp, min_diffs_start, min_diffs_end)
  } # inner func end
  plyr::ddply(peaks, "chr", cm2bp_chr, annot)

} # function end
