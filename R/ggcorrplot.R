#' Visualization of a correlation matrix using ggplot2
#' @import ggplot2
#' @description \itemize{ \item ggcorrplot(): A graphical display of a
#'   correlation matrix using ggplot2. \item cor_pmat(): Compute a correlation
#'   matrix p-values. }
#' @param corr the correlation matrix to visualize
#' @param method character, the visualization method of correlation matrix to be
#'   used. Allowed values are "square" (default), "circle".
#' @param type character, "full" (default), "lower" or "upper" display.
#' @param ggtheme ggplot2 function or theme object. Default value is
#'   `theme_minimal`. Allowed values are the official ggplot2 themes including
#'   theme_gray, theme_bw, theme_minimal, theme_classic, theme_void, .... Theme
#'   objects are also allowed (e.g., `theme_classic()`).
#' @param title character, title of the graph.
#' @param show.legend logical, if TRUE the legend is displayed.
#' @param legend.title a character string for the legend title. lower
#'   triangular, upper triangular or full matrix.
#' @param show.diag NULL or logical, whether display the correlation
#'   coefficients on the principal diagonal. If \code{NULL}, the default is to
#'   show diagonal correlation for \code{type = "full"} and to remove it when
#'   \code{type} is one of "upper" or "lower".
#' @param colors a vector of colors for the fill gradient. The default is a
#'   length-3 vector for the low, mid and high correlation values (mapped with
#'   \code{\link[ggplot2]{scale_fill_gradient2}}). A vector of any other length
#'   (\code{>= 2}) is spread evenly across the scale with
#'   \code{\link[ggplot2]{scale_fill_gradientn}}, so an n-color palette (e.g.
#'   \code{RColorBrewer::brewer.pal(11, "RdBu")}) can be passed directly.
#' @param outline.color the outline color of square or circle. Default value is
#'   "gray".
#' @param hc.order logical value. If TRUE, correlation matrix will be hc.ordered
#'   using hclust function.
#' @param hc.method the agglomeration method to be used in hclust (see ?hclust).
#' @param lab logical value. If TRUE, add correlation coefficient on the plot.
#' @param lab_col,lab_size size and color to be used for the correlation
#'   coefficient labels. used when lab = TRUE.
#' @param lab_fontface the font face (\code{"plain"}, \code{"bold"},
#'   \code{"italic"}, \code{"bold.italic"}) for the correlation coefficient
#'   labels. Default is \code{"plain"}. Used when \code{lab = TRUE}.
#' @param sig.stars logical value. If \code{TRUE} and a \code{p.mat} is
#'   supplied, significance stars are appended to the coefficient labels
#'   (\code{***} for p < 0.001, \code{**} for p < 0.01, \code{*} for p < 0.05),
#'   e.g. \code{"-0.85**"}. Only used when \code{lab = TRUE}. Default is
#'   \code{FALSE}. When \code{TRUE}, significance is shown by the stars and the
#'   \code{insig = "pch"} markers are not drawn.
#' @param p.mat matrix of p-value. If NULL, arguments sig.level, insig, pch,
#'   pch.col, pch.cex is invalid.
#' @param sig.level significant level, if the p-value in p-mat is bigger than
#'   sig.level, then the corresponding correlation coefficient is regarded as
#'   insignificant.
#' @param insig character, specialized insignificant correlation coefficients,
#'   "pch" (default), "blank". If "blank", wipe away the corresponding glyphs;
#'   if "pch", add characters (see pch for details) on corresponding glyphs.
#' @param pch add character on the glyphs of insignificant correlation
#'   coefficients (only valid when insig is "pch"). Default value is 4.
#' @param pch.col,pch.cex the color and the cex (size) of pch (only valid when
#'   insig is "pch").
#' @param tl.cex,tl.col,tl.srt the size, the color and the string rotation of
#'   text label (variable names). \code{tl.col} defaults to \code{NULL}, which
#'   inherits the color from the theme.
#' @param tl.vjust,tl.hjust the vertical and horizontal justification of the
#'   x-axis text labels, passed to \code{\link[ggplot2]{element_text}}. Both
#'   default to \code{1}; adjust them to reposition the variable-name labels.
#' @param digits Decides the number of decimal digits to be displayed (Default:
#'   `2`).
#' @param as.is A logical passed to \code{\link[reshape2]{melt.array}}. If
#'   \code{TRUE}, dimnames will be left as strings instead of being converted
#'   using \code{\link[utils]{type.convert}}.
#' @param nsmall the minimum number of digits to the right of the decimal point
#'   in the coefficient labels, passed to \code{\link[base]{format}}. Default is
#'   \code{0} (no minimum, current behavior). Set e.g. \code{nsmall = 2} to keep
#'   trailing zeros (such as 0.70). Only used when \code{lab = TRUE}.
#' @param leading.zero logical. If \code{TRUE} (default), coefficient labels keep
#'   the leading zero (e.g. \code{0.23}, \code{-0.67}). Set to \code{FALSE} to
#'   drop it (\code{.23}, \code{-.67}), which is common for correlation tables.
#'   Only used when \code{lab = TRUE}.
#' @param legend.limit a length-2 numeric vector giving the limits of the fill
#'   color scale. Default \code{c(-1, 1)} (suitable for a correlation matrix); set
#'   to \code{NULL} to use the data range instead, e.g. for a covariance matrix.
#' @param circle.scale a scaling factor for the circle sizes when
#'   \code{method = "circle"}. Default is \code{1}; increase it (e.g.
#'   \code{circle.scale = 2}) for larger circles or decrease it for smaller ones,
#'   which is useful when the output device size makes the default circles too
#'   small or too large. Has no effect when \code{method = "square"}.
#' @param coord.fixed logical value. If \code{TRUE} (default), the plot uses
#'   \code{\link[ggplot2]{coord_fixed}} so the cells are square. Set to
#'   \code{FALSE} to let the cells fill the plotting area (a non 1:1 aspect
#'   ratio), which can look better with many long variable names.
#' @return \itemize{ \item ggcorrplot(): Returns a ggplot2 \item cor_pmat():
#' Returns a matrix containing the p-values of correlations }
#' @examples
#' # Compute a correlation matrix
#' data(mtcars)
#' corr <- round(cor(mtcars), 1)
#' corr
#'
#' # Compute a matrix of correlation p-values
#' p.mat <- cor_pmat(mtcars)
#' p.mat
#'
#' # Visualize the correlation matrix
#' # --------------------------------
#' # method = "square" or "circle"
#' ggcorrplot(corr)
#' ggcorrplot(corr, method = "circle")
#'
#' # Reordering the correlation matrix
#' # --------------------------------
#' # using hierarchical clustering
#' ggcorrplot(corr, hc.order = TRUE, outline.color = "white")
#'
#' # Types of correlogram layout
#' # --------------------------------
#' # Get the lower triangle
#' ggcorrplot(corr,
#'   hc.order = TRUE, type = "lower",
#'   outline.color = "white"
#' )
#' # Get the upeper triangle
#' ggcorrplot(corr,
#'   hc.order = TRUE, type = "upper",
#'   outline.color = "white"
#' )
#'
#' # Change colors and theme
#' # --------------------------------
#' # Argument colors
#' ggcorrplot(corr,
#'   hc.order = TRUE, type = "lower",
#'   outline.color = "white",
#'   ggtheme = ggplot2::theme_gray,
#'   colors = c("#6D9EC1", "white", "#E46726")
#' )
#'
#' # Add correlation coefficients
#' # --------------------------------
#' # argument lab = TRUE
#' ggcorrplot(corr,
#'   hc.order = TRUE, type = "lower",
#'   lab = TRUE,
#'   ggtheme = ggplot2::theme_dark(),
#' )
#'
#' # Add correlation significance level
#' # --------------------------------
#' # Argument p.mat
#' # Barring the no significant coefficient
#' ggcorrplot(corr,
#'   hc.order = TRUE,
#'   type = "lower", p.mat = p.mat
#' )
#' # Leave blank on no significant coefficient
#' ggcorrplot(corr,
#'   p.mat = p.mat, hc.order = TRUE,
#'   type = "lower", insig = "blank"
#' )
#'
#' # Changing number of digits for correlation coeffcient
#' # --------------------------------
#' ggcorrplot(cor(mtcars),
#'   type = "lower",
#'   insig = "blank",
#'   lab = TRUE,
#'   digits = 3
#' )
#' @name ggcorrplot
#' @rdname ggcorrplot
#' @export

# function body
ggcorrplot <- function(corr,
                       method = c("square", "circle"),
                       type = c("full", "lower", "upper"),
                       ggtheme = ggplot2::theme_minimal,
                       title = "",
                       show.legend = TRUE,
                       legend.title = "Corr",
                       show.diag = NULL,
                       colors = c("blue", "white", "red"),
                       outline.color = "gray",
                       hc.order = FALSE,
                       hc.method = "complete",
                       lab = FALSE,
                       lab_col = "black",
                       lab_size = 4,
                       lab_fontface = "plain",
                       sig.stars = FALSE,
                       p.mat = NULL,
                       sig.level = 0.05,
                       insig = c("pch", "blank"),
                       pch = 4,
                       pch.col = "black",
                       pch.cex = 5,
                       tl.cex = 12,
                       tl.col = NULL,
                       tl.srt = 45,
                       tl.vjust = 1,
                       tl.hjust = 1,
                       digits = 2,
                       as.is = FALSE,
                       nsmall = 0L,
                       leading.zero = TRUE,
                       legend.limit = c(-1, 1),
                       circle.scale = 1,
                       coord.fixed = TRUE) {
  type <- match.arg(type)
  method <- match.arg(method)
  insig <- match.arg(insig)
  if (is.null(show.diag)) {
    if (type == "full") {
      show.diag <- TRUE
    } else {
      show.diag <- FALSE
    }
  }

  if (inherits(corr, "cor_mat")) {
    # cor_mat object from rstatix
    cor.mat <- corr
    corr <- .tibble_to_matrix(cor.mat)
    p.mat <- .tibble_to_matrix(attr(cor.mat, "pvalue"))
  }

  if (!is.matrix(corr) & !is.data.frame(corr)) {
    stop("Need a matrix or data frame!")
  }
  corr <- as.matrix(corr)

  # Transform the correlation (and matching p-value) matrix into the long
  # data frames the plot is built from: reorder, round, mask a triangle, melt,
  # and join per-cell significance. Extracted so the mixed-method path can reuse
  # exactly the same pipeline (P0.0). Returns the melted `corr` data frame and
  # the insignificant-cells `p.mat` data frame (or NULL).
  built <- .build_corr_df(
    corr = corr, p.mat = p.mat, type = type, show.diag = show.diag,
    hc.order = hc.order, hc.method = hc.method, digits = digits,
    sig.level = sig.level, insig = insig, as.is = as.is
  )
  corr <- built$corr
  p.mat <- built$p.mat

  # heatmap
  p <-
    ggplot2::ggplot(
      data = corr,
      mapping = ggplot2::aes(x = .data[["Var1"]], y = .data[["Var2"]], fill = .data[["value"]])
    )

  # modification based on method (extracted so the mixed-method path can request
  # a different glyph per triangle from the same builder, P0.0)
  p <- p + .method_layer(method, outline.color = outline.color, circle.scale = circle.scale)

  # adding colors
  if (length(colors) < 2) {
    stop("'colors' must contain at least 2 colors.", call. = FALSE)
  }
  if (length(colors) == 3) {
    p <- p + ggplot2::scale_fill_gradient2(
      low = colors[1],
      high = colors[3],
      mid = colors[2],
      midpoint = 0,
      limit = legend.limit,
      space = "Lab",
      name = legend.title
    )
  } else {
    # any palette that is not exactly low/mid/high: spread the colors evenly
    # across the scale with gradientn. Lets users pass e.g. an 11-color
    # RColorBrewer palette directly, without adding a second fill scale (#52).
    p <- p + ggplot2::scale_fill_gradientn(
      colours = colors,
      limits = legend.limit,
      name = legend.title
    )
  }

  # depending on the class of the object, add the specified theme
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme()
  } else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }


  p <- p +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = tl.srt,
        vjust = tl.vjust,
        size = tl.cex,
        hjust = tl.hjust,
        colour = tl.col
      ),
      axis.text.y = ggplot2::element_text(size = tl.cex, colour = tl.col)
    )
  if (coord.fixed) {
    p <- p + ggplot2::coord_fixed()
  } else {
    p <- p + ggplot2::coord_cartesian()
  }

  label <- round(x = corr[, "value"], digits = digits)
  if (nsmall > 0) label <- format(label, nsmall = nsmall, trim = TRUE)
  if (!leading.zero) {
    # drop the leading zero of values in (-1, 1), e.g. 0.23 -> .23, -0.67 -> -.67
    # (idiom from @PawelKulawiak, #15). The \\b keeps values like 1.00 untouched.
    label <- gsub("\\b0(\\.\\d+)", "\\1", label)
  }
  if (sig.stars && !is.null(p.mat)) {
    stars <- as.character(cut(corr$pvalue,
      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
      labels = c("***", "**", "*", "")
    ))
    stars[is.na(stars)] <- ""
    label <- paste0(label, stars)
  }
  if (!is.null(p.mat) & insig == "blank") {
    ns <- corr$pvalue > sig.level
    ns[is.na(ns)] <- FALSE
    if (sum(ns) > 0) label[ns] <- " "
  }

  # matrix cell labels
  if (lab) {
    p <- p +
      ggplot2::geom_text(
        mapping = ggplot2::aes(x = .data[["Var1"]], y = .data[["Var2"]]),
        label = label,
        color = lab_col,
        size = lab_size,
        fontface = lab_fontface
      )
  }

  # matrix cell glyphs
  if (!is.null(p.mat) & insig == "pch" & !sig.stars) {
    p <- p + ggplot2::geom_point(
      data = p.mat,
      mapping = ggplot2::aes(x = .data[["Var1"]], y = .data[["Var2"]]),
      shape = pch,
      size = pch.cex,
      color = pch.col
    )
  }

  # add titles
  if (title != "") {
    p <- p +
      ggplot2::ggtitle(title)
  }

  # removing legend
  if (!show.legend) {
    p <- p +
      ggplot2::theme(legend.position = "none")
  }

  # removing panel
  p <- p +
    .no_panel()
  p
}



#' Compute the matrix of correlation p-values
#'
#' @details \code{cor_pmat()} tests each pair of columns with
#'   \code{\link[stats]{cor.test}}. A pair with fewer than three overlapping
#'   non-missing observations (which \code{\link[stats]{cor.test}} cannot test,
#'   e.g. two variables that never co-occur) yields \code{NA} for that cell
#'   rather than aborting the whole computation. Pairs that can be tested are
#'   computed as before, and errors they raise are passed through.
#'
#'   The \code{use} argument controls which pairs are returned as \code{NA} so
#'   the p-value matrix can be aligned with a correlation matrix built the same
#'   way. With the default \code{"pairwise.complete.obs"} every pair that has
#'   enough overlapping observations is tested (the previous behavior). With
#'   \code{"everything"} a pair is set to \code{NA} whenever either variable has
#'   any missing value, so the \code{NA} pattern matches
#'   \code{\link[stats]{cor}(x)} with its default \code{use = "everything"}.
#'
#' @param x numeric matrix or data frame
#' @param ... other arguments to be passed to the function cor.test.
#' @param use character, how to treat pairs involving missing values when
#'   deciding which cells are \code{NA}. Either \code{"pairwise.complete.obs"}
#'   (default; test every pair that has enough overlapping observations) or
#'   \code{"everything"} (set a pair to \code{NA} as soon as either variable has
#'   a missing value, matching \code{\link[stats]{cor}}'s default). Mirrors the
#'   corresponding values of \code{\link[stats]{cor}}'s \code{use} argument.
#' @rdname ggcorrplot
#' @export

cor_pmat <- function(x, ..., use = c("pairwise.complete.obs", "everything")) {

  use <- match.arg(use)

  # initializing values
  mat <- as.matrix(x)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0

  # Pattern of pairs to leave as NA, taken from cor() under the same `use`, so
  # the result can be aligned with a cor(x, use = ...) matrix (#51). This is
  # missingness-driven, so it does not depend on the correlation method passed
  # through `...`; computing the mask with cor()'s default (pearson) is fine.
  cor_mask <- stats::cor(mat, use = use)

  # creating the p-value matrix
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (is.na(cor_mask[i, j])) {
        # cor() could not compute this pair under `use`: leave it NA.
        p.mat[i, j] <- p.mat[j, i] <- NA_real_
      } else {
        # a pair with too few overlapping observations (e.g. two variables that
        # never co-occur) makes cor.test() error; return NA for that cell instead
        # of aborting the whole matrix (#51). The NA substitution is gated on the
        # overlap count, so for any pair that CAN be tested (>= 3 overlapping obs)
        # a genuine error (bad method =, non-numeric input, ...) is re-raised and
        # still surfaces loudly instead of silently becoming NA.
        p.mat[i, j] <- p.mat[j, i] <- tryCatch(
          stats::cor.test(mat[, i], mat[, j], ...)$p.value,
          error = function(e) {
            if (sum(stats::complete.cases(mat[, i], mat[, j])) < 3) {
              NA_real_
            } else {
              stop(e)
            }
          }
        )
      }
    }
  }

  # name rows and columns of the p-value matrix
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)

  # return the final matrix
  p.mat
}



#+++++++++++++++++++++++
# Helper Functions
#+++++++++++++++++++++++

# Reorder -> round -> mask a triangle -> melt -> join per-cell significance.
# Takes the already-coerced correlation matrix and (optional) p-value matrix and
# the resolved show.diag, and returns the two long data frames the plot layers
# are built from:
#   $corr  : melted correlation data frame (Var1, Var2, value, pvalue, signif,
#            [coef], abs_corr)
#   $p.mat : the insignificant-cells data frame used by the insig = "pch" layer,
#            or NULL when no p.mat was supplied.
# Behaviour is identical to the inline pipeline it was extracted from (P0.0); the
# order of operations matters (round happens AFTER clustering and the
# significance test, #14/#25) and is preserved exactly.
.build_corr_df <- function(corr, p.mat, type, show.diag, hc.order, hc.method,
                           digits, sig.level, insig, as.is) {
  # Reordering and the triangular layouts require a square matrix; a non-square
  # (m x n) correlation matrix can only be shown in full (#5, #10).
  if (nrow(corr) != ncol(corr)) {
    if (hc.order) {
      stop("hc.order = TRUE requires a square correlation matrix.")
    }
    if (type != "full") {
      stop("type = '", type, "' requires a square correlation matrix; ",
           "use type = 'full' for a non-square matrix.")
    }
  }

  # Order on the unrounded matrix so the internal rounding below does not
  # introduce ties that perturb the hierarchical clustering (#14).
  if (hc.order) {
    ord <- .hc_cormat_order(corr, hc.method = hc.method)
    corr <- corr[ord, ord]
    if (!is.null(p.mat)) {
      p.mat <- p.mat[ord, ord]
    }
  }

  corr <- base::round(x = corr, digits = digits)

  if (!show.diag) {
    corr <- .remove_diag(corr)
    p.mat <- .remove_diag(p.mat)
  }

  # Get lower or upper triangle
  if (type == "lower") {
    corr <- .get_lower_tri(corr, show.diag)
    p.mat <- .get_lower_tri(p.mat, show.diag)
  } else if (type == "upper") {
    corr <- .get_upper_tri(corr, show.diag)
    p.mat <- .get_upper_tri(p.mat, show.diag)
  }

  # Melt corr and pmat
  corr <- reshape2::melt(corr, na.rm = TRUE, as.is = as.is)
  colnames(corr) <- c("Var1", "Var2", "value")
  corr$pvalue <- rep(NA, nrow(corr))
  corr$signif <- rep(NA, nrow(corr))

  if (!is.null(p.mat)) {
    p.mat <- reshape2::melt(p.mat, na.rm = TRUE, as.is = as.is)
    colnames(p.mat) <- c("Var1", "Var2", "value")
    # Match each p-value to its correlation cell by (Var1, Var2) rather than by
    # row position, so a differing NA pattern between corr and p.mat cannot
    # misalign them (or raise a length error). When the patterns match, the
    # match is the identity and the result is byte-identical.
    idx <- match(
      paste(corr$Var1, corr$Var2, sep = "\r"),
      paste(p.mat$Var1, p.mat$Var2, sep = "\r")
    )
    corr$coef <- corr$value
    corr$pvalue <- p.mat$value[idx]
    corr$signif <- as.numeric(corr$pvalue <= sig.level)
    p.mat <- subset(p.mat, p.mat$value > sig.level)
    # keep significance markers only for cells present in the correlation plot
    p.mat <- p.mat[paste(p.mat$Var1, p.mat$Var2, sep = "\r") %in%
      paste(corr$Var1, corr$Var2, sep = "\r"), ]
    if (insig == "blank") {
      # a cell with no matching p-value (unknown significance) is kept as-is
      keep <- ifelse(is.na(corr$signif), 1, corr$signif)
      corr$value <- corr$value * keep
    }
  } else {
    p.mat <- NULL
  }

  corr$abs_corr <- abs(corr$value) * 10

  list(corr = corr, p.mat = p.mat)
}

# Build the glyph layer(s) for a given method. Returns a list of ggplot
# components so the caller can add them with a single `+` (and so the
# mixed-method path can request a different glyph per triangle, P0.0).
.method_layer <- function(method, outline.color, circle.scale) {
  if (method == "square") {
    list(ggplot2::geom_tile(color = outline.color))
  } else if (method == "circle") {
    list(
      ggplot2::geom_point(
        color = outline.color,
        shape = 21,
        ggplot2::aes(size = .data[["abs_corr"]])
      ),
      ggplot2::scale_size(range = c(4, 10) * circle.scale),
      ggplot2::guides(size = "none")
    )
  }
}

# Get lower triangle of the correlation matrix
.get_lower_tri <- function(cormat, show.diag = FALSE) {
  if (is.null(cormat)) {
    return(cormat)
  }
  cormat[upper.tri(cormat)] <- NA
  if (!show.diag) {
    diag(cormat) <- NA
  }
  return(cormat)
}

# Get upper triangle of the correlation matrix
.get_upper_tri <- function(cormat, show.diag = FALSE) {
  if (is.null(cormat)) {
    return(cormat)
  }
  cormat[lower.tri(cormat)] <- NA
  if (!show.diag) {
    diag(cormat) <- NA
  }
  return(cormat)
}

.remove_diag <- function(cormat) {
  if (is.null(cormat)) {
    return(cormat)
  }
  diag(cormat) <- NA
  cormat
}
# hc.order correlation matrix
.hc_cormat_order <- function(cormat, hc.method = "complete") {
  dd <- stats::as.dist((1 - cormat) / 2)
  hc <- stats::hclust(dd, method = hc.method)
  hc$order
}

.no_panel <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()
  )
}


# Convert a tbl to matrix
.tibble_to_matrix <- function(x) {
  x <- as.data.frame(x)
  rownames(x) <- x[, 1]
  x <- x[, -1]
  as.matrix(x)
}
