#' Visualization of a correlation matrix using ggplot2
#' @import ggplot2
#' @importFrom grDevices col2rgb rgb
#' @description \itemize{ \item ggcorrplot(): A graphical display of a
#'   correlation matrix using ggplot2. The plot options are walked through in
#'   \href{https://www.datanovia.com/learn/biostatistics/correlation/ggcorrplot-in-r}{ggcorrplot:
#'   Correlation Matrix Heatmap in R with ggplot2}. \item cor_pmat(): Compute a
#'   correlation matrix p-values. Its use alongside the correlation matrix
#'   itself is covered in
#'   \href{https://www.datanovia.com/learn/biostatistics/correlation/correlation-matrix-in-r}{Correlation
#'   Matrix in R: Compute, Visualize & P-values}. }
#' @param corr the correlation matrix to visualize
#' @param method character, the visualization method of correlation matrix to be
#'   used. Allowed values are "square" (default), "circle".
#' @param scale.square logical. If \code{TRUE} and \code{method = "square"}, the
#'   squares are sized by the absolute correlation (larger square = stronger
#'   correlation), in addition to the fill color -- the classic corrplot
#'   size-scaled square look. Defaults to \code{FALSE} (constant full-cell
#'   squares, the current behavior). Has no effect for \code{method = "circle"}
#'   (circles are always sized). Uses \code{circle.scale} to tune the size range.
#'   As with \code{method = "circle"}, coefficient labels (\code{lab = TRUE}) are
#'   drawn at full size and may overflow the smallest squares.
#' @param cell.grid logical. If \code{TRUE}, draw a light rectangle around every
#'   cell (behind the glyphs) and remove the through-center gridlines, so the
#'   sized glyphs (\code{method = "circle"} or \code{scale.square = TRUE}) sit
#'   inside boxed cells -- the corrplot boxed-cell look. Defaults to \code{FALSE}
#'   (the current behavior). Has no effect on a full-tile square heatmap
#'   (\code{method = "square"} without \code{scale.square}), whose tiles already
#'   carry a cell border (\code{outline.color}).
#' @param cell.grid.col the color of the \code{cell.grid} cell borders. Defaults
#'   to \code{"grey90"}. Only used when \code{cell.grid = TRUE}.
#' @param lower.method,upper.method character, an optional per-triangle glyph for
#'   a mixed layout: one of "square", "circle" or "number" (the coefficient drawn
#'   as text, colored by its value on the fill ramp). When either is set, the plot
#'   switches to a mixed layout where the lower and upper triangles are drawn
#'   separately and the variable names are drawn on the diagonal; a triangle left
#'   \code{NULL} uses \code{method}. Both default to \code{NULL} (single-method
#'   plot, unchanged). In a mixed layout the single-method significance and label
#'   overlays (\code{lab}, \code{sig.stars}, \code{p.mat}, \code{insig},
#'   \code{pch*}) do not apply; show coefficients with a "number" triangle instead.
#'
#'   Over a light background the "number" text is drawn on a darkened copy of the
#'   ramp -- same hues, so warm still reads as positive and cool as negative, but
#'   dark enough that a coefficient near zero stays readable instead of washing
#'   out. Over a dark background the ramp is used as given, its pale middle being
#'   what reads there. The background is taken from \code{ggtheme}; a theme added
#'   to the returned plot with \code{+} arrives too late to be seen.
#' @param type character, "full" (default), "lower" or "upper" display. A mixed
#'   layout (see \code{lower.method}/\code{upper.method}) always uses the full
#'   matrix.
#' @param ggtheme ggplot2 function or theme object. Default value is
#'   `theme_minimal`. Allowed values are the official ggplot2 themes including
#'   theme_gray, theme_bw, theme_minimal, theme_classic, theme_void, .... Theme
#'   objects are also allowed (e.g., `theme_classic()`). A mixed "number" region
#'   reads the background from this argument, resolved against the default theme
#'   in force when \code{ggcorrplot()} is called, to decide how dark to draw the
#'   coefficient text (see \code{lower.method}/\code{upper.method}). So a dark
#'   theme belongs here rather than added to the returned plot with \code{+}, and
#'   a \code{\link[ggplot2]{theme_set}} issued after the call is not seen.
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
#' @param palette optional name of a built-in colorblind-safe diverging palette
#'   for the fill gradient: \code{"RdBu"} or \code{"PuOr"}. A convenience
#'   shortcut for \code{colors}: when set it supplies the gradient (an 11-stop
#'   ramp, white at zero, cool = negative / warm = positive) and takes precedence
#'   over \code{colors}. Defaults to \code{NULL} (use \code{colors}), so existing
#'   calls are unchanged.
#' @param preset optional name of a bundle of publication-grade defaults. The only
#'   value, \code{"publication"}, sets white cell outlines and the colorblind-safe
#'   \code{"RdBu"} palette in one token. It fills only the arguments you did not
#'   supply, so any argument you pass explicitly (e.g. \code{outline.color},
#'   \code{colors}, \code{palette}) overrides the preset. Defaults to \code{NULL}
#'   (no preset), leaving existing calls unchanged.
#' @param outline.color the outline color of square or circle. Default value is
#'   "gray".
#' @param hc.order logical value. If TRUE, correlation matrix will be hc.ordered
#'   using hclust function.
#' @param hc.method the agglomeration method to be used in hclust (see ?hclust).
#' @param hc.rect integer or \code{NULL} (default). If an integer \code{k}, draws
#'   \code{k} rectangles (no fill) around the clusters obtained by cutting the
#'   hierarchical tree, marking the cluster blocks on the diagonal. Requires
#'   \code{hc.order = TRUE} and \code{type = "full"} (the boxes span whole
#'   diagonal blocks). \code{NULL} (default) draws no rectangles. For a fully
#'   custom box style, add your own \code{annotate("rect", ...)} to the returned
#'   plot.
#' @param hc.rect.col the outline color of the \code{hc.rect} cluster rectangles.
#'   Defaults to \code{"gray30"}; set it to any color that suits your palette
#'   (e.g. \code{"black"} for a bolder box, or \code{"white"}). Only used when
#'   \code{hc.rect} is set.
#' @param lab logical value. If TRUE, add correlation coefficient on the plot.
#' @param lab_col,lab_size size and color to be used for the correlation
#'   coefficient labels. used when lab = TRUE.
#' @param lab_fontface the font face (\code{"plain"}, \code{"bold"},
#'   \code{"italic"}, \code{"bold.italic"}) for the correlation coefficient
#'   labels. Default is \code{"plain"}. Used when \code{lab = TRUE}.
#' @param sig.stars logical value. If \code{TRUE} and a \code{p.mat} is
#'   supplied, significance stars are appended to the coefficient labels
#'   (\code{***} for p <= 0.001, \code{**} for p <= 0.01, \code{*} for
#'   p <= 0.05), e.g. \code{"-0.85**"}. Only used when \code{lab = TRUE}. Default
#'   is \code{FALSE}. When \code{TRUE}, significance is shown by the stars and
#'   the \code{insig = "pch"} markers are not drawn. These three thresholds are
#'   fixed and are not affected by \code{sig.level}.
#' @param p.mat matrix of p-value. If NULL, arguments sig.level, insig, pch,
#'   pch.col, pch.cex is invalid.
#' @param sig.level significant level, if the p-value in p-mat is bigger than
#'   sig.level, then the corresponding correlation coefficient is regarded as
#'   insignificant. This governs which cells \code{insig = "pch"} marks and
#'   \code{insig = "blank"} wipes; the star thresholds used by
#'   \code{insig = "stars"} and \code{sig.stars} are fixed (see those arguments)
#'   and do not follow \code{sig.level}.
#' @param insig character, how to convey significance from \code{p.mat}: "pch"
#'   (default), "blank" or "stars". "pch" adds a character (see \code{pch}) on the
#'   glyphs of the insignificant cells; "blank" wipes those glyphs away; "stars"
#'   instead marks the SIGNIFICANT cells with significance stars
#'   (\code{***}/\code{**}/\code{*} for p <= 0.001/0.01/0.05 -- fixed thresholds,
#'   not \code{sig.level}). With the default
#'   \code{lab = FALSE} the stars are drawn on their own (in \code{pch.col}, sized
#'   by \code{lab_size}) as a standalone significance map; with \code{lab = TRUE}
#'   they are appended to the coefficient labels (e.g. \code{"-0.85***"}, as with
#'   \code{sig.stars}) so the two do not overprint.
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
#' @param as.is retained for backward compatibility; no longer affects the plot.
#'   The axis is now always drawn in the matrix (row/column) order, so the
#'   variable-name handling this argument used to control is done internally.
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
#' # Mixed layout: a different glyph per triangle
#' # --------------------------------
#' # numbers in the lower triangle, circles in the upper, names on the diagonal
#' ggcorrplot(corr,
#'   lower.method = "number", upper.method = "circle",
#'   show.legend = FALSE
#' )
#'
#' # Reordering the correlation matrix
#' # --------------------------------
#' # using hierarchical clustering
#' ggcorrplot(corr, hc.order = TRUE, outline.color = "white")
#' # draw rectangles around the clusters
#' ggcorrplot(corr, hc.order = TRUE, hc.rect = 3, outline.color = "white")
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
#' @seealso
#'   \href{https://www.datanovia.com/learn/biostatistics/correlation/ggcorrplot-in-r}{ggcorrplot:
#'   Correlation Matrix Heatmap in R with ggplot2} and
#'   \href{https://www.datanovia.com/learn/biostatistics/correlation/correlation-matrix-in-r}{Correlation
#'   Matrix in R: Compute, Visualize & P-values} for worked examples of the
#'   plot.
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
                       insig = c("pch", "blank", "stars"),
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
                       coord.fixed = TRUE,
                       lower.method = NULL,
                       upper.method = NULL,
                       hc.rect = NULL,
                       palette = NULL,
                       preset = NULL,
                       hc.rect.col = "gray30",
                       scale.square = FALSE,
                       cell.grid = FALSE,
                       cell.grid.col = "grey90") {
  type <- match.arg(type)
  method <- match.arg(method)
  # Resolve on insig[1] (not match.arg(insig)) so a caller passing the old
  # documented default vector explicitly -- insig = c("pch", "blank") -- still
  # collapses to its first element instead of erroring: match.arg() only accepts
  # a multi-value arg when it is identical to the full choices set, and adding
  # "stars" made c("pch", "blank") non-identical. Scalar/partial matching and the
  # default are unchanged.
  insig <- match.arg(insig[1], c("pch", "blank", "stars"))
  # A preset is a bundle of "publication-grade" defaults (white cell outlines and
  # a colorblind-safe diverging palette). It fills ONLY the arguments the caller
  # did not supply -- detected with missing() BEFORE any reassignment, so an
  # explicit argument always overrides the preset, and preset = NULL (the default)
  # changes nothing. This must run above the palette block, which reassigns
  # `colors`/`palette`.
  if (!is.null(preset)) {
    preset <- match.arg(preset, "publication")
    if (missing(outline.color)) outline.color <- "white"
    # Reach for the preset palette only when the caller pinned neither `palette`
    # nor `colors`, so an explicit `colors` is never clobbered.
    if (missing(palette) && missing(colors)) palette <- "RdBu"
  }
  # A named colorblind-safe diverging palette is a convenience shortcut for
  # `colors`: when set (non-NULL, the default), it supplies the fill gradient and
  # takes precedence over `colors`. `palette = NULL` leaves `colors` untouched, so
  # every existing call is unchanged.
  if (!is.null(palette)) {
    palette <- match.arg(palette, c("RdBu", "PuOr"))
    colors <- .ggcorrplot_palette(palette)
  }
  if (is.null(show.diag)) {
    if (type == "full") {
      show.diag <- TRUE
    } else {
      show.diag <- FALSE
    }
  }

  # Mixed layout: a different glyph per triangle (and the diagonal), requested via
  # the per-triangle arguments. Mixed mode fires only when at least one of them is
  # set, so every call that leaves them NULL (all existing calls) takes the
  # unchanged single-method path below and is byte-identical. "number" (a text
  # coefficient) is a value of these per-triangle arguments only; it is
  # deliberately NOT added to the scalar `method` choice set, whose match.arg must
  # keep resolving c("square","circle") to "square" (#85). The diagonal always
  # shows the variable names in a mixed layout (there is intentionally no
  # `diag.*` argument: any such name would start with "d" and make an abbreviated
  # `digits` call, e.g. `ggcorrplot(x, d = 2)`, ambiguous -- a CRAN regression).
  mixed <- !is.null(lower.method) || !is.null(upper.method)
  if (mixed) {
    glyphs <- c("square", "circle", "number")
    lower.method <- if (is.null(lower.method)) method else match.arg(lower.method, glyphs)
    upper.method <- if (is.null(upper.method)) method else match.arg(upper.method, glyphs)
    # A mixed layout draws both triangles and the diagonal, i.e. the full matrix.
    type <- "full"
    show.diag <- TRUE
    # The single-method coefficient/significance overlays do not apply to a mixed
    # layout (the coefficients are a region glyph via lower/upper.method =
    # "number"). Tell the user rather than dropping their arguments silently.
    if (lab || sig.stars || !is.null(p.mat)) {
      message(
        "In a mixed layout, `lab`, `sig.stars`, `p.mat`, `insig` and `pch*` are ",
        "not applied; use lower.method/upper.method = \"number\" to show the ",
        "coefficients."
      )
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

  if (mixed) {
    if (nrow(corr) != ncol(corr)) {
      stop("A mixed layout (lower.method/upper.method) requires a square ",
           "correlation matrix.", call. = FALSE)
    }
    # The mixed layout splits cells by grid position and names the diagonal from
    # the row variable, so the diagonal is only meaningful when the row and column
    # names match (as they do for any correlation matrix). Refuse a square matrix
    # with differing row/column names rather than silently mislabelling it.
    if (!identical(rownames(corr), colnames(corr))) {
      stop("A mixed layout requires the correlation matrix to have matching row ",
           "and column names.", call. = FALSE)
    }
  }

  # Transform the correlation (and matching p-value) matrix into the long
  # data frames the plot is built from: reorder, round, mask a triangle, melt,
  # and join per-cell significance. Extracted so the mixed-method path can reuse
  # exactly the same pipeline (P0.0). Returns the melted `corr` data frame and
  # the insignificant-cells `p.mat` data frame (or NULL).
  built <- .build_corr_df(
    corr = corr, p.mat = p.mat, type = type, show.diag = show.diag,
    hc.order = hc.order, hc.method = hc.method, digits = digits,
    sig.level = sig.level,
    # A mixed layout does not overlay significance, so the insig = "blank"
    # value-zeroing must not run here: otherwise a "number" region would print a
    # literal 0 for a non-significant cell instead of its coefficient. Force the
    # non-blanking path when mixed so the numbers stay the true coefficients.
    insig = if (mixed) "pch" else insig,
    as.is = as.is
  )
  corr <- built$corr
  p.mat <- built$p.mat
  hc <- built$hc

  # heatmap
  if (mixed) {
    # Pin BOTH axes to ONE level order, taken from the matrix order that
    # .build_corr_df() already established: its factor levels for a named matrix,
    # or the sorted integer indices for an unnamed one (which melts to 1..n).
    # Deriving the two axes independently from the order the cells happen to
    # appear in after melt() dropped the NA cells puts them out of step whenever
    # the NA pattern is ragged -- cor(x, use = "pairwise.complete.obs") with a
    # non-co-occurring pair, or a zero-variance column, both produce one -- and
    # then the region split below compares positions on two different axes, so
    # the diagonal name region lands on cells that are not self-pairs and
    # silently mislabels them. A level absent from the data on BOTH axes (an
    # all-NA variable) is dropped so the drop = FALSE axes do not resurrect it as
    # an empty band; a variable that still has data on one axis is kept.
    lvls <- if (is.factor(corr$Var1)) {
      levels(corr$Var1)
    } else {
      as.character(sort(unique(c(corr$Var1, corr$Var2))))
    }
    present <- unique(c(as.character(corr$Var1), as.character(corr$Var2)))
    lvls <- lvls[lvls %in% present]
    corr$Var1 <- factor(as.character(corr$Var1), levels = lvls)
    corr$Var2 <- factor(as.character(corr$Var2), levels = lvls)

    # One glyph per region, each drawn from its own subset of the cells. The base
    # plot carries no global fill so the text glyphs ("number"/"name") are not
    # forced to inherit it; each glyph layer sets the aesthetics it needs.
    p <- ggplot2::ggplot(
      data = corr,
      mapping = ggplot2::aes(x = .data[["Var1"]], y = .data[["Var2"]])
    )
    p <- p + .mixed_layers(
      corr, lower.method, upper.method,
      outline.color = outline.color, circle.scale = circle.scale,
      lab_size = lab_size, tl.cex = tl.cex, tl.col = tl.col,
      digits = digits, nsmall = nsmall, leading.zero = leading.zero,
      scale.square = scale.square,
      cell.grid = cell.grid, cell.grid.col = cell.grid.col
    )
    # A mixed layout always boxes at least the diagonal (name) region when
    # cell.grid = TRUE, so the gridline blanking below is always consistent here.
    draw.cell.grid <- cell.grid
  } else {
    p <-
      ggplot2::ggplot(
        data = corr,
        mapping = ggplot2::aes(x = .data[["Var1"]], y = .data[["Var2"]], fill = .data[["value"]])
      )

    # cell.grid: draw a light cell-rectangle grid BEHIND the sized glyphs so they
    # sit inside boxed cells (the corrplot look). Added before the glyph layer so
    # it renders underneath. Skipped for a full-tile square heatmap (method =
    # "square" without scale.square): its geom_tile already carries a cell border
    # (outline.color), so a second border would double-draw. fill = NA overrides
    # the inherited aes(fill = value) for drawing while still training the same
    # fill scale as the glyph layer (byte-identical fill legend).
    draw.cell.grid <- cell.grid && !(method == "square" && !scale.square)
    if (draw.cell.grid) {
      p <- p + ggplot2::geom_tile(fill = NA, colour = cell.grid.col)
    }

    # modification based on method (extracted so the mixed-method path can request
    # a different glyph per triangle from the same builder, P0.0)
    p <- p + .method_layer(method,
      outline.color = outline.color, circle.scale = circle.scale,
      scale.square = scale.square
    )
  }

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

  # A "number" region colours its text by the correlation value, so it needs a
  # colour scale that matches the fill scale. When a square/circle region also
  # draws fill, that fill scale carries the legend and the (redundant) colour
  # guide is suppressed; when numbers are the ONLY value encoding (e.g. both
  # triangles are "number"), the colour scale carries the legend instead, so the
  # plot is never left legend-less.
  if (mixed && "number" %in% c(lower.method, upper.method)) {
    has_fill_glyph <- any(c(lower.method, upper.method) %in% c("square", "circle"))
    colour_name <- if (has_fill_glyph) ggplot2::waiver() else legend.title
    # Text needs contrast that a fill does not: the fill ramp is centered on
    # white, so mapping the coefficient TEXT onto it unchanged makes every
    # near-zero coefficient invisible against the panel -- and a blank cell in a
    # correlogram reads as removed/non-significant, so an unreadable number is
    # misleading, not merely faint. Darken the ramp for the text only, keeping
    # each stop's hue (so warm = positive / cool = negative still reads at a
    # glance) but capping its luminance -- and only when the panel is light, since
    # on a dark panel the pale middle is already the readable end.
    text_colors <- if (.panel_is_light(ggtheme)) .legible_text_colors(colors) else colors
    if (length(colors) == 3) {
      p <- p + ggplot2::scale_colour_gradient2(
        low = text_colors[1], high = text_colors[3], mid = text_colors[2],
        midpoint = 0, limit = legend.limit, space = "Lab", name = colour_name
      )
    } else {
      p <- p + ggplot2::scale_colour_gradientn(
        colours = text_colors, limits = legend.limit, name = colour_name
      )
    }
    if (has_fill_glyph) p <- p + ggplot2::guides(colour = "none")
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
  # cell.grid replaces the through-center gridlines with per-cell rectangles, so
  # blank the panel grid (added after the theme() above so it wins). Gated on
  # draw.cell.grid -- TRUE only when boxes were actually drawn -- so a full-tile
  # square heatmap (which draws no box and keeps its gridlines behind the opaque
  # tiles) is genuinely untouched, matching the documented "no effect". This also
  # keeps a lower/upper full-tile square's gridlines in the blank triangle intact.
  if (draw.cell.grid) {
    p <- p + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  }
  if (coord.fixed) {
    p <- p + ggplot2::coord_fixed()
  } else {
    p <- p + ggplot2::coord_cartesian()
  }

  # The coefficient-label overlay and the significance glyphs are single-method
  # features. In a mixed layout the coefficients are a region glyph
  # (method.* = "number") and significance is not overlaid, so this whole block
  # is skipped; a non-mixed call runs it exactly as before.
  if (!mixed) {
    label <- .format_coef(corr[, "value"], digits = digits, nsmall = nsmall,
                          leading.zero = leading.zero)
    # Append significance stars to the coefficient labels for sig.stars, and also
    # for insig = "stars" when labels are shown (lab = TRUE): the stars share the
    # label's cell center, so routing them into the suffix here -- rather than
    # adding the standalone geom_text below -- avoids two text layers overprinting
    # into illegible output. For all other insig values this reduces to the
    # sig.stars condition exactly as before (byte-identical).
    if ((sig.stars || insig == "stars") && !is.null(p.mat)) {
      label <- paste0(label, .sig_stars(corr$pvalue))
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

    # standalone significance stars: mark the SIGNIFICANT cells with */**/***
    # (non-significant cells get nothing) as a significance-only map. Only drawn
    # when lab = FALSE; with lab = TRUE the stars are appended to the coefficient
    # labels above (numbers + stars, one text layer) so the two do not overprint.
    if (!is.null(p.mat) & insig == "stars" & !lab) {
      p <- p + ggplot2::geom_text(
        mapping = ggplot2::aes(x = .data[["Var1"]], y = .data[["Var2"]]),
        label = .sig_stars(corr$pvalue),
        color = pch.col,
        size = lab_size
      )
    }
  }

  # cluster rectangles: draw hc.rect boxes around the k clusters obtained by
  # cutting the dendrogram. Requires hc.order (so the variables are in dendrogram
  # order and each cluster is a contiguous block). The blocks are full squares on
  # the diagonal, so they are only meaningful on the full matrix -- on a lower/
  # upper triangle a box would extend over the blanked half. Added on top of the
  # glyphs.
  if (!is.null(hc.rect)) {
    if (!hc.order) {
      stop("hc.rect requires hc.order = TRUE.", call. = FALSE)
    }
    if (type != "full") {
      stop("hc.rect requires type = \"full\"; the cluster boxes span the whole ",
           "diagonal block and would extend over the hidden triangle otherwise.",
           call. = FALSE)
    }
    # The box positions are grid indices 1..n in dendrogram order; they line up
    # because .build_corr_df() coerces the axis to a factor in that order, so
    # numeric-looking or as.is names no longer sort the axis by value/alphabet.
    k <- hc.rect
    n <- length(hc$order)
    if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 1 || k > n ||
        k != as.integer(k)) {
      stop("hc.rect must be a single integer between 1 and ", n,
           " (the number of variables).", call. = FALSE)
    }
    b <- .hc_rect_bounds(hc, k)
    # No linewidth/size: the default border keeps this compatible with the
    # declared ggplot2 floor (linewidth is a ggplot2 >= 3.4.0 aesthetic).
    p <- p + ggplot2::annotate(
      "rect",
      xmin = b$lo - 0.5, xmax = b$hi + 0.5,
      ymin = b$lo - 0.5, ymax = b$hi + 0.5,
      fill = NA, colour = hc.rect.col
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
#' @seealso
#'   \href{https://www.datanovia.com/learn/biostatistics/correlation/correlation-test-in-r}{Correlation
#'   Test in R: Pearson, Spearman & Kendall} for the test behind the p-values.
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
  hc <- NULL
  if (hc.order) {
    hc <- .hc_cormat_order(corr, hc.method = hc.method)
    ord <- hc$order
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

  # The plot must display the variables in the matrix row/column order (which
  # carries the hc.order reordering). Capture that order before melting, then
  # coerce the axis back to a factor with these levels so the display order never
  # depends on how the names happen to sort (#37). Melting with as.is = TRUE keeps
  # the dimnames VERBATIM -- reshape2::melt's default type-conversion would turn
  # numeric-looking names into their numeric values, so as.character() of a
  # non-canonical name (e.g. "01", "1.10", "1e2") would no longer match the
  # captured level and every cell would collapse to NA. `unique()` guards the
  # pathological case of duplicated names (which would otherwise error on the
  # factor's duplicate levels).
  # Only named axes carry a meaningful order to preserve; an unnamed axis melts to
  # integer indices 1..n, which are already in row order, so it is left exactly as
  # before (a continuous axis) -- coercing it would be an unnecessary behavior
  # change for that input. Rows and columns are gated independently so a
  # partially-named matrix keeps whichever axis has names.
  row_levels <- if (!is.null(rownames(corr))) unique(rownames(corr)) else NULL
  col_levels <- if (!is.null(colnames(corr))) unique(colnames(corr)) else NULL

  # Melt corr and pmat
  corr <- reshape2::melt(corr, na.rm = TRUE, as.is = TRUE)
  colnames(corr) <- c("Var1", "Var2", "value")
  if (!is.null(row_levels)) corr$Var1 <- factor(as.character(corr$Var1), levels = row_levels)
  if (!is.null(col_levels)) corr$Var2 <- factor(as.character(corr$Var2), levels = col_levels)
  corr$pvalue <- rep(NA, nrow(corr))
  corr$signif <- rep(NA, nrow(corr))

  if (!is.null(p.mat)) {
    p.mat <- reshape2::melt(p.mat, na.rm = TRUE, as.is = TRUE)
    colnames(p.mat) <- c("Var1", "Var2", "value")
    if (!is.null(row_levels)) p.mat$Var1 <- factor(as.character(p.mat$Var1), levels = row_levels)
    if (!is.null(col_levels)) p.mat$Var2 <- factor(as.character(p.mat$Var2), levels = col_levels)
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

  # `hc` is the hclust object when hc.order = TRUE (needed for hc.rect), else NULL.
  list(corr = corr, p.mat = p.mat, hc = hc)
}

# Build the glyph layer(s) for a given method. Returns a list of ggplot
# components so the caller can add them with a single `+` (and so the
# mixed-method path can request a different glyph per triangle, P0.0).
.method_layer <- function(method, outline.color, circle.scale, scale.square = FALSE) {
  if (method == "square" && !scale.square) {
    list(ggplot2::geom_tile(color = outline.color))
  } else if (method == "square" && scale.square) {
    # size-scaled squares (corrplot-style): a filled square (shape 22) whose size
    # encodes |r|, mirroring the sized-circle glyph. Only reached when the caller
    # opts in with scale.square = TRUE, so the default square heatmap is unchanged.
    list(
      ggplot2::geom_point(
        color = outline.color,
        shape = 22,
        ggplot2::aes(size = .data[["abs_corr"]])
      ),
      ggplot2::scale_size(range = c(4, 10) * circle.scale),
      ggplot2::guides(size = "none")
    )
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

# Format the correlation coefficients for display: round to `digits`, optionally
# pad to `nsmall` decimals, optionally drop the leading zero. Shared by the
# single-method label overlay and the mixed "number" glyph so they format the
# same way.
.format_coef <- function(x, digits, nsmall, leading.zero) {
  label <- round(x = x, digits = digits)
  if (nsmall > 0) label <- format(label, nsmall = nsmall, trim = TRUE)
  if (!leading.zero) {
    # drop the leading zero of values in (-1, 1), e.g. 0.23 -> .23, -0.67 -> -.67
    # (idiom from @PawelKulawiak, #15). The \\b keeps values like 1.00 untouched.
    label <- gsub("\\b0(\\.\\d+)", "\\1", label)
  }
  label
}

# Significance stars for a vector of p-values: "***" p <= 0.001, "**" p <= 0.01,
# "*" p <= 0.05, "" otherwise (and "" for NA). The cut() breaks below are
# right-closed, so each threshold is inclusive. Shared by the sig.stars label
# suffix and the standalone insig = "stars" glyph so both use one definition.
.sig_stars <- function(pvalue) {
  stars <- as.character(cut(pvalue,
    breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
    labels = c("***", "**", "*", "")
  ))
  stars[is.na(stars)] <- ""
  stars
}

# Build the layers for a mixed layout: a per-triangle glyph over the lower
# triangle (Var1 index > Var2 index) and the upper triangle, plus the variable
# names on the diagonal. Each region is a subset of the melted correlation data
# frame drawn with its own layer so the glyphs can differ. "square"/"circle" draw
# the usual glyphs; "number" draws the coefficient as text colored by value
# (matched by a colour scale added in the caller); the diagonal always draws the
# variable name. Returns a list of ggplot components.
.mixed_layers <- function(df, lower.method, upper.method,
                          outline.color, circle.scale, lab_size, tl.cex, tl.col,
                          digits, nsmall, leading.zero, scale.square = FALSE,
                          cell.grid = FALSE, cell.grid.col = "grey90") {
  xi <- as.integer(df$Var1)
  yi <- as.integer(df$Var2)
  regions <- list(
    list(data = df[xi > yi, , drop = FALSE], method = lower.method),
    list(data = df[xi < yi, , drop = FALSE], method = upper.method),
    list(data = df[xi == yi, , drop = FALSE], method = "name")
  )

  layers <- list()
  has_sized <- FALSE
  for (r in regions) {
    d <- r$data
    if (nrow(d) == 0) next
    m <- r$method
    # cell.grid: box every region behind its glyph, EXCEPT a full-tile square
    # region (m == "square" without scale.square), whose geom_tile already draws
    # an outline.color border -- a second box would double-draw. Added before the
    # region's glyph so it renders underneath. fill = NA (no aes fill inherited
    # from the mixed base plot) draws only the border.
    if (cell.grid && !(m == "square" && !scale.square)) {
      layers <- c(layers, list(ggplot2::geom_tile(
        data = d, fill = NA, colour = cell.grid.col
      )))
    }
    if (m == "square" && !scale.square) {
      layers <- c(layers, list(ggplot2::geom_tile(
        data = d,
        mapping = ggplot2::aes(fill = .data[["value"]]),
        color = outline.color
      )))
    } else if (m == "square" && scale.square) {
      has_sized <- TRUE
      layers <- c(layers, list(ggplot2::geom_point(
        data = d,
        mapping = ggplot2::aes(fill = .data[["value"]], size = .data[["abs_corr"]]),
        color = outline.color, shape = 22
      )))
    } else if (m == "circle") {
      has_sized <- TRUE
      layers <- c(layers, list(ggplot2::geom_point(
        data = d,
        mapping = ggplot2::aes(fill = .data[["value"]], size = .data[["abs_corr"]]),
        color = outline.color, shape = 21
      )))
    } else if (m == "number") {
      layers <- c(layers, list(ggplot2::geom_text(
        data = d,
        mapping = ggplot2::aes(colour = .data[["value"]]),
        label = .format_coef(d$value, digits, nsmall, leading.zero),
        size = lab_size
      )))
    } else if (m == "name") {
      layers <- c(layers, list(ggplot2::geom_text(
        data = d,
        mapping = ggplot2::aes(label = .data[["Var1"]]),
        colour = if (is.null(tl.col)) "black" else tl.col,
        size = tl.cex / 3
      )))
    }
  }

  if (has_sized) {
    layers <- c(layers, list(
      ggplot2::scale_size(range = c(4, 10) * circle.scale),
      ggplot2::guides(size = "none")
    ))
  }

  # Each region is a separate layer over a subset of the cells, so a variable
  # absent from one region's subset (e.g. the first variable never appears in the
  # lower triangle) would otherwise make ggplot infer that axis's order from the
  # cells it does see, misaligning the two axes and bending the diagonal. Pin both
  # axes to the full variable order so every cell lands on the shared grid.
  lvls_x <- if (is.factor(df$Var1)) levels(df$Var1) else unique(as.character(df$Var1))
  lvls_y <- if (is.factor(df$Var2)) levels(df$Var2) else unique(as.character(df$Var2))
  layers <- c(layers, list(
    ggplot2::scale_x_discrete(limits = lvls_x, drop = FALSE),
    ggplot2::scale_y_discrete(limits = lvls_y, drop = FALSE)
  ))

  layers
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
  if (nrow(cormat) == ncol(cormat)) {
    diag(cormat) <- NA
  } else {
    # A non-square (m x n) matrix has no positional diagonal: `diag(cormat) <- NA`
    # would blank min(m, n) cells at (1,1), (2,2), ... regardless of what variables
    # sit there, wiping the wrong cells. Remove only the genuine self-pairs -- cells
    # whose row and column name the same variable -- so a matrix whose row and column
    # variables are disjoint keeps every cell.
    rn <- rownames(cormat)
    cn <- colnames(cormat)
    if (!is.null(rn) && !is.null(cn)) {
      cormat[outer(rn, cn, `==`)] <- NA
    } else {
      # Unnamed matrix: there are no names to identify self-pairs by, so keep the
      # historical positional removal (unchanged from previous behavior).
      diag(cormat) <- NA
    }
  }
  cormat
}

# Named colorblind-safe diverging palettes, as hardcoded hex vectors (the package
# targets R >= 3.3, which predates hcl.colors(), and Imports are kept minimal, so
# the values are not computed at run time). Each is an 11-stop ColorBrewer
# diverging ramp with white at the center, ordered low -> high (cool = negative,
# warm = positive) to match the default colors = c("blue", "white", "red")
# polarity. Verified against RColorBrewer::brewer.pal(11, .) at development time.
.ggcorrplot_palette <- function(name) {
  switch(name,
    RdBu = c(
      "#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
      "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"
    ),
    PuOr = c(
      "#2D004B", "#542788", "#8073AC", "#B2ABD2", "#D8DAEB", "#F7F7F7",
      "#FEE0B6", "#FDB863", "#E08214", "#B35806", "#7F3B08"
    )
  )
}
# Darken a vector of colors just enough to be readable as TEXT on a white panel,
# keeping each color's hue. Used only for the mixed "number" glyph, whose text is
# mapped onto the same diverging ramp as the fill: that ramp is white at zero, so
# an unmodified near-zero coefficient renders invisible.
#
# Relative luminance is the WCAG definition (linearised sRGB, Rec. 709 weights).
# A color already at or below the cap is returned unchanged (a saturated blue, a
# dark ramp end); everything else is scaled down, the pale middle by far the most
# and a stop only just over the cap barely at all. Scaling the linear channels by
# a common factor preserves the hue. The
# cap keeps the contrast ratio against white above 4.5:1, the usual threshold for
# small text, with enough headroom that rounding to 8-bit channels cannot push a
# stop back under it.
#
# A partly transparent stop is left alone: its luminance over the panel depends on
# what it is composited with, so darkening it by its opaque value would neither
# honour the contrast target nor keep the ramp's alpha uniform. Whatever the
# caller asked for is drawn unchanged, exactly as the fill scale draws it.
.legible_text_colors <- function(colors, max_luminance = 0.179) {
  unlist(lapply(colors, function(col) {
    rgba <- grDevices::col2rgb(col, alpha = TRUE)[, 1] / 255
    if (is.na(rgba[4]) || rgba[4] < 1) {
      return(col)
    }
    chan <- .linearise_srgb(rgba[1:3])
    lum <- sum(c(0.2126, 0.7152, 0.0722) * chan)
    if (is.na(lum) || lum <= max_luminance) {
      return(col)
    }
    chan <- chan * (max_luminance / lum)
    # back to sRGB
    srgb <- ifelse(chan <= 0.0031308, chan * 12.92, 1.055 * chan^(1 / 2.4) - 0.055)
    grDevices::rgb(srgb[1], srgb[2], srgb[3])
  }), use.names = FALSE)
}

# sRGB -> linear light, the first half of the WCAG relative-luminance definition.
.linearise_srgb <- function(u) {
  ifelse(u <= 0.03928, u / 12.92, ((u + 0.055) / 1.055)^2.4)
}

.relative_luminance <- function(col) {
  chan <- .linearise_srgb(grDevices::col2rgb(col)[, 1] / 255)
  sum(c(0.2126, 0.7152, 0.0722) * chan)
}

# Is the background behind the glyphs light enough that DARK text reads against
# it? The legibility floor only helps there: on a dark background the ramp's pale
# middle is already the READABLE end (white on grey50 clears 3.9:1), and darkening
# it would bury the text instead. So the floor is applied over a light background
# only, and a dark theme keeps the plain ramp.
#
# What sits behind a cell is panel.background when the theme draws one, and
# plot.background when it does not -- theme_minimal (our default) and theme_void
# blank the panel, so a dark recipe built on them carries its color on
# plot.background. Both are consulted, in that order, and each is resolved through
# the theme's inheritance before falling back to the raw element. A background we
# cannot resolve at all counts as light, which is the default device background.
#
# This sees the theme passed as `ggtheme`, resolved against the active default, so
# a globally theme_set() background counts too. A theme added to the finished plot
# with `+` arrives long after the scale is built, so a dark theme must be passed
# here to be taken into account; that limitation is documented on the arguments.
#
# The threshold is where the two branches break even FOR THE NEAR-ZERO STOP -- the
# pale middle of the ramp, which is the one the floor exists to rescue and the one
# that vanishes first. It is not a promise about the ramp as a whole: against a
# mid-grey panel a white-centred diverging ramp always has some intermediate stop
# at roughly the panel's own luminance, so neither branch reads well there and no
# threshold rescues it. Away from mid-grey, in both directions, the branch this
# picks is comfortably the better one.
.panel_is_light <- function(ggtheme, threshold = 0.44) {
  fill <- tryCatch(
    {
      th <- if (is.function(ggtheme)) ggtheme() else ggtheme
      # ggplot2 draws theme_get() + this theme, so a PARTIAL theme -- one that sets
      # a background without carrying a base theme -- is only half the story: the
      # elements it leaves out come from the active default. Resolve against that
      # default first, or a theme setting nothing but a dark plot.background looks
      # dark while the panel actually drawn is the default's light grey (and a
      # globally theme_set() dark default looks light). Anything that is not a
      # theme at all (NULL, a stray value, a function returning one) is ignored
      # when the plot is assembled, so what gets drawn is the active default on
      # its own -- read that, rather than falling through to the bare page.
      th <- if (inherits(th, "theme")) ggplot2::theme_get() + th else ggplot2::theme_get()
      # What a cell actually sits on, painted in the order the device paints it:
      # the white page, then plot.background, then panel.background. Compositing
      # rather than picking one element is what makes a partly transparent
      # background come out as the color the eye sees.
      .composite_over(
        .theme_element_fill(th, "panel.background"),
        .composite_over(.theme_element_fill(th, "plot.background"), "white")
      )
    },
    error = function(e) NULL
  )
  if (is.null(fill)) {
    return(TRUE)
  }
  lum <- tryCatch(.relative_luminance(fill), error = function(e) NA_real_)
  is.na(lum) || lum > threshold
}

# The fill of one theme element, exactly as the theme gives it (alpha included):
# resolved through the theme's inheritance where that works (a fill set on `rect`
# reaches `panel.background` that way), and read off the element directly
# otherwise. NULL when the element paints nothing usable -- blank, missing, NA, or
# not a single color.
.theme_element_fill <- function(th, element) {
  fill <- tryCatch(ggplot2::calc_element(element, th)$fill, error = function(e) NULL)
  if (is.null(fill)) {
    fill <- tryCatch(th[[element]]$fill, error = function(e) NULL)
  }
  if (is.null(fill) || length(fill) != 1L || is.na(fill)) {
    return(NULL)
  }
  fill
}

# Paint `fill` onto `backdrop` and return the resulting color. A fill that is
# absent or fully transparent paints nothing, so the backdrop shows through
# unchanged -- theme_void's plot.background is literally transparent black, which
# taken at face value would be the darkest background there is. A partly
# transparent fill is blended, so a 90%-opaque near-black panel is read as the
# near-black the eye sees rather than discarded for not being opaque.
.composite_over <- function(fill, backdrop) {
  if (is.null(fill)) {
    return(backdrop)
  }
  rgba <- tryCatch(grDevices::col2rgb(fill, alpha = TRUE)[, 1], error = function(e) NULL)
  if (is.null(rgba) || anyNA(rgba) || rgba[4] == 0) {
    return(backdrop)
  }
  if (rgba[4] == 255) {
    return(fill)
  }
  under <- tryCatch(grDevices::col2rgb(backdrop)[, 1], error = function(e) c(255, 255, 255))
  opacity <- rgba[4] / 255
  blended <- rgba[1:3] * opacity + under * (1 - opacity)
  grDevices::rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
}


# hc.order correlation matrix. Returns the whole hclust object (its $order gives
# the reordering; the tree itself is needed to draw cluster rectangles, hc.rect).
.hc_cormat_order <- function(cormat, hc.method = "complete") {
  dd <- stats::as.dist((1 - cormat) / 2)
  stats::hclust(dd, method = hc.method)
}

# Contiguous block bounds (in dendrogram-order axis positions 1..n) for the k
# clusters of an hclust tree. cutree() labels each variable with its cluster;
# reindexing by hc$order puts them in the same order as the plotted axes, where
# each cluster is guaranteed to be a contiguous run (a property of the dendrogram
# leaf order), so rle() gives exactly k blocks. Returns lo/hi position of each.
.hc_rect_bounds <- function(hc, k) {
  cl <- stats::cutree(hc, k = k)[hc$order]
  runs <- rle(cl)
  hi <- cumsum(runs$lengths)
  lo <- hi - runs$lengths + 1L
  list(lo = lo, hi = hi)
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
