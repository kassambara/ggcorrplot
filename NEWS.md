# ggcorrplot 0.1.3
  
## New features
   
- Support an object of class `cor_mat` as returned by the function `cor_mat()` [rstatix package]

## Minor changes
   
Merging with pull request 16 (@IndrajeetPatil, [#16](https://github.com/kassambara/ggcorrplot/pull/16)), which addresses the following issues: 

1. In all `README` and `roxygen` examples, the argument `outline.color` was written as `outline.col`, which created `warnings` in `RStudio` scripts about the partial matching of arguments. Fixed that.
2. Styled the code in `tidyverse` style guide (both in `R` script and `README` file).
3. Added spelling tests to make sure no spelling error fall through the cracks.
4. Bumped up the package version to highlight that this is the development version. Added a few more badges to `README` to convey the same thing. 
5. The `digits` argument (introduced in #12) wasn't working properly (https://github.com/IndrajeetPatil/ggstatsplot/issues/93).  This is now fixed. Also added an example to show that this works.


## Bug fixes
   
- When `insig = "blank"` correlation labels are no longer displayed for insignificant correlations (@axitamm, [#17](https://github.com/kassambara/ggcorrplot/pull/17))

# ggcorrplot 0.1.2
   
   
## Minor changes
   
- New argument `digits` added to `ggcorrplot()` (@IndrajeetPatil, [#12](https://github.com/kassambara/ggcorrplot/pull/12).
- New argument ggtheme added to `ggcorrplot()` (@IndrajeetPatil, [#11](https://github.com/kassambara/ggcorrplot/pull/11).
   
## Bug fixes
   
- Bug fix for label argument inside ggplot2::geom_text (@alekrutkowski, [#1](https://github.com/kassambara/ggcorrplot/pull/1))
- Now `ggcorrplot()` when both reshape and reshape2 packages are loaded ([#4](https://github.com/kassambara/ggcorrplot/issues/4)) 


# ggcorrplot 0.1.1


## New features
   
- ggcorrplot(): visualize a correlation matrix
- cor_pmat(): compute a correlation matrix p-values
