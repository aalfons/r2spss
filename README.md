# r2spss: Format R Output to Look Like SPSS

[![CRAN](https://www.R-pkg.org/badges/version/r2spss)](https://CRAN.R-project.org/package=r2spss) 


Create plots and `LaTeX` tables that look like `SPSS` output for use in teaching materials.  Rather than copying-and-pasting `SPSS` output into documents, `R` code that mocks up `SPSS` output can be integrated directly into dynamic `LaTeX` documents with tools such as [`knitr`](https://yihui.org/knitr/).  Functionality includes methods that are typically covered in introductory statistics classes: descriptive statistics, common hypothesis tests, ANOVA, and linear regression, as well as boxplots, histograms, scatterplots, and line plots (including profile plots).


## Installation

To install the latest (possibly unstable) development version from GitHub, you can pull this repository and install it from the `R` command line via

```
install.packages("devtools")
devtools::install_github("aalfons/r2spss")
```

If you already have package `devtools` installed, you can skip the first line.


## LaTeX requirements and knitr options

Some of the tables produced by `r2spss` require the `LaTeX` package `amsmath`, hence the following command should be included in the preamble of your `LaTeX` document.

```
% somewhere before \begin{document}
\usepackage{amsmath}
```

When creating `LaTeX` tables in `R` code chunks with `knitr`, the output of the chunk should be written directly into the output document by setting the chunk option `results='asis'`.  For more information on `knitr` chunk options, in particular various options for figures, please consult the [knitr documentation](https://yihui.org/knitr/options/).


## Package vignette

Various examples for using `r2spss` are given in the package vignette, which 
can be accessed from the `R` console with

```
vignette("r2spss-intro")
```


## Community guidelines

### Report issues and request features

If you experience any bugs or issues or if you have any suggestions for additional features, please submit an issue via the [*Issues*](https://github.com/aalfons/r2spss/issues) tab of this repository.  Please have a look at existing issues first to see if your problem or feature request has already been discussed.

### Contribute to the package

If you want to contribute to the package, you can fork this repository and create a pull request after implementing the desired functionality.

### Ask for help

If you need help using the package, or if you are interested in collaborations related to this project, please get in touch with the [package maintainer](https://personal.eur.nl/alfons/).
