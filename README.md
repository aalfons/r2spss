# r2spss: Format R Output to Look Like SPSS

[![CRAN](https://www.R-pkg.org/badges/version/r2spss)](https://CRAN.R-project.org/package=r2spss) 


Create plots and `LaTeX` tables that look like `SPSS` output for use in teaching materials.  Rather than copying-and-pasting `SPSS` output into documents, `R` code that mocks up `SPSS` output can be integrated directly into dynamic `LaTeX` documents with tools such as [`knitr`](https://yihui.org/knitr/).  Functionality includes statistical techniques that are typically covered in introductory statistics classes: descriptive statistics, common hypothesis tests, ANOVA, and linear regression, as well as box plots, histograms, scatter plots, and line plots (including profile plots).


## Installation

Package `r2spss` is on CRAN (The Comprehensive R Archive Network), hence the latest release can be easily installed from the `R` command line via

```
install.packages("r2spss")
```


## Building from source

To install the latest (possibly unstable) development version from GitHub, you can pull this repository and install it from the `R` command line via

```
install.packages("devtools")
devtools::install_github("aalfons/r2spss")
```

If you already have package `devtools` installed, you can skip the first line.


## LaTeX requirements

`LaTeX` tables created with package `r2spss` build upon several `LaTeX` packages.  A `LaTeX` style file that includes all requirements can be produced with function `r2spss.sty()`.  By default, it prints the content of the style file on the `R` console, but its only argument `path` can be used to specify the path to a folder in which to put the file *r2spss.sty*.  For instance, the following command can be used to put the style file in the current working directory.

```
r2spss.sty(path = ".")
```

After putting the style file in the folder that contains your `LaTeX` document, the following command should be included in the preamble of your `LaTeX` document, i.e., somewhere in between `\documentclass{}` and `\begin{document}`.

```
\usepackage{r2spss}
```

## Dynamic documents and knitr options

Package `r2spss` is the most useful when writing dynamic `LaTeX` documents with
tools such as the `R` package [`knitr`](https://yihui.org/knitr/).  When creating `LaTeX` tables in `R` code chunks with `knitr`, the output of the chunk should be written directly into the output document by setting the chunk option `results='asis'`.  For more information on `knitr` chunk options, in particular various options for figures, please consult the [knitr documentation](https://yihui.org/knitr/options/).


## Mimicking different SPSS versions

Package `r2spss` can create output that mimics the look of current `SPSS` versions, as well as the look of older versions.  The relevant functions contain the argument `version` for specifying which type of output to create.  Possible values are `"modern"` to mimic recent versions and `"legacy"` to mimic older versions.  `LaTeX` tables that mimic the look of recent SPSS version thereby build upon the `LaTeX` package [`nicematrix`](https://ctan.org/pkg/nicematrix) and its `NiceTabular` environment, which is preferred for its seamless display of background colors in the table.

However, `r2spss` requires `nicematrix` version 6.5 (2022-01-23) or later.  It is also important to note that tables using the `NiceTabular` environment may require several `LaTeX` compilations to be displayed correctly.  

Within a dynamic `LaTeX` document or any other `R` session, it can be useful to set a global preference for which `SPSS` version to mimic.  This can be done with the accessor function `r2spss_options$set()`.  For instance, a default to mimic older `SPSS` versions can be set with:

```
r2spss_options$set(version = "legacy")
```


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
