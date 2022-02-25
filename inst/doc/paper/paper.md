---
title: 'r2spss: Format R output to look like SPSS for use in teaching materials'
tags:
- R
- SPSS
- statistics
- teaching materials
date: "21 November 2021"
output:
  html_document:
    df_print: paged
authors:
- name: Andreas Alfons^[Corresponding author]
  orcid: 0000-0002-2513-3788
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: Erasmus School of Economics, Erasmus University Rotterdam, Netherlands
  index: 1
---


# Summary

With a focus on creating teaching materials, the add-on package `r2spss` [@r2spss] for the statistical computing environment `R` [@R] allows to create graphics and `LaTeX` [e.g., @LaTeX] tables that to look like output of the statistical software platform `SPSS` [@SPSS].  The package provides functionality for techniques that are typically covered in introductory statistics classes: descriptive statistics, common hypothesis tests, ANOVA, and linear regression, as well as box plots, histograms, scatter plots, and line plots (including profile plots).

<!--
The package allows to create tables and graphics that mimic recent versions of SPSS, as well as a simpler look of older versions (`SPSS` changed the look of the output in version 24) (TODO: check that it was indeed in version 24).
-->


# Statement of need

Many academic programs in the social sciences or economics require to teach statistics with `SPSS` [@SPSS].  Preparing teaching materials in this case typically involves copying-and-pasting `SPSS` output into documents or slides, which is cumbersome and prone to errors.  Moreover, this approach is not scalable for regular updates of the materials, or for individualizing assignments and exams in order to combat fraud.  On the other hand, tools such as package `knitr` [@xie15; @knitr] for integrating the statistical computing environment `R` [@R] and the document preparation system `LaTeX` [e.g., @LaTeX] make preparing teaching materials easier, less error-prone, and more scalable.  There are even specialized tools such as package `exams` [@gruen09; @zeileis14; @exams] that allow assignments and exams to be individualized in a scalable manner.  Package `r2spss` makes it possible to leverage those developments for creating teaching materials with `SPSS` output by mocking up such output with `R`.


# LaTeX requirements

<!--
Style file `r2spss.sty`, which includes all `LaTeX` requirements, can be created with  function `r2spss.sty()` from package `r2spss`. Then the command `\usepackage{r2spss}` should be included in the preamble of the `LaTeX` document. Tables that mimic recent version of `SPSS` output may require several compilations of the `LaTeX` document in order to be displayed correctly.
-->


# Example: Linear regression


<!--
Newer versions of SPSS: output more fancy but also more cluttered (e.g., headers in color, different line thicknesses, different shades of gray for different parts in a table).  Package `r2spss` replicates the simple style of older versions of SPSS, which I find cleaner and less distracting for teaching materials in statistics courses.
-->


# Customization and extensions

<!-- 
Objects returned by functions in `r2spss` are typically stored as returned by the corresponding `R` functions, e.g., regression results as "lm" objects.  This is such that users can manipulate those objects in R as they are used to.  The `print()` methods to generate the `LaTeX` tables consist of two building blocks: function `toSPSS()` to convert the results into SPSS syle tables, and `latexTableSPSS()` to generate the corresponding `LaTeX` tables.  These can be used to customize the output or to create SPSS-like output for functionality not implemented in package `r2spss`.
-->

# Acknowledgements

Andreas Alfons is partially supported by a grant of the Dutch Research Council 
(NWO), research program Vidi (project number VI.Vidi.195.141).


# References
