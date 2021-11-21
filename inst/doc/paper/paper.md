---
title: 'r2spss: Format R output to look like SPSS for use in teaching materials'
tags:
- R
- SPSS
- statistics
- teaching
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

Package `r2spss` [@r2spss] provides functionality for techniques typically covered in introductory statistics classes: descriptive statistics, common hypothesis tests, ANOVA, and linear regression, as well as boxplots, histograms, scatterplots, and line plots (including profile plots).


# Statement of need

Many academic programs in the social sciences or economics require to teach statistics with `SPSS` [@SPSS]. Preparing teaching materials in this case typically involves to copying-and-pasting `SPSS` output into documents or slides, which is cumbersome and prone to errors.  Moreover, this approach is not scalable for regular updates of the materials, or for individualizing assignments and exams in order to combat fraud.  On the other hand, tools such as `knitr` [@xie15; @knitr] for integrating the statistical computing environment `R` [@R] and the document preparation system `LaTeX` [e.g., @LaTeX] make preparing teaching materials easier, less error-prone, and more scalable. There are even specialized tools such as package `exams` [@gruen09; @zeileis14; @exams] that allow assignments and exams to be individualized in a scalable manner.  Package `r2spss` makes it possible to leverage those developments for creating teaching materials with `SPSS` output by mocking up such output with `R`.


# Example: Linear regression


<!--
Newer versions of SPSS: output more fancy but also more cluttered (e.g., headers in color, different line thicknesses, different shades of gray for different parts in a table).  Package `r2spss` replicates the simple style of older versions of SPSS, which I find cleaner and less distracting for teaching materials in statistics courses.
-->


# Acknowledgements

Andreas Alfons is partially supported by a grant of the Dutch Research Council 
(NWO), research program Vidi (project number VI.Vidi.195.141).


# References
