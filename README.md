<!-- badges: start -->

[![GitHub last commit](https://img.shields.io/github/last-commit/dankelley/ocetag)](https://img.shields.io/github/last-commit/dankelley/ocetag)
[![R-CMD-check](https://github.com/dankelley/ocetag/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dankelley/ocetag/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

This package provides e suite of functions for organizing information about
oceanographic data that are in the form of tags referenced to data features.
There is great flexibility in the nature of those tags.  For example, in
working with CTD data, an analyst might want to employ tags as used by the Argo
program, as a way to indicate untrustworthy data. Another example might involve
tags referring to features of interest, such as the mixed-layer depth or some
feature in temperature-salinity space.

The tags are stored in an SQLite database, for speed and efficiency. Another
advantage of the format over simpler formats is that altering the files takes
some effort, so changes are unlikely to come from someone double-clicking on
the file and accidentally changing the contents.

The package offers low-level functions, such as `createDatabase()` and `saveTag()`,
as well as a high-level Shiny app named `ctdTag()` that lets the analyst explore
CTD data in flexible ways, to discover and tag features of interest.


