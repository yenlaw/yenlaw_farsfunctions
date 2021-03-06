---
title: "farsfunctions"
author: "yenlaw"
date: "May 6, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Summary

The farsfunctions package gives the ability to read and visualise events provided by thethe National Highway Traffic Safety Administration (NHTSA) Fatality Analysis Reporting System (FARS) data.  

The package contains five functions - each function is defined below.

### Build Status

![](https://travis-ci.org/yenlaw/yenlaw_farsfunctions.svg?branch=master)

## fars_read

Reads a given file - of a FARS data format - specified by the filename

## make_filename

Create a filename for a given year with format "accident_YEAR.csv.bz2"

## fars_read_years

Read multiple .csv data file for a integer series of provided years

## fars_summarize_years

Summarises data on month and given year from a FARS data format

## fars_map_state

Creates a map visualising all accident events in a given State for a paricular year.
