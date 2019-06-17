<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Last-changedate](https://img.shields.io/badge/last%20change-2019--06--12-brightgreen.svg)](https://github.com/bramzandbelt/cmdsddfeitc/commits/master) [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.6.0-brightgreen.svg)](https://cran.r-project.org/) [![Task DOI](https://zenodo.org/badge/125838088.svg)](https://zenodo.org/badge/latestdoi/125838088) [![Code licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0002--6491--1247-green.svg)](https://orcid.org/0000-0002-6491-1247)

cmdsddfeitc - Research compendium for the report on the cognitive mechanisms of the defer-speedup and date-delay framing effects in intertemporal choice by Zandbelt
====================================================================================================================================================================

Compendium DOI
--------------

<!-- TODO: Add Zenodo DOI -->
The files at the URL above will generate the results as found in the preprint. The files hosted at <https://github.com/bramzandbelt/cmdsddfeitc/> are the development versions and may have changed since the preprint was published.

Author of this repository
-------------------------

Bram Zandbelt (<bramzandbelt@gmail.com>)

Published in:
-------------

TBA <!-- TODO: Add psyRxiv DOI -->

Overview
--------

The packagae `cmdsddfeitc` is a research compendium of the research project *Cognitive Mechanisms of the Defer-Speedup and Date-Delay Framing Effects in Intertemporal Choice* by Bram Zandbelt. This project was conducted at the Donders Institute, Radboud University / Radboucumc, Nijmegen, the Netherlands, and registered at the Donders Centre for Cognitive Neuroimaging under project number (DCCN PI: Roshan Cools).

This research compendium contains all data, code, and text associated with the above-mentioned publication. It is organized as follows:

    .
    ├── R
    ├── analysis
    ├── data
    ├── documents
    ├── figures
    ├── man
    ├── metadata
    ├── opt
    ├── packrat
    └── reports

The `R/` directory contains:

-   R code specific to the present project; functions are organized into files (e.g. functions for plotting are in `plot_functions.R`)

The `analysis/` directory contains:

-   R Markdown notebooks implementing the analyses (`notebook_templates/` directory), numbered in the order in which they should be run;
-   shell scripts running the R Markdown notebooks with appropriate parameters, if any (`bash/` directory).

The `data/` directory contains:

-   the raw performance data (`raw/` directory);
-   the data derived from the raw data (`derivatives/` directory), organized by notebook name.

The `documents/` directory contains:

-   documents describing the content of the experimental data (`content/` directory);
-   documents describing the context of the data (`context/` directory);
-   documents related to the report of this research project (`manuscript/` directory).

The `figures/` directory contains:

-   visualizations of descriptive and inferential statistics, organized by notebook name.

The `man/` directory contains:

-   documentation of objects inside the package, generated by `roxygen2`.

The `packrat/` directory contains:

-   R packages the research compendium depends on; for more info see <https://rstudio.github.io/packrat/>.

The `reports/` directory contains:

-   static HTML versions of the knitted R Markdown notebooks, organized by notebook name.

Finally, this research compendium is associated with a number of online objects, including:

<table>
<colgroup>
<col width="9%" />
<col width="45%" />
<col width="45%" />
</colgroup>
<thead>
<tr class="header">
<th>object</th>
<th>archived version</th>
<th>development version</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>preregistration</td>
<td><a href="https://osf.io/rzqh9/" class="uri">https://osf.io/rzqh9/</a></td>
<td>NA</td>
</tr>
<tr class="even">
<td>data management plan</td>
<td><a href="https://doi.org/10.6084/m9.figshare.4720978" class="uri">https://doi.org/10.6084/m9.figshare.4720978</a></td>
<td>NA</td>
</tr>
<tr class="odd">
<td>stimulus presentation code</td>
<td><a href="https://doi.org/10.5281/zenodo.3243777" class="uri">https://doi.org/10.5281/zenodo.3243777</a></td>
<td><a href="github.com/bramzandbelt/itch_time_framing_task" class="uri">github.com/bramzandbelt/itch_time_framing_task</a></td>
</tr>
<tr class="even">
<td>cognitive modeling code</td>
<td><a href="https://doi.org/10.5281/zenodo.3243806" class="uri">https://doi.org/10.5281/zenodo.3243806</a></td>
<td><a href="https://github.com/bramzandbelt/itchmodel" class="uri">https://github.com/bramzandbelt/itchmodel</a></td>
</tr>
</tbody>
</table>

How to use
----------

This repository is organized as an R package, called `cmdsddfeitc`. The R package structure was used to help manage dependencies, to take advantage of continuous integration for automated code testing and documentation, and to be able to follow a standard format for file organization. The package `cmdsddfeitc` depends on other R packages and non-R programs, which are listed below under [Dependencies](#Dependencies).

To download the package source as you see it on GitHub, for offline browsing, use this line at the shell prompt (assuming you have Git installed on your computer):

Install `cmdsddfeitc` package from Github:

``` r
devtools::install_github("bramzandbelt/cmdsddfeitc")
```

Once the download is complete, open the file `cmdsddfeitc.Rproj` in RStudio to begin working with the package and compendium files. To reproduce all analyses, run the shell script `analysis/bash/run_all_analyses.sh`. This will run all RMarkdown notebooks in correct order. Note, however, that this will *not* reproduce the computational modeling analyses performed in the document `03_computational_modeling_analysis.Rmd`), only the result of the optimizations. This is because optimization of all 708 models (59 participants (defer-speedup, N = 28; date-delay, N = 31), 6 parameterizations, 2 architectures) was done on a computer cluster and would take simply too long to run. In order to reproduce the computational modeling analyses, run `03_computational_modeling_analysis.Rmd` as a parameterized report with argument `optimize=TRUE`.

Licenses
--------

Manuscript: CC-BY-4.0 <http://creativecommons.org/licenses/by/4.0/>

Code: MIT <http://opensource.org/licenses/MIT>, year: 2019, copyright holder: Bram B. Zandbelt

Data: Data Use Agreement of Donders Institute <!-- TODO: Add URL -->

Dependencies
------------

Below is the output of `sessionInfo()`, showing version information about R, the OS, and attached or loaded packages:

``` r
sessionInfo()
#> R version 3.6.0 (2019-04-26)
#> Platform: x86_64-apple-darwin15.6.0 (64-bit)
#> Running under: macOS Mojave 10.14.5
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
#> LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_3.6.0  magrittr_1.5    htmltools_0.3.6 tools_3.6.0    
#>  [5] yaml_2.2.0      Rcpp_1.0.1      stringi_1.4.3   rmarkdown_1.13 
#>  [9] knitr_1.23      stringr_1.4.0   xfun_0.7        digest_0.6.19  
#> [13] packrat_0.4.9-3 evaluate_0.14
```

Packrat takes care of dependencies.

Acknowledgment
--------------

This research project was funded through European Union’s Horizon 2020 research and innovation programme under the Marie Skłodowska-Curie grant agreement No. 703141 to Bram B. Zandbelt.

Thanks to Ben Marwick for inspiration on [how to create, organize, and describe research compendia](https://github.com/benmarwick/researchcompendium).

Contact
-------

[Bram B. Zandbelt](mailto:bramzandbelt@gmail.com)

``` r
# Ignore rest of document
knitr::knit_exit()
```
