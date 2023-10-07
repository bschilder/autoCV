autoCV
================
<img src='https://github.com/bschilder/autoCV/raw/master/inst/hex/hex.png' title='Hex sticker for autoCV' height='300'><br>
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](https://img.shields.io/badge/devel%20version-0.99.0-black.svg)](https://github.com/bschilder/autoCV)
[![](https://img.shields.io/github/languages/code-size/bschilder/autoCV.svg)](https://github.com/bschilder/autoCV)
[![](https://img.shields.io/github/last-commit/bschilder/autoCV.svg)](https://github.com/bschilder/autoCV/commits/master)
<br> [![R build
status](https://github.com/bschilder/autoCV/workflows/rworkflows/badge.svg)](https://github.com/bschilder/autoCV/actions)
[![](https://codecov.io/gh/bschilder/autoCV/branch/master/graph/badge.svg)](https://app.codecov.io/gh/bschilder/autoCV)
<br>
<a href='https://app.codecov.io/gh/bschilder/autoCV/tree/master' target='_blank'><img src='https://codecov.io/gh/bschilder/autoCV/branch/master/graphs/icicle.svg' title='Codecov icicle graph' width='200' height='50' style='vertical-align: top;'></a>  
<h4>  
Authors: <i>Brian Schilder</i>  
</h4>
<h4>  
README updated: <i>Oct-07-2023</i>  
</h4>

<!-- To modify Package/Title/Description/Authors fields, edit the DESCRIPTION file -->

## `autoCV`: Automated Curriculum Vitae

### Automatically generate and style your CV from tables.

## Installation

``` r
if(!require("remotes")) install.packages("remotes")

remotes::install_github("bschilder/autoCV")
library(autoCV)
```

## Documentation

### [Website](https://bschilder.github.io/autoCV)

### [Getting started](https://bschilder.github.io/autoCV/articles/autoCV)

<hr>

## Contact

### [Neurogenomics Lab](https://www.neurogenomics.co.uk/)

UK Dementia Research Institute  
Department of Brain Sciences  
Faculty of Medicine  
Imperial College London  
[GitHub](https://github.com/neurogenomics)  
[DockerHub](https://hub.docker.com/orgs/neurogenomicslab)

<br>

## Session Info

<details>

``` r
utils::sessionInfo()
```

    ## R version 4.3.1 (2023-06-16)
    ## Platform: aarch64-apple-darwin20 (64-bit)
    ## Running under: macOS Ventura 13.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.4        jsonlite_1.8.7      renv_1.0.3         
    ##  [4] dplyr_1.1.3         compiler_4.3.1      BiocManager_1.30.22
    ##  [7] tidyselect_1.2.0    rvcheck_0.2.1       scales_1.2.1       
    ## [10] yaml_2.3.7          fastmap_1.1.1       here_1.0.1         
    ## [13] ggplot2_3.4.3       R6_2.5.1            generics_0.1.3     
    ## [16] knitr_1.44          yulab.utils_0.1.0   tibble_3.2.1       
    ## [19] desc_1.4.2          dlstats_0.1.7       rprojroot_2.0.3    
    ## [22] munsell_0.5.0       pillar_1.9.0        RColorBrewer_1.1-3 
    ## [25] rlang_1.1.1         utf8_1.2.3          cachem_1.0.8       
    ## [28] badger_0.2.3        xfun_0.40           fs_1.6.3           
    ## [31] memoise_2.0.1       cli_3.6.1           magrittr_2.0.3     
    ## [34] rworkflows_0.99.14  digest_0.6.33       grid_4.3.1         
    ## [37] rstudioapi_0.15.0   lifecycle_1.0.3     vctrs_0.6.3        
    ## [40] data.table_1.14.8   evaluate_0.22       glue_1.6.2         
    ## [43] fansi_1.0.4         colorspace_2.1-0    rmarkdown_2.25     
    ## [46] tools_4.3.1         pkgconfig_2.0.3     htmltools_0.5.6

</details>
