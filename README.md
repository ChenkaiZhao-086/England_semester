# England_semester
This repository contains the code for the paper: **[University reopening and COVID-19 in the UK: population mobility, not student return, drove autumn resurgence](https://..com/)**

**Authors:** Chenkai Zhao, Peter Sloan, Xiaoyu Xu, Ruth McQuillan, Harry Campbell, You Li

**Corresponding author:** You Li, You.Li@njmu.edu.cn

All the fitting, analysis and figure generation can be run from "main.R". This loads in all the dependencies and functions. It has to be run in the order specified, and opens any files where manual edits can be made (e.g. type of simulation outcome). All the code and data (which is publically available) are in the repo, and can be run directly from the master script.

**IMPORTANT:**
- Please confirm that **Rtools** is installed on your computer, otherwise the code cannot run.
- Please install the necessary packages at first.
- This analysis uses parallel computing. Please modify it according to the number of CPU cores you wish to use.

Input files: <br>
England_hall-v1.4.xlsx: LTLAs, HEIs and halls data. <br>

Output files - see paper for references:

When the code runs, it will generate a file path named Output, and all running results will be saved to this path. 
Parameter inference and simulation of the model require a long time. It takes about 8 hours to run parameter inference for one scenario, and about 20 minutes to run one simulation.

All analyses were completed on a MacBook with an M2 MAX chip and 64GB of memory. The session info was as follow:
```
R version 4.4.2
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.6.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Asia/Shanghai
tzcode source: internal

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggh4x_0.3.0       scales_1.3.0      coda_0.19-4.1     extraDistr_1.10.0 truncnorm_1.0-9   progress_1.2.3    doParallel_1.0.17 iterators_1.0.14  foreach_1.5.2    
[10] lubridate_1.9.4   forcats_1.0.0     stringr_1.5.1     dplyr_1.1.4       purrr_1.0.2       readr_2.1.5       tidyr_1.3.1       tibble_3.2.1      ggplot2_3.5.1    
[19] tidyverse_2.0.0   data.table_1.16.4 readxl_1.4.3      deSolve_1.40      Rcpp_1.0.13-1    

loaded via a namespace (and not attached):
 [1] gtable_0.3.6      jsonlite_1.8.9    crayon_1.5.3      compiler_4.4.2    tidyselect_1.2.1  lattice_0.22-6    R6_2.6.1          generics_0.1.3    munsell_0.5.1    
[10] pillar_1.10.1     tzdb_0.4.0        rlang_1.1.5       stringi_1.8.4     timechange_0.3.0  cli_3.6.4         withr_3.0.2       magrittr_2.0.3    grid_4.4.2       
[19] hms_1.1.3         lifecycle_1.0.4   prettyunits_1.2.0 vctrs_0.6.5       glue_1.8.0        cellranger_1.1.0  codetools_0.2-20  colorspace_2.1-1  tools_4.4.2      
[28] pkgconfig_2.0.3
```
See LICENSE file for licensing details.