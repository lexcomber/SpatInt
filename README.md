# Spatial interpolation using areal features: a review of methods and opportunities using new forms of data with coded illustrations
Coded examples in R of the different approaches described in *Geography Compass*. 2019; e12465, https://doi.org/10.1111/gec3.12465

Alexis Comber<sup>1</sup>, Wen Zeng<sup>1</sup>,<sup>2</sup>

<sup>1</sup>School of Geography, University of Leeds, Leeds, LS2 9JT, UK. Email: a.comber@leeds.ac.uk

<sup>2</sup>Shandong University of Science and Technology, Qingdao, P.R. China. Email: alvin_z@163.com

## Abstract 
This paper provides a high level review of different approaches for spatial interpolation using areal features. It groups these into those that use ancillary data to constrain or guide the interpolation (dasymetric, statistical, street weighted and point-based), and those do not but instead develop and refine allocation procedures (area to point, pycnophylactic and areal weighting). Each approach is illustrated by being applied to the same case study. The analysis is extended to examine the the opportunities arising from the many new forms of spatial data that are generated by everyday activities such as social media, check-ins, websites offering services, micro-blogging sites, social sensing, etc, as well as intentional VGI activities, both supported by ubiquitous web- and GPS-enabled technologies. Here data of residential properties from a commercial website was used as ancillary data. Overall, the interpolations using many of the new forms of data perform as well as traditional, formal data, highlighting the analytical opportunities as ancillary information for spatial interpolation and for supporting spatial analysis more generally. However, the case study also highlighted the need to consider the completeness and representativeness of such data. The R code used to generate the data, to develop the analysis and to create the tables and figures is provided.

## Code 
The code and data are used to illustrate a Geography Compass paper: Comber A, Zeng W. Spatial interpolation using areal features: A review of methods and opportunities using new forms of data with coded illustrations. *Geography Compass*. 2019; e12465, https://doi.org/10.1111/gec3.12465.

You can download the `SpatInt_Jan22.R` file (`.Rmd` file to come...perhaps!) and run this in R or RStudio. It loads the `.RData` files from this site and provides links to the `.R` files which describe how the data were created and assembled. You may need to install some of the packages but the code checks and does this. The script will load up the data (and different forms of ancillary data) and illustrate each of spatial interpolation approaches described in the text. Please contact me if you have queries! Lex (a.comber@leeds.ac.uk)

## Acknowledgements
This work was supported by the Natural Science Foundation of Shandong Province (ZR201702170310, the State Scholarship Fund of China Scholarship Council (201808370092) and the Natural Environment Research Council (NE/S009124/1). All of the analyses and mapping were undertaken in R 4.0.4 the open source statistical software.

## Package and Session Info
R version 4.0.4 (2021-02-15)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 10.16

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] GGally_2.1.1        ggplot2_3.3.5       raster_3.5-2        gclus_1.3.2        
 [5] cluster_2.1.2       repmis_0.5          OpenStreetMap_0.3.4 rgdal_1.5-23       
 [9] reshape2_1.4.4      osmdata_0.1.5       gstat_2.0-7         sf_0.9-8           
[13] GISTools_0.7-4      MASS_7.3-53.1       RColorBrewer_1.1-2  tmap_3.3-1         
[17] pycno_1.2           rgeos_0.5-5         maptools_1.1-2      sp_1.4-6           

loaded via a namespace (and not attached):
 [1] xts_0.12.1         lubridate_1.7.10   httr_1.4.2         R.cache_0.14.0     tools_4.0.4       
 [6] utf8_1.2.1         R6_2.5.0           KernSmooth_2.23-18 DBI_1.1.1          colorspace_2.0-0  
[11] withr_2.4.2        tidyselect_1.1.0   leaflet_2.0.4.1    curl_4.3           compiler_4.0.4    
[16] leafem_0.1.3       rvest_1.0.0        xml2_1.3.2         scales_1.1.1       classInt_0.4-3    
[21] proxy_0.4-26       stringr_1.4.0      digest_0.6.27      foreign_0.8-81     R.utils_2.10.1    
[26] base64enc_0.1-3    dichromat_2.0-0    pkgconfig_2.0.3    htmltools_0.5.1.1  htmlwidgets_1.5.3 
[31] rlang_0.4.10       FNN_1.1.3          generics_0.1.0     zoo_1.8-9          jsonlite_1.7.2    
[36] crosstalk_1.1.1    dplyr_1.0.5        R.oo_1.24.0        magrittr_2.0.1     Rcpp_1.0.7        
[41] munsell_0.5.0      fansi_0.4.2        abind_1.4-5        lifecycle_1.0.0    R.methodsS3_1.8.1 
[46] terra_1.4-11       stringi_1.5.3      leafsync_0.1.0     tmaptools_3.1-1    plyr_1.8.6        
[51] parallel_4.0.4     crayon_1.4.1       lattice_0.20-41    stars_0.5-2        pillar_1.6.0      
[56] spacetime_1.2-5    codetools_0.2-18   XML_3.99-0.6       glue_1.4.2         data.table_1.14.0 
[61] png_0.1-7          vctrs_0.3.7        gtable_0.3.0       purrr_0.3.4        reshape_0.8.8     
[66] assertthat_0.2.1   lwgeom_0.2-6       e1071_1.7-9        class_7.3-18       viridisLite_0.4.0 
[71] tibble_3.1.1       intervals_0.15.2   rJava_1.0-5        units_0.7-1        ellipsis_0.3.1    
>
