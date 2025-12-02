# UK marine ornamental imports

Author(s): [Sarah Alewijnse](https://github.com/sarah-alewijnse)

This repository contains the code used to clean and analyse data on the import of marine ornamental species into the United Kingdom.

Please note: some sensitive information in the code has been redacted for this public version.

## Session Information

```
R version 4.5.1 (2025-06-13 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
[3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.utf8    

time zone: Europe/London
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] robis_2.11.3      rredlist_1.1.1    taxize_0.10.0     rgbif_3.8.3       beepr_2.0         patchwork_1.3.2  
 [7] ggplot2_4.0.0     worrms_0.4.3      rfishbase_5.0.1   stringr_1.6.0     tidyr_1.3.1       dplyr_1.1.4      
[13] data.table_1.17.8 readr_2.1.5       here_1.0.2       

loaded via a namespace (and not attached):
 [1] DBI_1.2.3          tmaptools_3.3      leafpop_0.1.0      rlang_1.1.6        magrittr_2.0.4     otel_0.2.0        
 [7] e1071_1.7-16       compiler_4.5.1     png_0.1-8          systemfonts_1.3.1  vctrs_0.6.5        httpcode_0.3.0    
[13] pkgconfig_2.0.3    crayon_1.5.3       fastmap_1.2.0      mapedit_0.7.0      lwgeom_0.2-14      leafem_0.2.5      
[19] promises_1.5.0     tzdb_0.5.0         purrr_1.1.0        satellite_1.0.6    cachem_1.1.0       jsonlite_2.0.0    
[25] later_1.4.4        mapview_2.11.4     uuid_1.2-1         terra_1.8-80       parallel_4.5.1     R6_2.6.1          
[31] bslib_0.9.0        stringi_1.8.7      RColorBrewer_1.1-3 stars_0.6-8        jquerylib_0.1.4    Rcpp_1.1.0        
[37] zoo_1.8-14         audio_0.1-11       base64enc_0.1-3    httpuv_1.6.16      tidyselect_1.2.1   abind_1.4-8       
[43] rstudioapi_0.17.1  codetools_0.2-20   miniUI_0.1.2       curl_7.0.0         lattice_0.22-7     tibble_3.3.0      
[49] plyr_1.8.9         shiny_1.11.1       withr_3.0.2        S7_0.2.0           sf_1.0-21          units_1.0-0       
[55] proxy_0.4-27       xml2_1.4.1         pillar_1.11.1      whisker_0.4.1      KernSmooth_2.23-26 stats4_4.5.1      
[61] generics_0.1.4     rprojroot_2.1.1    sp_2.2-0           hms_1.1.4          scales_1.4.0       xtable_1.8-4      
[67] class_7.3-23       glue_1.8.0         lazyeval_0.2.2     tools_4.5.1        XML_3.99-0.19      grid_4.5.1        
[73] ape_5.8-1          crosstalk_1.2.2    nlme_3.1-168       raster_3.6-32      cli_3.6.5          brew_1.0-10       
[79] textshaping_1.0.4  svglite_2.2.2      gtable_0.3.6       oai_0.4.0          sass_0.4.10        digest_0.6.37     
[85] classInt_0.4-11    crul_1.6.0         htmlwidgets_1.6.4  farver_2.1.2       htmltools_0.5.8.1  lifecycle_1.0.4   
[91] leaflet_2.2.3      httr_1.4.7         shinyWidgets_0.9.0 mime_0.13         
```

