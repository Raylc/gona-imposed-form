# gona-imposed-form
Imposed form in the Early Acheulean? Evidence from Gona, Afar, Ethiopia, Journal of Human Evolution.

## Getting Started
Please open the Rproj file instead of the Rcode directly to make sure relative paths work!

## File Structure
The repository is organised into four main directories: code, data, figure, and manuscript.

### code
* `Visualization` ... This is the R script generating the main figures (fig. 4-7) of this paper using data derived from SPSS.

### data
* `3D-cleaned-for-Cheng-080224`... This csv files include the comparative data of both caliper measurement and 3d measurement for method validation.
* `Data_dictionary`... This files explains the meaning of variables and how they were measured/calculated.
* `Gona_EA_for_Cheng_020824`... This csv file is the main data file recording all relevant attributes analyzed in the paper.

### figure
* `Fig.1.jpg` ... This is Fig.1 in the manuscript.
* `Fig.2.png` ... This is Fig.2 in the manuscript.
* `Fig.3.png` ... This is Fig.3 in the manuscript.
* `Fig.4.png` ... This is Fig.4 in the manuscript.
* `Fig.5.png` ... This is Fig.5 in the manuscript.
* `Fig.6.png` ... This is Fig.6 in the manuscript.
* `Fig.7.png` ... This is Fig.7 in the manuscript.
* `FAI_SDI.drawio` ... This is the raw file generating Fig.1.
* `Figures.pptx` ... This is the raw file generating Picture1-4.
* `Picture1.png` ... This is generated through Microsoft PowerPoint and integrated in Fig.7.
* `Picture2.png` ... This is generated through Microsoft PowerPoint and integrated in Fig.7.
* `Picture3.png` ... This is generated through Microsoft PowerPoint and integrated in Fig.7.
* `Picture4.png` ... This is generated through Microsoft PowerPoint and integrated in Fig.7.

### manuscript
* `jhe.csl` ... This is the JHE citation styleguide.
* `bibliography.bib` ... This is the reference file.
* `manuscript.pdf` ... This is the pdf file of the manuscript.
* `manuscript.Rmd` ... This is the RMarkdown file of the manuscript.
* `manuscript_word.docx` ... This is the word file of the manuscript.
* `manuscript_word.Rmd` ... This is the RMarkdown file of the word version of the manuscript.

## Dependencies
The code has been successfully executed on on CL's PC with the following R settings.

* CL's R setting
 ``` 
R version 4.4.0 (2024-04-24 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 22631)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: Asia/Shanghai
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1       psych_2.4.6.26         dplyr_1.1.4            fastmap_1.1.1         
 [5] MVN_5.9                bayestestR_0.14.0      promises_1.3.0         digest_0.6.35         
 [9] estimability_1.5.1     mime_0.12              lifecycle_1.0.4        magrittr_2.0.3        
[13] compiler_4.4.0         rlang_1.1.3            tools_4.4.0            utf8_1.2.4            
[17] yaml_2.3.8             knitr_1.46             mnormt_2.1.1           here_1.0.1            
[21] abind_1.4-5            miniUI_0.1.1.1         purrr_1.0.2            grid_4.4.0            
[25] datawizard_0.12.2      fansi_1.0.6            xtable_1.8-4           colorspace_2.1-0      
[29] ggplot2_3.5.1          emmeans_1.10.3         scales_1.3.0           MASS_7.3-60.2         
[33] insight_0.20.2         cli_3.6.2              mvtnorm_1.2-5          rmarkdown_2.26        
[37] generics_0.1.3         rstudioapi_0.16.0      tzdb_0.4.0             parameters_0.22.1     
[41] pbapply_1.7-2          energy_1.7-11          stringr_1.5.1          parallel_4.4.0        
[45] effectsize_0.8.9       BayesFactor_0.9.12-4.7 vctrs_0.6.5            boot_1.3-30           
[49] Matrix_1.7-0           carData_3.0-5          car_3.1-2              hms_1.1.3             
[53] nortest_1.0-4          tidyr_1.3.1            glue_1.7.0             cowplot_1.1.3         
[57] stringi_1.8.4          gtable_0.3.5           later_1.3.2            lmtest_0.9-40         
[61] gsl_2.1-8              munsell_0.5.1          tibble_3.2.1           pillar_1.9.0          
[65] htmltools_0.5.8.1      ggExtra_0.10.1         R6_2.5.1               rprojroot_2.0.4       
[69] evaluate_0.23          shiny_1.8.1.1          lattice_0.22-6         readr_2.1.5           
[73] png_0.1-8              moments_0.14.1         DFA.CANCOR_0.2.8       httpuv_1.6.15         
[77] MatrixModels_0.5-3     Rcpp_1.0.12            coda_0.19-4.1          nlme_3.1-164          
[81] xfun_0.43              zoo_1.8-12             pkgconfig_2.0.3    
 ``` 

## Help

Please contact raylc1996@outlook.com if you have any questions related to the code or data.
