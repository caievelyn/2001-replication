
-------------------------------
			READ ME
-------------------------------
This repository contains the data and scripts necessary for reproducing the results in the article "How Political Parties Shape Public Opinion in the Real World" by Martin Bisgaard and Rune Slothuus, published in the American Journal of Political Science. 

Note that the original survey data, denoted "raw_paneldata.dta" in the script "build_data.R" is not included in the repository. The full, original dataset and documentation from this panel survey is available from "Rigsarkivet", the Danish National Archives (ttps://www.sa.dk/en/). The data is archived with the title "DDA-22007 Party political conflicts and citizens attitudes panel data 2010-2011" and can be accessed through this URL: http://dda.dk/catalogue/22007?lang=en

The repository contains the following files:

DOCUMENTATION
- codebook_mediacontent.pdf	(description of variables contained in "partypositions.sav")
- codebook_paneldata.pdf	(description of variables contained in "paneldata_long.dta" & "paneldata_wide.dta")
- codebook_followupsurvey.pdf	(description of variables contained in "followup_study.sav")
- codebook_ess.pdf			(description of variables contained in "ESS1-8e01.dta")
- codebook_reliability.pdf	(description of variables contained in "reliability_all.sav")
- questionnaire_wave1.pdf (full questionnaire for first panel wave)	
- questionnaire_wave1_originalDanish.pdf (full questionnaire for first panel wave in original language)	
- questionnaire_wave2.pdf (full questionnaire for second panel wave)
- questionnaire_wave2_originalDanish.pdf (full questionnaire for second panel wave in original language)	
- questionnaire_wave3.pdf (full questionnaire for third panel wave)
- questionnaire_wave3_originalDanish.pdf (full questionnaire for third panel wave in original language)	
- questionnaire_wave4.pdf (full questionnaire for fourth panel)
- questionnaire_wave4_originalDanish.pdf (full questionnaire for fourth panel wavein original language)	
- questionnaire_wave5.pdf (full questionnaire for fifth panel wave)
- questionnaire_wave5_originalDanish.pdf (full questionnaire for fifth panel wave in original language)	

DATA FILES	
- partypositions.sav 		(coded party positions from newspaper articles)
- reliability_all.sav		(subset of coded party positions coded by two independent coders)
- paneldata_long.dta 		(five wave panel data in LONG format)
- paneldata_wide.dta 		(five wave panel data in WIDE format)
- followup_study.sav 		(followup representative survey)
- ESS1-8e01.dta				(selected variables from the European Social Survey (wave 1-8), https://www.europeansocialsurvey.org/)
								European Social Survey Cumulative File, ESS 1-8 (2018). Data file edition 1.0. NSD - Norwegian Centre for Research Data, Norway - Data Archive and distributor of ESS data for ESS ERIC. doi:10.21338/NSD-ESS-CUMULATIVE.

R SCRIPTS
- build_data.R			(generates the data files for the panel analysis from the original data)
- main_analysis.R	 	(reproduces all numerical results and figures reported in the main article)
- SI_analysis.R 		(reproduces all numerical results and figures reported in the supplemental information appendix)


-----------------------------------
Computing environment information:
------------------------------------
R version 3.6.1 (2019-07-05)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17134)

attached packages:
 [1] sandwich_2.5-1   MatchIt_3.0.2    pBrackets_1.0    reshape2_1.4.3  
 [5] psych_1.9.12.31  xtable_1.8-4     stargazer_5.2.2  lmtest_0.9-37   
 [9] zoo_1.8-6        lme4_1.1-23      Matrix_1.2-17    plm_2.2-3       
[13] memisc_0.99.17.2 MASS_7.3-51.4    lattice_0.20-38  plyr_1.8.4      
[17] car_3.0-7        carData_3.0-2    foreign_0.8-71   rio_0.5.16      

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.2        bdsmatrix_1.3-3   zeallot_0.1.0     digest_0.6.21    
 [5] R6_2.4.0          cellranger_1.1.0  repr_1.0.1        backports_1.1.5  
 [9] pillar_1.4.2      Rdpack_0.11-0     miscTools_0.6-22  rlang_0.4.0      
[13] curl_4.2          readxl_1.3.1      minqa_1.2.4       data.table_1.12.4
[17] nloptr_1.2.1      splines_3.6.1     statmod_1.4.32    readr_1.3.1      
[21] stringr_1.4.0     compiler_3.6.1    pkgconfig_2.0.3   base64enc_0.1-3  
[25] mnormt_1.5-5      maxLik_1.3-6      htmltools_0.4.0   tibble_2.1.3     
[29] crayon_1.3.4      grid_3.6.1        nlme_3.1-140      jsonlite_1.6     
[33] magrittr_1.5      bibtex_0.4.2      zip_2.0.4         stringi_1.4.3    
[37] vctrs_0.2.0       boot_1.3-22       openxlsx_4.1.0.1  Formula_1.2-3    
[41] tools_3.6.1       forcats_0.4.0     hms_0.5.1         abind_1.4-5      
[45] parallel_3.6.1    gbRd_0.4-11       haven_2.1.1 