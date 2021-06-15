# Tocilizumab in COVID-19 – A Bayesian reanalysis of RECOVERY

This repository contains code and data to reproduce analysis in the research article "Tocilizumab in COVID-19 – A Bayesian reanalysis of RECOVERY"

Authors: [Arthur M. Albuquerque](https://twitter.com/arthur_alb1), Lucas Tramujas, Lorenzo R. Sewanan and [James M. Brophy](https://twitter.com/brophyj)

OSF project: [https://osf.io/qhvcp/](https://osf.io/qhvcp/)

## Project organization

```
Folders

│── final_analyses          <- All project data and code to reproduce analyses in the article
│   ├── analyses            <- RMarkdown files for all analysis
│   ├── data                <- Raw data from the the extraction process
|   ├── output              <- Modified data and final figures
│       ├── data            <- Modified data files
│           └── analyses       <- .RData files required to run .Rmd analysis files
│       ├── plots           <- Intermediate visualizations and data for final figures
│           ├── appendix       <- Figures and tables for the Supplementary Material
│           ├── manuscript     <- Figures and tables for the article
│   └── script              
│       └── functions  
│           └── analyses     <- Custom functions required to run .Rmd analysis files
│
├── preregistration          <- .Rmd and .pdf files used to create files uploaded in this project's OSF repository
|   |
│   └── data-extraction-templates <- Original tables used during the data extraction process
│
└── renv                     <- Created by the R package {renv}


```

## Getting started
  All analyses were conducted in R (R Environment version 4.0.4). 

1.  To download all files and reproduce our analyses, clone this repository using Git's integration with RStudio. Here is a tutorial article in case you are not familiar with cloning repositories:

    Vuorre M, Curley JP. Curating Research Assets: A Tutorial on the Git Version Control System. Advances in Methods and Practices in Psychological Science 2018;1:219–36. doi:[10.1177/2515245918754826](10.1177/2515245918754826)

       After cloning this repository, open the `tocilizumab_reanalysis.Rproj` file and you will be able to run all files.
2. We used the R package [{renv}](https://rstudio.github.io/renv/) to make this R project as reproducible as possible. In summary, {renv} guarantees that all required R packages for this project are downloaded to your computer. Please check their ["Get Started" vignette](https://rstudio.github.io/renv/articles/renv.html) in case you would like to learn more about it.

## Notes
 There were twelve .RData files that were too large (>50MB) to upload to GitHub. These files are essential to run these .Rmd scripts:

 - `/final_analyses/analyses/03_Figures_Draft.Rmd` 
 - `/final_analyses/analyses/04_Sensitivity_Analyses_Different_Baseline_Risks.Rmd`
 - `/final_analyses/analyses/05_Sensitivity_Analyses_Different_Priors.Rmd`
 - `/final_analyses/analyses/07_Hospital_Discharge_Varying_Baseline_Risk.Rmd`

We have uploaded these .RData files to [https://osf.io/veazj/](https://osf.io/veazj/). We then wrote code lines in these four .Rmd scripts to be able to load these .RData files directly from OSF without the need to save them into your computer. We used functions such as ``url()``,``gzcon()``, and ``readRDS()``, inspired by [this script](https://osf.io/73thx/) we found online.

In general, the framework explained above works perfectly.
Yet, we have experienced time-out issues while loading large files from OSF.
Thus, if one prefers to load the files from your computer, we suggest to download
these twelve .Rdata files from [https://osf.io/veazj/](https://osf.io/veazj/) and
save them in the `/final_analyses/output/data/analyses` folder of this repository in your computer.
Lastly, one would have to alter the scripts to load the files from the computer, and not from OSF.

In summary, we suggest to stay with the framkework which loads the .RData files from OSF.

