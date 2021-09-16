# Tocilizumab in COVID-19 – A Bayesian Reanalysis

This repository contains code and data to reproduce analysis in the research articles:

- ["Tocilizumab in COVID-19 – A Bayesian reanalysis of RECOVERY"](https://www.medrxiv.org/content/10.1101/2021.06.15.21258966v1) (preprint)
- **"Effect of Tocilizumab in Hospitalized COVID-19 Patients on Corticosteroids – A Bayesian Reanalysis"** (under review)

Authors: [Arthur M. Albuquerque](https://twitter.com/arthur_alb1), Lucas Tramujas, Lorenzo R. Sewanan, Donald R. Williams and [James M. Brophy](https://twitter.com/brophyj)

## Project organization

There are three main folders in this repository:

- `01_preregistration`, which corresponds to files regarding our preregistered [project in OSF](https://osf.io/qhvcp/)
- `02_preprint`, which corresponds to files regarding our preprint ["Tocilizumab in COVID-19 – A Bayesian reanalysis of RECOVERY"](https://www.medrxiv.org/content/10.1101/2021.06.15.21258966v1)
- `03_updated_analyses_who`, which corresponds to files regarding our newest research article **"Effect of Tocilizumab in Hospitalized COVID-19 Patients on Corticosteroids – A Bayesian Reanalysis"** (under review)

In each folder, one can find further instructions on how to reproduce the analysis (in the corresponding `README.md` file). 

## Getting started

All analyses were conducted in R (R Environment version 4.0.4). 

1.  To download all files and reproduce our analyses, clone this repository using Git's integration with RStudio. Here is a tutorial article in case you are not familiar with cloning repositories:

    *Vuorre M, Curley JP. Curating Research Assets: A Tutorial on the Git Version Control System. Advances in Methods and Practices in Psychological Science 2018;1:219–36.* [https://doi.org/10.1177%2F2515245918754826](https://doi.org/10.1177%2F2515245918754826)

       After cloning this repository, open the `tocilizumab_reanalysis.Rproj` file and you will be able to run all files.
2. We used the R package [{renv}](https://rstudio.github.io/renv/) to make this R project as reproducible as possible. In summary, {renv} guarantees that all required R packages for this project are downloaded to your computer in the necessary versions. Please check their ["Get Started" vignette](https://rstudio.github.io/renv/articles/renv.html) in case you would like to learn more about it.
