<a href="https://covid19datahub.io"><img src="https://storage.covid19datahub.io/img/logo.svg" align="right" height="128"/></a>

# R Interface to COVID-19 Data Hub

![](https://www.r-pkg.org/badges/version/COVID19) ![](https://www.r-pkg.org/badges/last-release/COVID19) ![](https://cranlogs.r-pkg.org/badges/grand-total/COVID19) [![](https://img.shields.io/badge/doi-10.13140/RG.2.2.11649.81763-orange.svg)](https://doi.org/10.13140/RG.2.2.11649.81763)

## Quickstart

```R
# install the package
remotes::install_github("covid19datahub/R")

# load the package
require(COVID19)
```

## Download the data

See the full documentation by typing `?covid19`

```r
# Worldwide data by country
x <- covid19()

# Worldwide data by state
x <- covid19(level = 2)

# Specific country data by city
x <- covid19(c("Italy","US"), level = 3)
```

## Merge with World Bank Open Data

```R
# Merge with World Bank data
wb <- c("gdp" = "NY.GDP.MKTP.CD", "hosp_beds" = "SH.MED.BEDS.ZS")
x  <- covid19(wb = wb)
```

## List the data sources

```R
# Data sources
s <- attr(x, "src")
```

