<a href="https://covid19datahub.io"><img src="https://storage.covid19datahub.io/logo.svg" align="right" height="128"/></a>

# R Interface to COVID-19 Data Hub

![](https://www.r-pkg.org/badges/version/COVID19) ![](https://cranlogs.r-pkg.org/badges/last-month/COVID19) [![DOI](https://joss.theoj.org/papers/10.21105/joss.02376/status.svg)](https://doi.org/10.21105/joss.02376)

Provides a daily summary of COVID-19 cases, deaths, recovered, tests, vaccinations, and hospitalizations for 230+ countries, 760+ regions, and 12000+ administrative divisions of lower level.  Includes policy measures, mobility data, and geospatial identifiers. Data source: COVID-19 Data Hub https://covid19datahub.io

## Quickstart

```R
# install the package
install.packages("COVID19")

# load the package
library("COVID19")
```

## Usage

The only function in the package is `covid19()`. 

By default, the function downloads worldwide data by country:

```R
x <- covid19()
```

### Level

The argument `level` specifies the granularity of the data:

- 1: country-level data
- 2: state-level data
- 3: lower-level data

Download worldwide data by state:

```R
x <- covid19(level = 2)
```

### Country

The argument `country` filters the data by country. This is a vector of country names or [ISO codes](https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes) (ISO 3166-1 Alpha-2 code, Alpha-3 code, or numeric code). 

Download data for Italy and Unites States at county/province level:

```R
x <- covid19(country = c("Italy", "US"), level = 3)
```

### Time range

The arguments `start` and `end` specify the period of interest. The data are subsetted to match this time range.

Download national-level data for United States from 01 October 2021 to 01 November 2021:

```R
x <- covid19("US", start = "2021-10-01", end = "2021-11-01")
```

### Vintage

The parameter `vintage` allows to retrieve the snapshot of the dataset that was available on the given date. This typically differs from subsetting the latest data, as most governments are updating the data retroactively. Available since 14 April, 2020.

Retrieve the data that were available on 15 May, 2020:

```R
x <- covid19(vintage = "2020-05-15")
```

### Download folder

The argument `dir` specifies the folder where the data files are to be downloaded. By default this is a temporary folder. 

Download the files in the folder `data`:

```R
dir.create("data")
x <- covid19(dir = "data")
```

### World Bank Open Data

Country-level covariates by [World Bank Open Data](https://data.worldbank.org/) can be added via the argument `wb`. This is a character vector of indicator codes to download. The codes can be found by inspecting the corresponding URL. For example, the code of the indicator "Hospital beds (per 1,000 people)" available at https://data.worldbank.org/indicator/SH.MED.BEDS.ZS is `SH.MED.BEDS.ZS`. The indicators are typically available at a yearly frequency. This function returns the latest data available between the `start` and the `end` date. Example using GDP and number of hospital beds:

```R
x <- covid19(wb = c("gdp" = "NY.GDP.MKTP.CD", "hosp_beds" = "SH.MED.BEDS.ZS"))
```

### Google Mobility Reports

Mobility data by [Google Mobility Reports](https://www.google.com/covid19/mobility/) can be added via the argument `gmr`. This is the link to the Google "CSV by geographic area" ZIP folder. At the time of writing, the link is https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip. As the link has been stable since the beginning of the pandemic, the function accepts `gmr=TRUE` to automatically use this link.

```R
x <- covid19(gmr = TRUE)
```

### Apple Mobility Reports

As of April 14, 2022, Apple is no longer providing COVID-19 [mobility trends reports](https://covid19.apple.com/mobility). If you have downloaded the data file previously, you can still use it by setting `amr="path/to/file.csv"`.

```R
x <- covid19(amr = "path/to/file.csv")
```

## Documentation

See the full documentation [online](https://cran.r-project.org/package=COVID19/COVID19.pdf), or in R by typing `?covid19`

## Cite as

*Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376, doi: 10.21105/joss.02376.*

A BibTeX entry for LaTeX users is

```latex
@Article{,
    title = {COVID-19 Data Hub},
    year = {2020},
    doi = {10.21105/joss.02376},
    author = {Emanuele Guidotti and David Ardia},
    journal = {Journal of Open Source Software},
    volume = {5},
    number = {51},
    pages = {2376}
}
```
