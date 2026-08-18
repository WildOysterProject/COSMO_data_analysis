# COSMO Data Analysis
Processing and analysis of volunteer data collected for Wild Oyster Project's Community Oyster Science Monitoring Opportunity program.

This repository contains R code used by the **Wild Oyster Project** 

## Repository Structure

```text
cosmo-data-analysis/
├── COSMO.Rproj
├── COSMO.R
├── README.md
├── .gitignore
├── .gitattributes
└── Input/
```

The current workflow includes:

* Importing COSMO monitoring data from Excel
* Data cleaning and type conversion
* Calculating oyster and mussel density per square meter
* Processing percent-cover observations
* Normalizing percent-cover measurements
* Aggregating taxa into broader taxonomic groups

## `Input/`

Contains source data used by the analysis. Note: source data files are not tracked in this repository.

## Monitoring Sites

The current analysis includes data from the following sites:

| Code | Site              |
| ---- | ----------------- |
| SP   | Strawberry Point  |
| HH   | Heron's Head Park |
| PM   | Point Molate      |
| PE   | Point Emery       |
| YBI   | Yerba Buena Island       |

## Packages

```
library(tidyverse)
library(ggplot2)
library(readxl)
```
## Data Availability

The source COSMO monitoring dataset is maintained separately from this GitHub repository. The `Input/` directory is excluded from version control to prevent source datasets from being inadvertently committed to GitHub.

Information about public access to COSMO data and associated archival datasets will be added as data products are prepared for release.

## About Wild Oyster Project

Wild Oyster Project works to restore native oysters and oyster habitat while engaging communities in monitoring, restoration, and stewardship.

For more information, visit the Wild Oyster Project website.

## Contributors

This repository is maintained by Wild Oyster Project staff and collaborators.

## License

License information will be added prior to public release of the repository.
