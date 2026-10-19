# More Urban Spatial Tools (MUST)

**More Urban Spatial Tools (MUST)** is an R package designed to support urban GIS analysis and the study of changes in urban environments over time.

The package aims to bring together geospatial remote sensing (via OpenEO) and land-use/land-cover (via OpenStreetMaps and Ohsome) datasets from  to facilitate the analysis of urban areas, land use, vegetation, and environmental conditions.

## Table of Contents

- [Overview](#overview)
- [Main Features](#main-features)
- [Requirements](#requirements)
- [Installation](#installation)
- [Getting Started](#getting-started)
- [Available Functions](#available-functions)
- [Example Workflow](#example-workflow)
- [Data Sources](#data-sources)
- [Limitations](#limitations)
- [Contributing](#contributing)
- [License](#license)

## Overview

MUST is intended to provide a straightforward workflow for retrieving and combining spatial data for cities over a selected period.

The package was developed to support temporal analyses of urban green spaces, but it may also support broader investigations of land-use patterns and environmental indicators.

## Main Features

The package is designed to support the following tasks:

- **Municipal boundaries:** Retrieve municipal boundary data for spatial analysis.
- **Urban boundaries:** Estimate urban boundaries.
- **Land use and land cover (LULC):** Retrieve land-use and land-cover information from historical OpenStreetMap data via Ohsome.
- **Vegetation indicators:** Retrieve Normalized Difference Vegetation Index (NDVI), Enhanced Vegetation Index (EVI) and Modified Soil Adjusted Vegetation Index (MSAVI) data.
- **Land surface temperature:** Retrieve Landsat 8 land surface temperature (LST) data.
- **Temporal analysis:** Support comparisons of urban spatial characteristics over multiple years.

The availability, temporal coverage, and processing options depend on the individual data source and function.

## Requirements

To use MUST, you need:

- R (version 3.5 or later, as specified in the package metadata).
- An R development environment, such as RStudio.
- An internet connection to retrieve remote geospatial data from OpenEO, OpenStreetMaps and Ohsome.
- Any additional access credentials for the OpenEO API (This involves making a free account).


## Installation

### Step 1: Install the installation helper

If you do not already have `remotes`, install it from CRAN:

```r
install.packages("remotes")
```

### Step 2: Install MUST from GitHub

Run the following command in R or RStudio:

```r
remotes::install_github(
  "JoeriMorpurgo/MoreUrbanSpatialTools"
)
```

### Step 3: Load the package

```r
library(MUST)
```

If installation fails, check your R version, internet connection, and the installation messages for missing dependencies or service configuration requirements.

## Getting Started

Start by loading the package and exploring its documentation.

```r
library(MUST)

help(package = "MUST")
```

To inspect the functions exported by the package, run:

```r
getNamespaceExports("MUST")
```

You can obtain help for an individual function using:

```r
?function_name
```

Replace `function_name` with the actual name of a function available in MUST.

Before requesting remote data, consult the relevant function documentation to identify its required arguments, supported years, spatial inputs, and authentication requirements.

## Available Functions

MUST includes R functions for retrieving and processing urban spatial data.

For a complete and accurate list of functions, see the R source code and the documentation in the repository:

- [`R/`](R/)
- [`man/`](man/)

The documentation for each function describes its purpose, arguments, expected inputs, and returned objects.

## Example Workflow

A typical analysis using MUST may follow these steps:

1. Select a city or municipality.
2. Define the years to investigate.
3. Retrieve the relevant municipal boundary or urban extent.
4. Obtain land-use and or remote sensing indicator data.
5. Check the spatial reference system, resolution, and temporal coverage.
6. Process and compare the resulting spatial datasets.
7. Visualise the results and document the analysis.


## Data Sources

MUST is designed to work with data from several sources, including:

- **OpenEO:** Satellite imagery and remote-sensing products.
- **OpenStreetMaps** Crowd-sources open data for land-use and land-cover.
- **Ohsome:** Historical OpenStreetMap data for analysing changes in mapped land use and urban features.

Data availability, licensing, attribution, and access requirements should be checked with the original providers.

## Limitations

Please consider the following when interpreting results:

- Remote data sources may have different spatial resolutions and temporal coverage.
- OpenStreetMap data reflects mapped features and may not represent all real-world land use.
- Satellite-derived indicators can be affected by clouds, image availability, and processing choices.
- Remote data retrieval may depend on external APIs and their current access policies.
- Results should be validated before being used for policy decisions or other applications requiring high accuracy.

## Contributing

Suggestions, bug reports, and improvements are welcome.

To contribute:

1. Open an issue in the GitHub repository to describe a problem or proposed improvement.
2. Fork the repository.
3. Create a branch for your changes.
4. Submit a pull request describing the changes.

Please include reproducible examples when reporting bugs.

## License

MUST is distributed under the GNU General Public License version 3 (GPL-3). Review the repository's licensing files for the applicable terms.

## Repository

Source code, documentation, and updates are available at:

https://github.com/JoeriMorpurgo/MoreUrbanSpatialTools
