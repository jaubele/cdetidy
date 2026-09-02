
# cdetidy

A tidyverse-style toolkit for working with California Department of
Education (CDE) datasets.

The `cdetidy` R package contains functions developed by the Orange
County Department of Education (OCDE) for transforming, validating,
labeling, and exporting public CDE datasets. It supports internal OCDE
workflows related to data warehousing, reporting, and dashboard
preparation, but may also be useful for others working with similar data
structures.

------------------------------------------------------------------------

## Key Features

- Clean and validate CDS codes for counties, districts, and schools
- Apply standardized group labels across CDE data domains
- Generate and log SQL schema definitions for warehouse tables
- Create bulk-insertable flat files with logging and metadata
- Detect schema drift across years for rebuild planning
- Check primary keys, suppression rules, and table structures
- Support reusable, year-spanning CDE ETL workflows

------------------------------------------------------------------------

## Installation

Install the package directly from GitHub:

``` r
install.packages("pak")  # Run once if pak is not installed
pak::pak("jaubele/cdetidy")

library(cdetidy)
```
