# CCIEA Whale Entanglement Dashboard


- [California Current Integrated Ecosystem Assessment (CCIEA) Whale Entanglement
  Dashboard](#california-current-integrated-ecosystem-assessment-cciea-indicator-dashboard)
  - [Code](#code)
  - [CCIEA Resources](#cciea-resources)

<hr>

## California Current Integrated Ecosystem Assessment (CCIEA) Whale Entanglement Dashboard and Maps

The CCIEA Whale Entanglement Dashboard is an on-line resource for viewing and
downloading the most recent CCIEA whale entanglement indicators and data.

The dashboard provides:

- Monthly updates of indicator data as available
- Interactive time series plots
- Metadata for each indicator
- Links for downloading indicator time series as csv files
- Links to ERDDAP™ for custom downloads
- Whale Entanglement Maps
- Crab Fishery Maps

### Code

The code in this repository is written in Quarto, R, and OJS. GitHub
actions perform daily updates of the indicator time series.

- **Github Action** - runs daily at 5 am
  - main.yaml: update_dashboard.R
    - libraries: tidyverse (jsonlite)
    - input: data/indicators.csv
      1.  list of indicators
      2.  ERDDAP™ dataset
      3.  which indicator group/plot they belong in
    - output: data/items_dashboard.json
- **Web Page** (Quarto)
  - \_quarto.yml
    - index.qmd
      1.  code: Markdown, R/OJS/html
      2.  input: items_dashboard.json
  - plotting library: [dygraph](https://dygraphs.com/)

### CCIEA Resources

- CCIEA Web pages:
  <https://www.integratedecosystemassessment.noaa.gov/regions/california-current>
- Ecosystem Status Reports:
  <https://www.integratedecosystemassessment.noaa.gov/regions/california-current/california-current-reports>
- Technical Documentation:
  <https://cciea-esr.github.io/ESR-Technical-Documentation-FY2026/>
- CCIEA Uploader: <https://cciea-esr.github.io/CCIEA-uploader/>
