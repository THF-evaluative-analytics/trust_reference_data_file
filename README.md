# trust_reference_data_file
R code that creates a data file (trust_ref_file_csv) containing characteristics of NHS England trusts using publicly-available reference data.


#### Project Status: On-going
  [![R build status](https://github.com/THF-evaluative-analytics/THFstyle/workflows/R-CMD-check/badge.svg)](https://github.com/THF-evaluative-analytics/trust_reference_data_file/actions)
  
  
## About this project

The Improvement Analytics Unit (IAU) uses quite a lot of publicly-available reference data about the NHS as part of the conterfactual analyses that carries out for different health-care evaluations. 

This repository describes the way that the IAU created a data file containing characteristics of NHS England acute and mental health trusts using publicly-available reference data (described in 'Data sources' section below). This trust-level file contains yearly series of data about catchment population sizes, demographics of patients registered and hospital activity. 

The IAU would use this trust reference data file to get information for risk adjustment in counterfactual analyses using synthetic control methods at trust level. One example is the IAU 'DECISION' project that examines how the provision of a psychiatric decision unit (PDU) impacts hospital utilisation at some mental health trusts and their associated local acute trusts in the country 2 years after the opening of the PDU. However, we believe that this file might be useful to other analytics units that would need a dataset containing characteristics of NHS England trusts.


## Requirements

These scripts were written in RStudio Version 1.1.463. 
The following R packages (available on CRAN) are needed to run the script:

tidyverse,
data.table,
lubridate,
here,
janitor


## Summary of R codes

The 'R' code reads all available datasets for trust characteristics from various sources and cleans them. It then creates a skeleton trust reference file, adds the catchment sizes, calculates percentages for age and gender groupings and reformats the file. Lastly, it fills missing values for cases where there is no available data.


## Data sources

1. PEER data: data tab from Appendix A from NHS Peer Finder Tool
Ref: https://www.england.nhs.uk/insights-platform/model-hospital/
https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/innovative-uses-of-data/multi-dataset-analysis/nhs-trust-peer-finder-tool

2. CATCHMENT data: from Public Health England
Ref: https://app.powerbi.com/view?r=eyJrIjoiODZmNGQ0YzItZDAwZi00MzFiLWE4NzAtMzVmNTUwMThmMTVlIiwidCI6ImVlNGUxNDk5LTRhMzUtNGIyZS1hZDQ3LTVmM2NmOWRlODY2NiIsImMiOjh9

3. NHS Code Organisation: from NHS England
Ref: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiLrvvQlervAhU-gP0HHUf2DfcQFjADegQIBhAD&url=https%3A%2F%2Fwww.england.nhs.uk%2Fwp-content%2Fuploads%2F2014%2F11%2Fnhs-non-nhs-ods-codes.xlsx&usg=AOvVaw1hduuzFTZeTUFD1QYmk7u2

4. NHS TRUST NAMES (etr): from NHS England and NHS Digital
(2021) Ref: https://digital.nhs.uk/services/organisation-data-service/data-downloads/other-nhs-organisations
(2016) Ref: https://data.england.nhs.uk/dataset/ods-nhs-trusts-and-sites

5. Names of Mental Health Trusts: from Wikipedia
Ref: https://en.wikipedia.org/wiki/Mental_health_trust#List_of_MHTs 


## Authors

This analysis was conducted by:

Geraldine Clarke [Twitter](https://twitter.com/GeraldineCTHF)

Paris Pariza [Twitter](https://twitter.com/ParizaParis) - [Github](https://github.com/Ppariz)


## License

This project is licensed under the [MIT License](LICENSE.md).


