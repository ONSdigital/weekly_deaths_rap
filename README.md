# Weekly Deaths RAP

*README last updated - 08/12/2025*

*Authors:*

- Data Science team in National Statistician's Office (NSO) Analysis Unit
    - Code development
- Population Life Events (HEALTH.DATA@ons.gov.uk)
    - Topic and pipeline owner

## Contents
- [Project statement](#project-statement)
- [Project description](#project-description)
- [Installation instructions](#installation-instructions)
- [Contribution guidance](#contribution_guidance)
- [Permissions and locations](#permissions-and-locations)
- [Data lifecycle](#data-lifecycle)

## Project statement

Weekly publication of death registration and occurrence data for the latest available week.  

- [Latest dataset](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales)
-  [Evergreen page/Dashboard](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/articles/deathsregisteredweeklyinenglandandwales/2025-03-26/)

## Project description

The `weekly.deaths` package contains functions to allow weekly figures of death registrations and occurrences to be produced from provisional death registration data. The following files are created:

- a publishable excel file
- a QA html document
- a run log
- a dataset for the dashboard/evergreen page

The statistics created are for several different breakdowns, including:

- Week number
- Area of usual residence (country and region)
- Sex
- Age group
- IMD quantile group
- Place of occurrence
- Cause of death
- Certification type
- Registration delay

Deaths figures are based on registrations, occurrences and expected deaths (from excess deaths model). UK, Scotland and Northern Ireland figures are also incorporated and presented.

## Installation instructions

You need to have `devtools` package installed. Clone this GitLab repository and run `devtools::install("weekly.deaths")` to install the package in R. Please note that your working directory needs to be set as the project directory, e.g.: `setwd("location_on_your_PC/weekly-deaths-rap")`.



### Installing other packages

The [rapid.spreadsheets](https://github.com/RAPID-ONS/rapid.spreadsheets) package is required. You can install the package directly from GitHub using ```devtools::install_github("RAPID-ONS/rapid.spreadsheets", dependencies = TRUE, build_vignettes = TRUE)``` or download the package from the [GitHub repository](https://github.com/RAPID-ONS/rapid.spreadsheets) using **<> Code** then **Download ZIP**, before installing using:

```devtools::install("rapid.spreadsheets", dependencies = TRUE, build_vignettes = TRUE)```

### Running the pipeline

To run the analysis, your working directory should be set to the weekly-deaths-rap folder, where the cloned repository is stored. Firstly, update the run variables in the config.R file. This includes:

- **week** - Update to week of interest (usually most recent week of data available)
- **publication_date** - this will be the following Wednesday from when you start running the data
- **scope** - this will be "UK" if all data is available. Use "EW" if Scotland and NI tables  are not ready so previous week's data is used.

Other config variables will only be updated less frequently, for example when moving to a new year or when annual data is finalised. These include:

- **year** - the registration year of interest
- **chart_years** - the years of data that need to be imported to produce dashboard chart with two years worth of data.
- **provisional_years** - which years of data should be using the provisional data (uses the weekly death data extract)
- **finalised_years** - which years of data should be finalised (uses annual deaths data extract)

After you have updated the config file you can run the analysis by typing `source("main.R")` OR `source("main.R", echo = TRUE)` in R Studio console. Both options will produce outputs in the same way, but the second will also print out the code used into the console. You can also open the script `main.R` in R Studio and run it using `Ctrl + Shift + Enter`.

Once the script has run successfully and output a publication ready dataset, run log, and quality report will be saved into the **outputs** folder, in your locally cloned repository.

### Running the pipeline with mock data

The inputs folder contains **weekly_deaths_mock_data.xlsx**. This data is purely demonstrative and does not reflect actual values. It can be used to demo running the pipeline for week 2 of 2025. In the main.R script, the data import section should be replaced with the following code which will create the data in the format needed to run the pipeline.

```
mock_data_filename <- "inputs/weekly_deaths_mock_data.xlsx"
mock_data_sheets <- readxl::excel_sheets(mock_data_filename)
mock_data_list <- lapply(mock_data_sheets, function(x)
  readxl::read_excel(mock_data_filename, sheet = x)
)
names(mock_data_list) <- mock_data_sheets

# Put dataframes in global environment
list2env(mock_data_list, envir = .GlobalEnv)
imd <- list(england = imd_england, wales = imd_wales)
```

#### Updating spreadsheet text
Text for the cover sheet, contents page and notes is stored in .txt files inside the spreadsheet_text folder. To make changes to the text that appears in the .xlsx dataset file, the text should be updated in these files. Information such as the table the notes apply to is in the config. The final note is about finalised data and this note is only applicable and displayed if finalised data is used. If a new note is added it should be added before the note on finalised data, and links and row numbers updated in the config.

### Provisional and Final data
Finalised data should be used when it is available. Most of the tables in the dataset will only be for the latest year, and so will contain provisional data. Table 2 (Occurrences) and the data for the evergreen page/dashboard also contain data from previous years. Whether data for a year is finalised or provisional is set in the config using **provisional_years** and **finalised_years**, both of which should contain at least one year. Check with Population Life Events before changing to finalised data.


### Additional Quality Assurance
Internally we inspect the output data from the pipeline using R Shiny dashboard as a part of quality assurance process before it is published. To share something similar externally we created a [prototype data explorer tool (experimental)](https://onsdigital.github.io/weekly_deaths_data_explorer/) that allows users to explore the latest weekly deaths data. It includes some of the charts we use internally, and further development will depend on user needs.





## Contribution guidance
To contribute to this pipeline please get in touch with the project owner(s).

- Never push directly to the main branch. Develop new code in separate branch and create a merge request
- Another analyst should peer review you code before merging with main
- Ensure unit tests are still working before merging new code. To check, run `devtools::test("weekly.deaths")`
- When new functions are added to the package, they should have complete documentation and unit tests written
- Code should follow the tidyverse style guide, and utilize the functions contained within the weekly.deaths package
- Use the [lintr package](https://lintr.r-lib.org/) to aid with following the tidyverse style guide

- Use the standardised commit messages when pushing your local changes:
  - **feat** - A new feature
  - **fix** - A bug fix
  - **docs** - Changes only to documentation
  - **style** - Changes to formatting (missing semicolons, etc.)
  - **refactor** - A code change that neither fixes a bug nor adds a feature
  - **update** - Changes to update code but not change functionality
  - **test** - Adding missing, or correcting existing, tests
  - **chore** - Change to build process or auxiliary tools, or maintenance
  

## Permissions and locations
Information on database permissions and file locations is available on the internal pipeline README.

  
## Data lifecycle
- Mortality data is received in ONS from GRO (General register Office)
    - The Life Events Processing (LEP) team deal with collating this data, and it is stored within LEDR (Life Events Data Repository).
    - The Analytical Health Data Service (AHDS) then extract whatever data is needed for analysis from LEDR (Life Events Data Repository).

For more information on mortality data, please see the [User guide to mortality statistics](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/methodologies/userguidetomortalitystatisticsjuly2017).
