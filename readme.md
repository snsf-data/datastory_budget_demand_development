# Rising demand for funding is becoming a challenge

*Demand for SNSF funding has been rising for years now. Why is this? Are more researchers applying for funding or are they submitting higher budgets? What consequences is this development having? An evaluation of internal data provides answers.*

[English](https://data.snf.ch/stories/rising-demand-for-funding-is-becoming-a-challenge-en.html)\
[German](https://data.snf.ch/stories/steigende-nachfrage-nach-foerdermitteln-wird-zur-herausforderung-de.html)\
[French](https://data.snf.ch/stories/la-demande-croissante-de-fonds-devient-un-defi-fr.html)

**Authors**: Gabriel Okasa and Michaela Strinzel

**Publication date**: 14.03.2024

## Code

The main code for the settings of the data story is contained the `main.R` script, while the code for the data story itself is provided in the respective Quarto documents:

-   `en.qmd`: English version of the data story
-   `de.qmd`: German version of the data story
-   `fr.qmd`: French version of the data story

The above scripts source the functions and definitions provided in the `utils/` subdirectory:

-   `util_funs.R`: script containing utility functions for descriptive statistics and plotting
-   `preprocess_data.R`: script containing data pre-processing from raw data for analysis-ready dataset
-   `define_plot_descriptions.R`: script containing multi-lingual descriptions for dynamic plotting

## Data description

The data used in this data story are available in the folder `data`. The data consist of two files:

-   `data/data_raw.csv`: containing information about 76915 proposals between 2011 and 2023. Each row represents a single application. The following variables are included:

    -   `CallDecisionYearReporting`: funding decision year of all applications of one call
    -   `FundingInstrumentGaLevel1`: top-level funding scheme hierarchy: Projects, Careers, Programmes, Infrastructure and Science Communication
    -   `FundingInstrumentGaReporting`: mid-level funding scheme hierarchy: e.g., Ambizione, Project funding, Spark, etc.
    -   `AmountRequested`: amount initially requested by the applicant (in CHF)
    -   `AmountGranted`: amount approved by the SNSF (in CHF)
    -   `IsApproved`: indicator if applications has been approved
    -   `DurationRequestedMonth`: Project duration requested by the applicant
    -   `TeamSize`: number of persons associated with the application: applicant, co-applicants and project partners

-   `data/data_federal.csv`: containing information about the yearly federal contributions to the SNSF between 2011 and 2023. Each row represents a yearly amount. The following variables are included:

    -   `Year`: year of the federal contribution
    -   `FederalContribution`: amount contributed by the federal government to the SNSF (in billion CHF)

In case of any questions, please contact: [datastories\@snf.ch](datastories@snf.ch).
