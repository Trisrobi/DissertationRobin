# EU Enlargement and Trade-Network Structure

This repository contains the code and derived data for a dissertation examining whether the **2004 enlargement of the European Union** was followed by changes in the concentration and diversification of member states' import and export networks.

The project combines international input-output data, network-based concentration measures, and panel econometrics. It constructs annual country-level **Gini coefficients** and **Shannon entropy** measures from industry trade flows, then evaluates changes around EU accession using fixed-effects and difference-in-differences models.

## Research design

- **Period:** 2000–2014
- **Trade data:** [World Input-Output Database (WIOD), 2016 release](https://www.rug.nl/ggdc/valuechain/wiod/wiod-2016-release?lang=en)
- **Treatment group:** Czechia, Estonia, Hungary, Latvia, Lithuania, Poland, Slovakia, and Slovenia
- **Comparison group:** Other EU countries represented in the analytical sample
- **Outcomes:** Import and export network concentration/diversification
- **Empirical methods:** Panel fixed effects, difference-in-differences, and parallel-trends checks

The treatment indicator identifies the post-2004 period for the eight Central and Eastern European countries in the sample that joined the EU in 2004.

## Methodology

The analysis proceeds in three stages:

1. **Network construction**  
   Annual WIOD tables are converted into directed, weighted industry trade networks for each country.

2. **Network measurement**  
   Import and export structures are summarized separately using:
   - the **Gini coefficient**, where a higher value indicates greater concentration;
   - **Shannon entropy**, where a higher value indicates greater diversification.

3. **Econometric analysis**  
   The network measures are merged with World Bank indicators and analyzed using country-panel models around the 2004 enlargement. The controls cover:
   - working-age population;
   - labor force with basic education;
   - research and development expenditure as a percentage of GDP;
   - foreign direct investment net inflows as a percentage of GDP.

## Repository structure

| Path | Contents |
|---|---|
| [`Rcode/`](Rcode) | R scripts for constructing trade networks, calculating Gini and entropy measures, and merging World Bank controls |
| [`Stata code/`](Stata%20code) | Stata scripts for summary statistics, panel regressions, difference-in-differences estimation, and parallel-trends checks |
| [`Final dataset used/`](Final%20dataset%20used) | Derived country-year datasets used in the econometric analysis |

### R scripts

| Script | Purpose |
|---|---|
| [`Entropy imports.R`](Rcode/Entropy%20imports.R) | Calculates annual entropy measures for import networks |
| [`entropyexports.R`](Rcode/entropyexports.R) | Calculates annual entropy measures for export networks |
| [`gini_results_imports.R`](Rcode/gini_results_imports.R) | Calculates annual Gini coefficients for import networks |
| [`gini_resultsexports.R`](Rcode/gini_resultsexports.R) | Calculates annual Gini coefficients for export networks |
| [`Adding world bank data for entropy.R`](Rcode/Adding%20world%20bank%20data%20for%20entropy.R) | Adds the World Bank covariates to the entropy panels |
| [`Addingworldbankdataforginicoefficient.R`](Rcode/Addingworldbankdataforginicoefficient.R) | Adds the World Bank covariates to the Gini panels |

## Data

The committed analytical datasets cover 27 countries over 15 years, giving 405 country-year observations for each EU-level import/export specification.

The World Bank covariates in [`Additional Data.csv`](Final%20dataset%20used/Additional%20Data.csv) are:

| Indicator | World Bank code |
|---|---|
| Labor force with basic education (% of working-age population) | `SL.TLF.BASC.ZS` |
| Population ages 15–64, total | `SP.POP.1564.TO` |
| Research and development expenditure (% of GDP) | `GB.XPD.RSDV.GD.ZS` |
| Foreign direct investment, net inflows (% of GDP) | `BX.KLT.DINV.WD.GD.ZS` |

## Reproducing the analysis

### Requirements

The data-construction scripts use R with the following packages:

`igraph`, `tidyverse`, `glue`, `reshape2`, `plm`, `haven`, and `fredr`.

The econometric scripts require Stata. Some table-export commands use the community-contributed `estout` package.

### Workflow

1. Download the annual WIOD R files for 2000–2014 from the 2016 release.
2. Update the WIOD input paths in the four network-construction scripts under [`Rcode/`](Rcode).
3. Run the import and export scripts to generate the Gini and entropy country-year measures.
4. Run the two World Bank merge scripts for the desired import/export specification.
5. Update the input path at the top of the relevant Stata `.do` file and execute the panel analysis.

> **Reproducibility note:** the repository includes the final derived CSV files, but not the raw annual WIOD `.RData` files. The scripts also retain the original absolute local paths, so these paths must be changed before rerunning the full pipeline.

## Tools

- **R:** trade-network construction and data preparation
- **igraph:** directed weighted network representation
- **Stata:** panel fixed-effects and difference-in-differences analysis
- **WIOD:** international input-output flows
- **[World Bank Open Data](https://data.worldbank.org/):** macroeconomic and structural controls
