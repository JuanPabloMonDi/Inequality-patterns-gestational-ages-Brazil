# Geographical heterogeneity in small-for-gestational-age births in Brazil, 2012–2022: an ecological analysis.

> [!IMPORTANT]
>
> The construction of this README is still in progress and we may do some updates.

On this GitHub we provide all the code and files used in the paper *Geographical heterogeneity in small-for-gestational-age births in Brazil, 2012–2022: an ecological analysis. (preprint)*

## Abstract

<blockquote>
  <sub>
The correlation between small for gestational age (SGA) and perinatal mortality holds significant implications for maternal and neonatal health. Early pregnancy risk assessment to identify infants at risk of SGA birth is considered an effective strategy. Geographic inequities in service access, international migration, and social determinants may shape the spatial distribution of SGA and infant mortality. Objective: This study presents a population-based ecological characterization of SGA prevalence and IMR across Brazil’s 558 microregions (2012–2022), assessing spatial autocorrelation, spatial co-location, and the indirect contribution of migration. Methods: We conducted a nationwide, population-based ecological study of small-for-gestational-age (SGA) births in Brazil from 2012 to 2022. Vital statistics from the Ministry of Health covered 31,351,324 live births aggregated across 558 microregions. We estimated SGA prevalence and infant mortality rates (IMR), applying correction factors from the Brazilian national statistical office to adjust for underreporting. Spatial patterns were assessed using univariate and bivariate analyses to evaluate autocorrelation and the co-location of SGA and IMR at national and state levels. We also applied the demographic balance equation to assess the indirect contribution of migration to SGA and IMR. All analyses were performed in R (via RStudio). Findings: Our findings indicate that SGA and IMR exhibited persistent spatial clustering, with high–high clusters concentrated in economically disadvantaged microregions of the Northeast and in northern frontier microregions of Roraima and northern Amazonas. Bivariate analyses indicated a positive spatial association between higher in-migration and higher SGA prevalence and IMR in these frontier microregions. Interpretation: From 2015 onward, hotspots of SGA and IMR intensified in Brazil’s northern border states, coinciding with marked increases in migrant inflows. Strain on local health systems and constrained access to prenatal and neonatal care may contribute to these patterns. While causal inference is limited by the ecological design, the results highlight priority areas for strengthening maternal–infant services in migrant-receiving regions.
</sub>
</blockquote>

## About this repository

Some information that you might know :

-   Since the raw data is too large and heavy to be uploaded. It was not possible for us to attach it directly here. Fortunately, the data used is public and you can find it in the open data portal of the Sistema Único de Saúde (SUS, Brazil's public funded health system) which you can access [here](https://opendatasus.saude.gov.br/).

    -   For births, go to the [OpenDataSUS portal](https://opendatasus.saude.gov.br/) and search for "Nascidos Vivos" or "SINASC" to get the births data by year. On our code, we saved this files in the *data* folder under the name `SINASC_*year*.csv`.

    -   For deaths, go to the same portal and write "Mortalidade Geral" or "SIM" to get the deaths by year. Save this data on the *data* folder under the name `Mortalidade_Geral_*year*.csv`

    -   For subnotification on births and deaths, the data is available in the Brazilian Statistics Bureau site, IBGE. You can download this data [here](https://ftp.ibge.gov.br/Estatisticas_Vitais/Estimativas_sub_registro_nascimentos/2018/xlsx/). Note that this are several files, on this case, we saved these files on the *data/subnotification* folder, with name `Sub_nascidos_*year*.csv` in case of births and `SINASC_obitos_*year*.csv` for deaths.


This repository contains the following components:

### 📁 Folders

1. **data**: Contains all the necessary input data (births and deaths by year), except the previously mentioned files.
2. **Maps and Tables**: Stores all the maps, plots, and tables used in the article.
3. **Miscellaneous Material**: Includes supplementary materials, mainly from earlier versions of the paper.

### 📄 R Scripts

4. **Lancet 1 - Functions.R**: Processes and merges birth and death data for the selected year and region. It also calculates infant mortality rates at the city level for each year.
5. **Lancet 2 - Maps and Graphics.R**: After running *Lancet 1*, this script generates the maps and graphics shown in the paper (output is saved in the *Maps and Tables* folder).

## Next steps:

-    If necessary, we may add a link to a Google Drive folder with some RData to give the option of avoid intensive computation parts.

-   In case you wanna cite this GitHub, please use the same citation of the paper. (You will find it here soon 😊)
