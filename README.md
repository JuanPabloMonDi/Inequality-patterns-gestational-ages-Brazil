# Geographical heterogeneity in small-for-gestational-age births in Brazil, 2012–2022: an ecological analysis.

> [!IMPORTANT]
>
> The construction of this README is still in progress and we may do some updates.

On this GitHub we provide all the code and files used in the paper *Geographical heterogeneity in small-for-gestational-age births in Brazil, 2012–2022: an ecological analysis. (preprint)*

Some information that you might know :

-   Since the raw data is too large and heavy to be uploaded. It was not possible for us to attach it directly here. Fortunately, the data used is public and you can find it in the open data portal of the Sistema Único de Saúde (SUS, Brazil's public funded health system) which you can access [here](https://opendatasus.saude.gov.br/).

    -   For births, go to the [OpenDataSUS portal](https://opendatasus.saude.gov.br/) and search for "Nascidos Vivos" or "SINASC" to get the births data by year. On our code, we saved this files in the *data* folder under the name `SINASC_*year*.csv`.

    -   For deaths, go to the same portal and write "Mortalidade Geral" or "SIM" to get the deaths by year. Save this data on the *data* folder under the name `Mortalidade_Geral_*year*.csv`

    -   For subnotification on births and deaths, the data is available in the Brazilian Statistics Bureau site, IBGE. You can download this data [here](https://ftp.ibge.gov.br/Estatisticas_Vitais/Estimativas_sub_registro_nascimentos/2018/xlsx/). Note that this are several files, on this case, we saved these files on the *data/subnotification* folder, with name `Sub_nascidos\_\*year\*.csv` in case of births and `SINASC_obitos\_\*year\*.csv` for deaths.

-   The link to a Google Drive folder with some RData to give the option to avoid intensive computation parts.

-   Next step: Explain briefly the files and folders on this repository

-   In case you wanna cite this GitHub, please use the same citation of the paper. We will provide it here briefly

-   Next steps: Provide a brief context of the article
