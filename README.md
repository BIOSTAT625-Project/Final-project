# Final-project: Depression Surveillance based on Google Trends

In this project, we first collect great amount of raw data from Google trends and other websites and then process these data with some algorithm. Relationship between search volume of depression and other factors is studied and predictions are made. It shows that higher employment and higher temperature are related to lower population depression. We further build an R shiny Application to visualize the search volumes of depression-related words by state compared with different factors. Both line graph and map can be shown interactively.

A fantastic R Shiny Application can be accessed via [https://conchaespina.shinyapps.io/FinalProject625/]( https://conchaespina.shinyapps.io/FinalProject625/)

This repository contains major materials (scripts and documents) about our project:

- `README.md`
- `app.R`: Codes for Shiny app
- `Helper-script/`:
  - `gtrendsR_collect_data_slowly.R`: Codes to collect data via package `gtrendsR`
  - `google_trends_data_cleaning.R`: Primary cleaning step for Google Trends data
  - `air_quality_data_API.R`: Codes to collect AQI data
  - `data_clean.R`: Transformation and combination of Google Trends data and weather data
  - `ARIMAbygroup.R`: Codes for ARIMA model and search volume prediction
- `Report/`: R Markdown files and knitted pdf files of final report and figures embedded
