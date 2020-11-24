# DA2_and_Coding1_Assign_COVID19

This is the GitHub repo for the joint assignment for DA2 and Coding1 on the assoiation between registered covid-19 cases and registered number
of death due to covid-19 on a given day.

The repo contains:

- **Data folder**: The clean data set contains daily covid19 case reports on October 13, 2020, which includes daily confirmed cases, daily death cases, daily recovered cases, and daily active cases from 182 listed countries. 

Daily confirmed cases include confirmed and probable (where reported).
Daily death cases include confirmed and probable (where reported).
Daily recovered cases are estimates based on local media, reports, and state and local reporting when available, and therefore maybe substantially lower than the true number.
Daily active cases are calculated as the total daily confirmed cases minus daily death cases and daily recovered cases.

This data set is taken from: 
https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data 
Originally from 2019 Novel Coronavirus Visual Dashboard operated by the Johns Hopkins University Centre for Systems Science and Engineering (JHU CSSE)

Population in 2019 from listed countries is also included in this data set. It is downloaded from the World Bank- World Development Indicators. The number of population is in ten thousands. 
link: https://data.worldbank.org/indicator/SP.POP.TOTL
 
- **Codes folder**:The codes folder includes the following:

1. covid_get_data.R: this is the R script that collects all the data used in this analysis.

2. covid_clean_data.R: this is the R script that contains the steps of cleaning the data set before doing analysis.

3. covid_analysis_data.R: this is the R script with all the analysis part of this project. 

4. covid_analysis_data.Rmd: same as covid_analysis _data.R but in .Rmd. You should be able to replicate my results by running this Rmd file.

5. Joint_Assignment_COVID.Rproj: Project file is also included so you should be able to open all the R scripts and Rmd file in this project. 
 
- **Docs folder**: it contains both .html and .pdf generated from .rmd.

- **Output folder**: in this folder, you can find the code generated model summary statistics table in the html format, and also a screenshot of the html result.
