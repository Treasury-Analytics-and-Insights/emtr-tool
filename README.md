# EMTR Scenario Family Tool

Currently under development @ The Treasury - New Zealand

Developed by Liam Barnes

## About
The EMTR Scenario Family Tool has been developed as to support policy analysis exploring incentives to work in the tax and welfare system. The tool takes in a set of economic and demographic characteristics of a family alongside tax and welfare parameters for TY24 to determine that family’s eligibility for various income support regimes (welfare payments, tax credits etc). Parameters include; hourly wage, number and age of children (if any), partner wage and income (if any), weekly housing cost, housing type and accommodation supplement rate region (AS Area). Using this information, it calculates EMTRs for that family across a range of levels of gross wage income. 

## Deployment
1. Use `shinylive::export(appdir = "app", destdir = "docs")` to generate shinylive output
2. Use `httpuv::runStaticServer("docs")` to run the static web app

## Github Pages
[Click here](https://treasury-analytics-and-insights.github.io/emtr-tool/) to view a deployed version of the app on Github pages
