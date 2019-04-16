# CEA Registry Search Function Shiny App

## Background
The [Cost-Effectiveness Analysis (CEA) Registry](https://cevr.tuftsmedicalcenter.org/databases/cea-registry) is a database of all English-language cost-utility analyses published from 1976 to present. The CEA Registry is maintained and updated by Tufts Medical Center's [Center for the Evaluation of Value and Risk in Health (CEVR)](http://cevr.tuftsmedicalcenter.org/). 

This R Shiny App serves a front-end search feature to improve access to the registry for both sponsors and the public. 


## Links and Access
###### [Public Access CEA Registry](https://cevr.shinyapps.io/CEARsearchFreeAccess/)
###### [Full CEA Registry](https://cevr.shinyapps.io/CEARsearchFullAccess/)

Full access to the CEA Registry data is available only to [CEVR sponsors](https://cevr.tuftsmedicalcenter.org/sponsorship). Features not accessible in the public version are: complete database results for queries (free version limited to 1000 results), ability to download the database as an excel spreadsheet, CEVR research support and technical assistance.  

## Code structure
Built with [R Shiny](https://shiny.rstudio.com/) via [R Studio IDE](https://www.rstudio.com/).

Packages used:

 - [Shiny Dashboard](https://rstudio.github.io/shinydashboard/)
 - [DT](https://rstudio.github.io/DT/)
 - [data.table](https://github.com/Rdatatable/data.table/wiki)
 - [readxl](https://readxl.tidyverse.org/)
 - [Shiny JS](https://deanattali.com/shinyjs/)

## Version
Current verion: 1.1.7.

## Screenshots
 
## Authors and Acknowledgement
Funded and supported by [CEVR](http://cevr.tuftsmedicalcenter.org/) and [CEVR sponsors](https://cevr.tuftsmedicalcenter.org/sponsorship). 

Developed by Joanna Emerson, with the support from Brittany D'Cruz, Tara Lavelle, Peter J. Neumann, Josh T. Cohen, Ari Panzer, and Sam Yang.  

<img src="https://pbs.twimg.com/profile_images/958789469632516096/hUT1dpXt.jpg" width="150" height="150"> <img src="https://jobs.tuftsmedicalcenter.org/jobs/ui/images/logo/tufts-medical-center-logo.svg" width="175" height="150">
