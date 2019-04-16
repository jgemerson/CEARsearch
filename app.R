##################################################################
#                             Jojo Emerson                       #
#              CEA registry search function: FREE ACCESS         #
#                     Code changes: April 10, 2019               #
#                    GitHub Upload: April 16, 2019               #
#                             Version 1.1.8                      #
##################################################################


############              REQUIRED FILES              ############

# Main directory: 
#  - app.R: R shiny app (current file)
#  - cleaning.R: data cleaning file (restricted for GitHub upload)
#  - google-analytics.js: google analytics (restricted for GitHub upload)
# Data directory: (data files restricted for GitHub upload)
#   - METHODS.xlsx: CEA Reg methods from Nov 28 2018
#   - RATIOS.xlsx: CEA Reg ratios from Nov 28 2018
#   - WEIGHTS.xlsx: CEA Reg weights from Nov 28 2018
#   - CleanInterventionType_Ratios.xlsx: from Jan 10 2019
#   - METHODS_countries.xlsx: from Jan 11 2019
#   - RATIOS_countries.xlsx: from Jan 11 2019
# rsconnect directory: (restricted for GitHub upload)
#  - connection to shinyapps.io account for deployment
# www directory:
#   - cea.png: CEA reg logo for footer
#   - cea-knockout.png: CEA reg logo for footer
#   - cevr.png: CEVR logo for footer

#call libraries
library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(readxl)
library(shinyjs)

#source cleaning file - omitted from GitHub upload
#source('cleaning.R')
#upload test data
Countries_methods<<-readRDS("data/Countries_methods.rds")
Countries_ratios<<-readRDS("data/Countries_ratios.rds")
InterventionTypes<<-readRDS("data/InterventionTypes.rds")
keywords<<-readRDS("data/keywords.rds")
links<<-readRDS("data/links.rds")
methodsdf<<-readRDS("data/methodsdf.rds")
methodsfull<<-readRDS("data/methodsfull.rds")
ratiosdf<<-readRDS("data/ratiosdf.rds")
ratiosfull<<-readRDS("data/ratiosfull.rds")
weightsdf<<-readRDS("data/weightsdf.rds")
weightsfull<<-readRDS("data/weightsfull.rds")

#Create disease choices vector
disease_choices<-sort(unique(c(methodsfull$DisChapter1,methodsfull$DisChapter2,methodsfull$DisChapter3,methodsfull$DisChapter4,
                               methodsfull$DisCode1, methodsfull$DisCode2, methodsfull$DisCode3, methodsfull$DisCode4)))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  #header
  dashboardHeader(title = "CEA Registry Search: Free Access", titleWidth  = 350
  ), #close header
  
  #sidebar
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                #methods tab
                menuItem("Methods", tabName = "methods", icon = icon("search")),
                #ratios tab
                menuItem("Ratios", tabName = "ratios", icon = icon("search")),
                #weights tab
                menuItem("Utility Weights", tabName = "weights", icon = icon("search")),
                
                #footer with contact and logos
                tags$div(style = "position:fixed; bottom: 0;padding:25px;",
                         tags$br(),
                         
                         #sponsor log in button
                         tags$a(style =  "position:center; padding:10px; border: 1.5px; border-radius: 2px; border-color: #fff; color: #fff; background-color: #275d8b;",
                                "Sponsor Login", href="https://cevr.shinyapps.io/CEARsearchFullAccess/"
                         ),
                         tags$br(), tags$br(),
                         
                         #logos
                         tags$br(),
                         tags$a(tags$img(src ="cevr-knockout.png",  width = "120px", height = "60px"), href="https://cevr.tuftsmedicalcenter.org/", target = "_blank"),
                         tags$br(), tags$br(),
                         tags$a(tags$img(src = "cea-knockout.png",width = "120px", height = "60px"), href="https://cevr.tuftsmedicalcenter.org/databases/cea-registry", target = "_blank"),
                         
                         #developer info
                         tags$br(), tags$br(),
                         "Developer:",
                         tags$br(),
                         tags$a("Joanna Emerson", href="mailto:jemerson@tuftsmedicalcenter.org")
                )
                
                
    ) #close sidebar menu
  ), #close sidebar
  
  dashboardBody(
    
    #link to google analytics javascript file
    #tags$head(includeScript("google-analytics.js")),
    
    useShinyjs(),
    
    tabItems(
      
      #methods tab
      tabItem(tabName = "methods",
              
              #change font
              tags$head(tags$style(HTML('
                                        * {
                                        font-family: Arial, sans-serif; !important
                                        }
                                        '))),
              
              #methods selectize inputs
              fluidRow(
                column(width = 12,
                       #keywords filter
                       selectizeInput(inputId = "methods_keywords", label = "Keywords (PubMed)", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'), width = '100%')
                )
              ),
              
              fluidRow(
                column(width = 3,
                       #Author filter
                       selectizeInput(inputId = "methods_author", label = "Primary Author", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                ),
                
                column(width = 3,
                       #PubYear filter
                       sliderInput("methods_year", "Publication year", min = 1976, max = 2018, value = c(1976, 2018), step = 1, dragRange = TRUE, sep = "", ticks = FALSE)
                ),
                column(width = 3,
                       #Country filter
                       selectizeInput(inputId = "methods_country", label = "Country", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                ),
                #ICD10 filter
                column(width = 3,
                       selectizeInput(inputId = "methods_icd10", label = "Disease (ICD10)", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                )
              ), #close fluid row 1
              
              fluidRow(
                #Prevention stage filter
                column(width = 3,
                       selectizeInput(inputId = "methods_prev", label = "Prevention stage", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                ),
                #Intervention type filter
                column(width = 3,
                       selectizeInput(inputId = "methods_inttype", label = "Intervention Type", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                ),
                #perspective filter
                column(width = 3,
                       selectizeInput(inputId = "methods_perspective", label = "Study Perspective", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                ),
                #quality filter
                column(width = 3,
                       sliderInput("methods_rating", "Article quality rating", min = 0, max = 7, value = c(0, 7), step = .25, dragRange = TRUE, sep = "", ticks = FALSE)
                )
              ), #close row 2
              
              
              fluidRow(
                #ArticleID filter
                column(width = 12,
                       shinyjs::hidden(selectizeInput(inputId = "methods_ArticleID", label = "Article ID", choices = NULL, multiple = TRUE, options = list(placeholder = 'None selected'), width = '100%'))
                )
              ), #close row 4
              
              fluidRow(
                column(width = 6, align = 'left',
                       #view ratios button
                       actionButton('methods_viewratios', "View ratios for this subset",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       #view weights button
                       actionButton('methods_viewweights', "View weights for this subset",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                
                column(width = 6, align = 'right',
                       #advanced filter button 
                       actionButton('methods_showIDs', "Article IDs",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       #clear filters button
                       actionButton('methods_reset', "Clear filters",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )), #close row 5
              
              fluidRow(
                column(width = 6,
                       htmlOutput(outputId = "methods_error"),
                       "Click above buttons to view corresponding ratio or weight information for currently displayed articles. Filter selections will remain in this tab."
                ), 
                column(width = 6, align = "right",
                       "Information in the table may be incomplete due to changes in database structure over time and our partial review triaging process."
                )
              ),
              
              fluidRow(
                column(width = 3),
                #currently filtered text
                column(width = 6, align = "center", style = "border: 2px solid silver; border-radius: 10px;",
                       shinyjs::hidden(htmlOutput("methods_filtered"))),
                
                tags$br()
              ), #close row 6
              
              fluidRow(
                htmlOutput("sponsorship_blurb")
              ), #close row 7
              
              #methods data table   
              fluidRow(
                DT::dataTableOutput("methods_data")
              )#close row 8
              
              ), #close methods tab
      
      #ratios tab
      tabItem(tabName = "ratios",
              
              #ratios selectize input
              fluidRow(
                column(width = 12,
                       #keywords filter
                       selectizeInput(inputId = "ratios_keywords", label = "Keywords (PubMed)", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'), width = '100%')
                )
              ),
              
              fluidRow(
                # country filter
                column(width = 4,
                       selectizeInput(inputId = "ratios_country", label = "Target population*: Country", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                ),
                #target pouplation: gender
                column(width = 4,
                       selectizeInput(inputId = "ratios_gender", label = "Target population*: Gender", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                ),
                #target population: age
                column(width = 4,
                       selectizeInput(inputId = "ratios_age", label = "Target population*: Age", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                )
              ), #close row 2
              
              fluidRow(
                #Intervention type filter
                column(width = 4,
                       selectizeInput(inputId = "ratios_int", label = "Intervention type", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                ),
                column(width = 4,
                       #PubYear filter
                       sliderInput("ratios_year", "Publication year", min = 1976, max = 2018, value = c(1976, 2018), step = 1, dragRange = TRUE, sep = "", ticks = FALSE)
                ),
                column(width = 4,
                       selectizeInput(inputId = "ratios_quadrant", label = "Cost-effectiveness Quadrant", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                )),#close row 3
              
              fluidRow(column(width = 12, align = 'right',
                              "*Target population country, gender, and age only collected from 2012 onward.",
                              tags$br()
              )),
              
              fluidRow(
                #ArticleID filter
                column(width = 12,
                       shinyjs::hidden(selectizeInput(inputId = "ratios_ArticleID", label = "Article ID", choices = NULL, multiple = TRUE, options = list(placeholder = 'None selected'), width = '100%'))
                )),
              
              fluidRow(
                column(width = 6, align = 'left',
                       #view methods button
                       actionButton('ratios_viewmethods', "View methods for this subset",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       #view weights button
                       actionButton('ratios_viewweights', "View weights for this subset",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                
                #clear filters button
                column(width = 6, align = 'right',
                       actionButton('ratios_showIDs', "Article IDs",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       actionButton('ratios_reset', "Clear filters",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )
              ), #close row 4
              
              fluidRow(
                column(width = 6,
                       "Click above buttons to view corresponding method or weight information for currently displayed ratios. Filter selections will remain in this tab."
                ), 
                column(width = 6, align = "right",
                       "Information in the table may be incomplete due to changes in database structure over time and our partial review triaging process."
                )
              ),
              
              fluidRow(
                column(width = 3),
                #currently filtered text
                column(width = 6, align = "center", style = "border: 2px solid silver; border-radius: 10px;",
                       shinyjs::hidden(htmlOutput("ratios_filtered"))),
                
                tags$br()
              ), #close row 6
              
              fluidRow(
                htmlOutput("ratios_sponsorship_blurb")
              ), #close row 7
              
              #ratios datatable
              fluidRow(
                DT::dataTableOutput("ratios_data")
              ) #close row 8
              
      ), #close ratios tab
      
      #weights tab
      tabItem(tabName = "weights",
              
              #selectize inputs for weights
              fluidRow(
                column(width = 12,
                       #keywords filter
                       selectizeInput(inputId = "weights_keywords", label = "Keywords (PubMed)", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'), width = '100%')
                )
              ),
              
              fluidRow(
                column(width = 4,
                       #health state ID filter
                       selectizeInput(inputId = "weights_healthstate", label = "Health state", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                ),
                column(width = 4,
                       #utility weight filter
                       sliderInput(inputId = "weights_utilityweight", label = "Utility weight value",  min = -1, max = 1, value = c(-1, 1), step = .01, dragRange = TRUE, sep = "", ticks = FALSE)
                ),
                column(width = 4,
                       #disease filter
                       selectizeInput("weights_disease", label = "Disease (ICD10)", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                )
              ), #close row 1
              
              fluidRow(
                column(width = 4,
                       #PubYear filter
                       sliderInput("weights_year", "Publication year", min = 1976, max = 2018, value = c(1976, 2018), step = 1, dragRange = TRUE, sep = "", ticks = FALSE)
                ),
                column(width = 4,
                       #source filter
                       selectizeInput(inputId = "weights_source", label = "Source", choices = NULL, multiple = TRUE, options = list(placeholder = 'Select...'))
                )),
              
              fluidRow(
                column(width = 12,
                       #Article ID filter
                       shinyjs::hidden(selectizeInput(inputId = "weights_ArticleID", label = "Article ID", choices = NULL, multiple = TRUE, options = list(placeholder = 'None selected'),width = '100%'))
                )
              ),
              
              fluidRow(
                column(width = 6, align = 'left',
                       #view methods button
                       actionButton('weights_viewmethods', "View methods for this subset",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       #view weights button
                       actionButton('weights_viewratios', "View ratios for this subset",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                #clear filters button
                column(width = 6, align = 'right',
                       actionButton('weights_showIDs', "Article IDs", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       actionButton('weights_reset', "Clear filters",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )), #close row 3
              
              fluidRow(
                column(width = 6,
                       "Click above buttons to view corresponding method or ratio information for currently displayed weights. Filter selections will remain in this tab."
                ), 
                column(width = 6, align = "right",
                       "Information in the table may be incomplete due to changes in database structure over time and our partial review triaging process."
                )
              ),
              
              fluidRow(
                column(width = 3),
                #currently filtered text
                column(width = 6, align = "center", style = "border: 2px solid silver; border-radius: 10px;",
                       shinyjs::hidden(htmlOutput("weights_filtered"))),
                
                tags$br()
              ), #close row 6
              
              fluidRow(
                htmlOutput("weights_sponsorship_blurb")
              ), #close row 7
              
              #weights dataframe
              fluidRow(
                DT::dataTableOutput("weights_data")
              ) #close row 8
              
      )#close weights tab
              )#close tab items
              )#close body  
  
    ) #close dashboard page

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #update selectize inputs - methods
  updateSelectizeInput(session, "methods_ArticleID", choices = c('',methodsdf$ArticleID), server = TRUE)
  updateSelectizeInput(session, "methods_keywords", choices = c('', keywords$Keywords), server = TRUE)
  updateSelectizeInput(session, "methods_author",choices = c('',methodsdf$PrimAuthLastName), server = TRUE)
  updateSelectizeInput(session, "methods_country", choices = c('', "Afghanistan",	"Albania",	"Algeria",	"American Samoa",	"Andorra",	 "Angola",	"Antigua and Barbuda",	 "Argentina",	"Armenia",	 "Aruba",	 "Australia",	"Austria",	"Azerbaijan",	"Bahamas",	"Bahrain",	"Bangladesh",	"Barbados",	"Belarus",	"Belgium",	 "Belize",	 "Benin",	"Bermuda",	 "Bhutan",	"Bolivia",	 "Bosnia and Herzegovina",	"Botswana",	 "Brazil",	 "British Virgin Islands",	 "Brunei",	"Bulgaria",	 "Burkina Faso",	"Burundi",	"Cabo Verde",	"Cambodia",	"Cameroon",	 "Canada",	 "Cayman Islands",	"Central African Republic",	"Chad",	 "Channel Islands",	 "Chile",	 "China",	"Colombia",	"Comoros",	"Democratic Republic of Congo",	 "Congo",	"Costa Rica",	"Cote dIvoire",	"Croatia",	"Cuba",	 "Curaco",	 "Cyprus",	"Czech Republic",	"Denmark",	"Djibouti",	"Dominica",	 "Dominican Republic",	"Ecuador",	 "Egypt",	"El Salvador",	 "Equatorial Guinea",	"Eritrea",	"Estonia",	"Ethiopia",	 "Faroe Islands",	"Fiji",	"Finland",	 "France",	 "French Polynesia",	 "Gabon",	"The Gambia",	"Georgia",	"Germany",	 "Ghana",	 "Gibraltar",	 "Greece",	 "Greenland",	"Grenada",	"Guam",	"Guatemala",	 "Guinea",	"Guinea Bissau",	 "Guyana",	 "Haiti",	 "Honduras",	"Hong Kong",	"Hungary",	"Iceland",	 "India",	"Indonesia",	"Iran",	"Iraq",	"Ireland",	"Isle of Man",	 "Israel",	 "Italy",	"Jamaica",	 "Japan",	 "Jordan",	"Kazakhstan",	 "Kenya",	"Kiribati",	"North Korea",	 "South Korea",	 "Kosovo",	 "Kuwait",	 "Kyrgyz Republic",	"Laos",	 "Latvia",	"Lebanon",	"Lesotho",	"Liberia",	 "Libya",	"Liechtenstein",	 "Lithuania",	"Luxembourg",	 "Macao",	 "Macedonia",	"Madagascar",	 "Malawi",	"Malaysia",	"Maldives",	"Mali",	 "Malta",	 "Marshall Islands",	"Mauritania",	 "Mauritius",	 "Mexico",	"Micronesia",	"Moldova",	 "Monaco",	"Mongolia",	"Montenegro",	"Morocco",	"Mozambique",	"Myanmar",	"Namibia",	 "Nauru",	 "Nepal",	"Netherlands",	"New Caledonia",	 "New Zealand",	 "Nicaragua",	 "Niger",	"Nigeria",	"Northern Mariana Islands",	 "Norway",	"Oman",	"Pakistan",	 "Palau",	 "Panama",	 "Papua New Guinea",	"Paraguay",	"Peru",	"Philippines",	 "Poland",	"Portugal",	"Puerto Rico",	 "Qatar",	"Romania",	"Russian Federation",	 "Rwanda",	 "Samoa",	"San Marino",	 "Sao Tome and Principe",	 "Saudi Arabia",	"Senegal",	 "Serbia",	 "Seychelles",	 "Sierra Leone",	 "Singapore",	 "Sint Maarten Dutchpart",	"Slovak Republic",	"Slovenia",	"Solomon Islands",	"Somalia",	"South Africa",	"South Sudan",	"Spain",	"Sri Lanka",	"St Kitts and Nevis",	"St Lucia",	"St Martin",	"St Vincent and the Grenadines",	"Sudan",	"Suriname",	"Swaziland",	 "Sweden",	"Switzerland",	"Syria",	"Tajikistan",	"Tanzania",	"Thailand",	 "Timor Leste",	 "Togo",	"Tonga",	"Trinidad and Tobago",	"Tunisia",	 "Turkey",	"Turkmenistan",	 "Turks and Caicos",	 "Tuvalu",	 "Uganda",	"Ukraine",	"United Arab Emirates",	 "United Kingdom",	"United States",	"Uruguay",	 "Uzbekistan",	"Vanuatu",	"Venezuela",	"Vietnam",	"Virgin Islands US",	"Virgin Islands US",	 "Yemen",	 "Zambia",	"Zimbabwe",	 "Taiwan",	"Cook Islands",	"Niue",	 "Wallis and Futuna",	"NA"), server = TRUE)
  updateSelectizeInput(session, "methods_inttype", choices = c('', "Care Delivery","Diagnostic","Health Education/Behavior","Immunization","Medical Device", "Medical Procedure","Pharmaceutical","Screening","Surgical","Other","None/NA"), server = TRUE)
  updateSelectizeInput(session, "methods_prev", choices = c('','Primary', 'Secondary', 'Tertiary'), server = TRUE)
  updateSelectizeInput(session, "methods_icd10", choices = c('',disease_choices), server = TRUE)
  updateSelectizeInput(session, "methods_perspective", choices = c('',"Societal","Health Care Payer","Limited Societal","Health Care Sector","Not stated","Other"), server = TRUE)
  #reset filters when reset button is hit
  observeEvent(input$methods_reset,{
    updateSelectizeInput(session, "methods_ArticleID", choices = c('',methodsdf$ArticleID), server = TRUE)
    updateSelectizeInput(session, "methods_keywords", choices = c('', keywords$Keywords), server = TRUE)
    updateSelectizeInput(session, "methods_country", choices = c('', "Afghanistan",	"Albania",	"Algeria",	"American Samoa",	"Andorra",	 "Angola",	"Antigua and Barbuda",	 "Argentina",	"Armenia",	 "Aruba",	 "Australia",	"Austria",	"Azerbaijan",	"Bahamas",	"Bahrain",	"Bangladesh",	"Barbados",	"Belarus",	"Belgium",	 "Belize",	 "Benin",	"Bermuda",	 "Bhutan",	"Bolivia",	 "Bosnia and Herzegovina",	"Botswana",	 "Brazil",	 "British Virgin Islands",	 "Brunei",	"Bulgaria",	 "Burkina Faso",	"Burundi",	"Cabo Verde",	"Cambodia",	"Cameroon",	 "Canada",	 "Cayman Islands",	"Central African Republic",	"Chad",	 "Channel Islands",	 "Chile",	 "China",	"Colombia",	"Comoros",	"Democratic Republic of Congo",	 "Congo",	"Costa Rica",	"Cote dIvoire",	"Croatia",	"Cuba",	 "Curaco",	 "Cyprus",	"Czech Republic",	"Denmark",	"Djibouti",	"Dominica",	 "Dominican Republic",	"Ecuador",	 "Egypt",	"El Salvador",	 "Equatorial Guinea",	"Eritrea",	"Estonia",	"Ethiopia",	 "Faroe Islands",	"Fiji",	"Finland",	 "France",	 "French Polynesia",	 "Gabon",	"The Gambia",	"Georgia",	"Germany",	 "Ghana",	 "Gibraltar",	 "Greece",	 "Greenland",	"Grenada",	"Guam",	"Guatemala",	 "Guinea",	"Guinea Bissau",	 "Guyana",	 "Haiti",	 "Honduras",	"Hong Kong",	"Hungary",	"Iceland",	 "India",	"Indonesia",	"Iran",	"Iraq",	"Ireland",	"Isle of Man",	 "Israel",	 "Italy",	"Jamaica",	 "Japan",	 "Jordan",	"Kazakhstan",	 "Kenya",	"Kiribati",	"North Korea",	 "South Korea",	 "Kosovo",	 "Kuwait",	 "Kyrgyz Republic",	"Laos",	 "Latvia",	"Lebanon",	"Lesotho",	"Liberia",	 "Libya",	"Liechtenstein",	 "Lithuania",	"Luxembourg",	 "Macao",	 "Macedonia",	"Madagascar",	 "Malawi",	"Malaysia",	"Maldives",	"Mali",	 "Malta",	 "Marshall Islands",	"Mauritania",	 "Mauritius",	 "Mexico",	"Micronesia",	"Moldova",	 "Monaco",	"Mongolia",	"Montenegro",	"Morocco",	"Mozambique",	"Myanmar",	"Namibia",	 "Nauru",	 "Nepal",	"Netherlands",	"New Caledonia",	 "New Zealand",	 "Nicaragua",	 "Niger",	"Nigeria",	"Northern Mariana Islands",	 "Norway",	"Oman",	"Pakistan",	 "Palau",	 "Panama",	 "Papua New Guinea",	"Paraguay",	"Peru",	"Philippines",	 "Poland",	"Portugal",	"Puerto Rico",	 "Qatar",	"Romania",	"Russian Federation",	 "Rwanda",	 "Samoa",	"San Marino",	 "Sao Tome and Principe",	 "Saudi Arabia",	"Senegal",	 "Serbia",	 "Seychelles",	 "Sierra Leone",	 "Singapore",	 "Sint Maarten Dutchpart",	"Slovak Republic",	"Slovenia",	"Solomon Islands",	"Somalia",	"South Africa",	"South Sudan",	"Spain",	"Sri Lanka",	"St Kitts and Nevis",	"St Lucia",	"St Martin",	"St Vincent and the Grenadines",	"Sudan",	"Suriname",	"Swaziland",	 "Sweden",	"Switzerland",	"Syria",	"Tajikistan",	"Tanzania",	"Thailand",	 "Timor Leste",	 "Togo",	"Tonga",	"Trinidad and Tobago",	"Tunisia",	 "Turkey",	"Turkmenistan",	 "Turks and Caicos",	 "Tuvalu",	 "Uganda",	"Ukraine",	"United Arab Emirates",	 "United Kingdom",	"United States",	"Uruguay",	 "Uzbekistan",	"Vanuatu",	"Venezuela",	"Vietnam",	"Virgin Islands US",	"Virgin Islands US",	 "Yemen",	 "Zambia",	"Zimbabwe",	 "Taiwan",	"Cook Islands",	"Niue",	 "Wallis and Futuna",	"NA"), server = TRUE)
    updateSelectizeInput(session, "methods_author",choices = c('',methodsdf$PrimAuthLastName), server = TRUE)
    updateSelectizeInput(session, "methods_inttype", choices = c('', "Care Delivery","Diagnostic","Health Education/Behavior","Immunization","Medical Device", "Medical Procedure","Pharmaceutical","Screening","Surgical","Other","None/NA"), server = TRUE)
    updateSelectizeInput(session, "methods_prev", choices = c('','Primary', 'Secondary', 'Tertiary'), server = TRUE)
    updateSelectizeInput(session, "methods_icd10", choices = c('',disease_choices), server = TRUE)
    updateSliderInput(session, 'methods_year', min = 1976, max = 2018, value = c(1976, 2018), step = 1)
    updateSelectizeInput(session, "methods_perspective", choices = c('',"Societal","Health Care Payer","Limited Societal","Health Care Sector","Not stated","Other"), server = TRUE)
    updateSliderInput(session, 'methods_rating', min = 0, max = 7, value = c(0, 7), step = 0.25)
  })
  
  #update selectize inputs - ratios
  updateSelectizeInput(session, "ratios_keywords", choices = c('', keywords$Keywords), server = TRUE)
  updateSelectizeInput(session, "ratios_ArticleID", choices = c('',ratiosdf$ArticleID), server = TRUE)
  updateSelectizeInput(session, "ratios_country", choices = c('', "Afghanistan",	"Albania",	"Algeria",	"American Samoa",	"Andorra",	 "Angola",	"Antigua and Barbuda",	 "Argentina",	"Armenia",	 "Aruba",	 "Australia",	"Austria",	"Azerbaijan",	"Bahamas",	"Bahrain",	"Bangladesh",	"Barbados",	"Belarus",	"Belgium",	 "Belize",	 "Benin",	"Bermuda",	 "Bhutan",	"Bolivia",	 "Bosnia and Herzegovina",	"Botswana",	 "Brazil",	 "British Virgin Islands",	 "Brunei",	"Bulgaria",	 "Burkina Faso",	"Burundi",	"Cabo Verde",	"Cambodia",	"Cameroon",	 "Canada",	 "Cayman Islands",	"Central African Republic",	"Chad",	 "Channel Islands",	 "Chile",	 "China",	"Colombia",	"Comoros",	"Democratic Republic of Congo",	 "Congo",	"Costa Rica",	"Cote dIvoire",	"Croatia",	"Cuba",	 "Curaco",	 "Cyprus",	"Czech Republic",	"Denmark",	"Djibouti",	"Dominica",	 "Dominican Republic",	"Ecuador",	 "Egypt",	"El Salvador",	 "Equatorial Guinea",	"Eritrea",	"Estonia",	"Ethiopia",	 "Faroe Islands",	"Fiji",	"Finland",	 "France",	 "French Polynesia",	 "Gabon",	"The Gambia",	"Georgia",	"Germany",	 "Ghana",	 "Gibraltar",	 "Greece",	 "Greenland",	"Grenada",	"Guam",	"Guatemala",	 "Guinea",	"Guinea Bissau",	 "Guyana",	 "Haiti",	 "Honduras",	"Hong Kong",	"Hungary",	"Iceland",	 "India",	"Indonesia",	"Iran",	"Iraq",	"Ireland",	"Isle of Man",	 "Israel",	 "Italy",	"Jamaica",	 "Japan",	 "Jordan",	"Kazakhstan",	 "Kenya",	"Kiribati",	"North Korea",	 "South Korea",	 "Kosovo",	 "Kuwait",	 "Kyrgyz Republic",	"Laos",	 "Latvia",	"Lebanon",	"Lesotho",	"Liberia",	 "Libya",	"Liechtenstein",	 "Lithuania",	"Luxembourg",	 "Macao",	 "Macedonia",	"Madagascar",	 "Malawi",	"Malaysia",	"Maldives",	"Mali",	 "Malta",	 "Marshall Islands",	"Mauritania",	 "Mauritius",	 "Mexico",	"Micronesia",	"Moldova",	 "Monaco",	"Mongolia",	"Montenegro",	"Morocco",	"Mozambique",	"Myanmar",	"Namibia",	 "Nauru",	 "Nepal",	"Netherlands",	"New Caledonia",	 "New Zealand",	 "Nicaragua",	 "Niger",	"Nigeria",	"Northern Mariana Islands",	 "Norway",	"Oman",	"Pakistan",	 "Palau",	 "Panama",	 "Papua New Guinea",	"Paraguay",	"Peru",	"Philippines",	 "Poland",	"Portugal",	"Puerto Rico",	 "Qatar",	"Romania",	"Russian Federation",	 "Rwanda",	 "Samoa",	"San Marino",	 "Sao Tome and Principe",	 "Saudi Arabia",	"Senegal",	 "Serbia",	 "Seychelles",	 "Sierra Leone",	 "Singapore",	 "Sint Maarten Dutchpart",	"Slovak Republic",	"Slovenia",	"Solomon Islands",	"Somalia",	"South Africa",	"South Sudan",	"Spain",	"Sri Lanka",	"St Kitts and Nevis",	"St Lucia",	"St Martin",	"St Vincent and the Grenadines",	"Sudan",	"Suriname",	"Swaziland",	 "Sweden",	"Switzerland",	"Syria",	"Tajikistan",	"Tanzania",	"Thailand",	 "Timor Leste",	 "Togo",	"Tonga",	"Trinidad and Tobago",	"Tunisia",	 "Turkey",	"Turkmenistan",	 "Turks and Caicos",	 "Tuvalu",	 "Uganda",	"Ukraine",	"United Arab Emirates",	 "United Kingdom",	"United States",	"Uruguay",	 "Uzbekistan",	"Vanuatu",	"Venezuela",	"Vietnam",	"Virgin Islands US",	"Virgin Islands US",	 "Yemen",	 "Zambia",	"Zimbabwe",	 "Taiwan",	"Cook Islands",	"Niue",	 "Wallis and Futuna",	"NA"), server = TRUE)
  updateSelectizeInput(session, "ratios_gender", choices = c('',"Both genders", "Female", "Male", "Gender not specified"), server = TRUE)
  updateSelectizeInput(session, "ratios_age", choices = c('',"0-18 years","19-40 years","41-64 years","65+ years","Adult (unspecified)","Age unknown"), server = TRUE)
  updateSelectizeInput(session, "ratios_int", choices = c('', "Care Delivery",	"Diagnostic",	"Health Education or Behavior",	"Immunization",	"Medical Device",	"Medical Procedure",	"Pharmaceutical",	"Screening",	"Surgical",	"Other",	"None",	"Multiple Intervention Types"), server = TRUE)
  updateSelectizeInput(session, "ratios_quadrant", choices = c('', "Health gain/More cost", "Health loss/More cost", "Health gain/Less cost", "Health loss/Less cost"), server = TRUE)
  #reset filters when reset button is hit
  observeEvent(input$ratios_reset,{
    updateSelectizeInput(session, "ratios_keywords", choices = c('', keywords$Keywords), server = TRUE)
    updateSelectizeInput(session, "ratios_ArticleID", choices = c('',ratiosdf$ArticleID), server = TRUE)
    updateSelectizeInput(session, "ratios_country", choices = c('', "Afghanistan",	"Albania",	"Algeria",	"American Samoa",	"Andorra",	 "Angola",	"Antigua and Barbuda",	 "Argentina",	"Armenia",	 "Aruba",	 "Australia",	"Austria",	"Azerbaijan",	"Bahamas",	"Bahrain",	"Bangladesh",	"Barbados",	"Belarus",	"Belgium",	 "Belize",	 "Benin",	"Bermuda",	 "Bhutan",	"Bolivia",	 "Bosnia and Herzegovina",	"Botswana",	 "Brazil",	 "British Virgin Islands",	 "Brunei",	"Bulgaria",	 "Burkina Faso",	"Burundi",	"Cabo Verde",	"Cambodia",	"Cameroon",	 "Canada",	 "Cayman Islands",	"Central African Republic",	"Chad",	 "Channel Islands",	 "Chile",	 "China",	"Colombia",	"Comoros",	"Democratic Republic of Congo",	 "Congo",	"Costa Rica",	"Cote dIvoire",	"Croatia",	"Cuba",	 "Curaco",	 "Cyprus",	"Czech Republic",	"Denmark",	"Djibouti",	"Dominica",	 "Dominican Republic",	"Ecuador",	 "Egypt",	"El Salvador",	 "Equatorial Guinea",	"Eritrea",	"Estonia",	"Ethiopia",	 "Faroe Islands",	"Fiji",	"Finland",	 "France",	 "French Polynesia",	 "Gabon",	"The Gambia",	"Georgia",	"Germany",	 "Ghana",	 "Gibraltar",	 "Greece",	 "Greenland",	"Grenada",	"Guam",	"Guatemala",	 "Guinea",	"Guinea Bissau",	 "Guyana",	 "Haiti",	 "Honduras",	"Hong Kong",	"Hungary",	"Iceland",	 "India",	"Indonesia",	"Iran",	"Iraq",	"Ireland",	"Isle of Man",	 "Israel",	 "Italy",	"Jamaica",	 "Japan",	 "Jordan",	"Kazakhstan",	 "Kenya",	"Kiribati",	"North Korea",	 "South Korea",	 "Kosovo",	 "Kuwait",	 "Kyrgyz Republic",	"Laos",	 "Latvia",	"Lebanon",	"Lesotho",	"Liberia",	 "Libya",	"Liechtenstein",	 "Lithuania",	"Luxembourg",	 "Macao",	 "Macedonia",	"Madagascar",	 "Malawi",	"Malaysia",	"Maldives",	"Mali",	 "Malta",	 "Marshall Islands",	"Mauritania",	 "Mauritius",	 "Mexico",	"Micronesia",	"Moldova",	 "Monaco",	"Mongolia",	"Montenegro",	"Morocco",	"Mozambique",	"Myanmar",	"Namibia",	 "Nauru",	 "Nepal",	"Netherlands",	"New Caledonia",	 "New Zealand",	 "Nicaragua",	 "Niger",	"Nigeria",	"Northern Mariana Islands",	 "Norway",	"Oman",	"Pakistan",	 "Palau",	 "Panama",	 "Papua New Guinea",	"Paraguay",	"Peru",	"Philippines",	 "Poland",	"Portugal",	"Puerto Rico",	 "Qatar",	"Romania",	"Russian Federation",	 "Rwanda",	 "Samoa",	"San Marino",	 "Sao Tome and Principe",	 "Saudi Arabia",	"Senegal",	 "Serbia",	 "Seychelles",	 "Sierra Leone",	 "Singapore",	 "Sint Maarten Dutchpart",	"Slovak Republic",	"Slovenia",	"Solomon Islands",	"Somalia",	"South Africa",	"South Sudan",	"Spain",	"Sri Lanka",	"St Kitts and Nevis",	"St Lucia",	"St Martin",	"St Vincent and the Grenadines",	"Sudan",	"Suriname",	"Swaziland",	 "Sweden",	"Switzerland",	"Syria",	"Tajikistan",	"Tanzania",	"Thailand",	 "Timor Leste",	 "Togo",	"Tonga",	"Trinidad and Tobago",	"Tunisia",	 "Turkey",	"Turkmenistan",	 "Turks and Caicos",	 "Tuvalu",	 "Uganda",	"Ukraine",	"United Arab Emirates",	 "United Kingdom",	"United States",	"Uruguay",	 "Uzbekistan",	"Vanuatu",	"Venezuela",	"Vietnam",	"Virgin Islands US",	"Virgin Islands US",	 "Yemen",	 "Zambia",	"Zimbabwe",	 "Taiwan",	"Cook Islands",	"Niue",	 "Wallis and Futuna",	"NA"), server = TRUE)
    updateSelectizeInput(session, "ratios_gender", choices = c('',"Both genders", "Female", "Male", "Gender not specified"), server = TRUE)
    updateSelectizeInput(session, "ratios_age", choices = c('',"0-18 years","19-40 years","41-64 years","65+ years","Adult (unspecified)","Age unknown"), server = TRUE)
    updateSelectizeInput(session, "ratios_int", choices = c('', "Care Delivery",	"Diagnostic",	"Health Education or Behavior",	"Immunization",	"Medical Device",	"Medical Procedure",	"Pharmaceutical",	"Screening",	"Surgical",	"Other",	"None",	"Multiple Intervention Types"), server = TRUE)
    updateSliderInput(session, 'ratios_year', min = 1976, max = 2018, value = c(1976, 2018), step = 1)
    updateSelectizeInput(session, "ratios_quadrant", choices = c('', "Health gain/More cost", "Health loss/More cost", "Health gain/Less cost", "Health loss/Less cost"), server = TRUE)
  })
  
  #update selectize inputs - weights
  updateSelectizeInput(session, "weights_keywords", choices = c('', keywords$Keywords), server = TRUE)
  updateSelectizeInput(session, "weights_ArticleID", choices = c('',weightsdf$ArticleID), server = TRUE)
  updateSelectizeInput(session, "weights_healthstate", choices = c('',weightsdf$HealthState), server = TRUE)
  updateSelectizeInput(session, "weights_source", choices = c('',"Primary source","Secondary source","Both primary and secondary","Source not collected"), server = TRUE)
  updateSelectizeInput(session, "weights_disease", choices = c('',disease_choices), server = TRUE)
  
  #reset filters when reset button is hit
  observeEvent(input$weights_reset,{
    updateSelectizeInput(session, "weights_keywords", choices = c('', keywords$Keywords), server = TRUE)
    updateSelectizeInput(session, "weights_ArticleID", choices = c('',weightsdf$ArticleID), server = TRUE)
    updateSelectizeInput(session, "weights_healthstate", choices = c('',weightsdf$HealthState), server = TRUE)
    updateSliderInput(session, "weights_utilityweight", min = -1, max = 1, value = c(-1, 1), step = .01)
    updateSelectizeInput(session, "weights_source", choices = c('',"Primary source","Secondary source","Both primary and secondary","Source not collected"), server = TRUE)
    updateSliderInput(session, 'weights_year', min = 1976, max = 2018, value = c(1976, 2018), step = 1)
    updateSelectizeInput(session, "weights_disease", choices = c('',disease_choices), server = TRUE)
  })
  
  #toggle filtered ArticleIDs filters when showIDs filter buttons clicked
  #methods
  observeEvent(input$methods_showIDs,{
    shinyjs::toggleElement('methods_ArticleID')
  })
  #ratios
  observeEvent(input$ratios_showIDs,{
    shinyjs::toggleElement('ratios_ArticleID')
  })
  #weights
  observeEvent(input$weights_showIDs,{
    shinyjs::toggleElement('weights_ArticleID')
  })
  
  #currently filtered text
  #methods
  output$methods_filtered<-renderText({paste('<center>','<font size="3">', '<b>','Limited subset: showing ', nrow(methods_table()), " of ", nrow(methodsfull), " total articles.")})
  observe(
    if(!is.null(input$methods_ArticleID) | !is.null(input$methods_keywords) | !is.null(input$methods_country) | (input$methods_year[1]>1976 | input$methods_year[2]<2017)
       | !is.null(input$methods_author) | !is.null(input$methods_inttype) | !is.null(input$methods_prev) | !is.null(input$methods_icd10)
       | !is.null(input$methods_perspective) | (input$methods_rating[1]>0 | input$methods_rating[2]<7))
    {
      shinyjs::showElement('methods_filtered')
    }
    else if(is.null(input$methods_ArticleID) & is.null(input$methods_keywords) & is.null(input$methods_country) & (input$methods_year[1]<=1976 | input$methods_year[2]>=2017)
            & is.null(input$methods_author) & is.null(input$methods_inttype) & is.null(input$methods_prev) & is.null(input$methods_icd10)
            & is.null(input$methods_perspective) & (input$methods_rating[1]<=0 | input$methods_rating[2]>=7))
    {
      shinyjs::hideElement('methods_filtered')
    }
  )  
  #ratios
  output$ratios_filtered<-renderText({paste('<center>','<font size="3">', '<b>','Limited subset: showing ', nrow(ratios_table()), " of ", nrow(ratiosfull), " total ratios.")})
  observe(
    if(!is.null(input$ratios_ArticleID) | !is.null(input$ratios_keywords) | !is.null(input$ratios_country) | !is.null(input$ratios_gender) | !is.null(input$ratios_age) |
       !is.null(input$ratios_int) | (input$ratios_year[1]>1976 | input$ratios_year[2]<2017)| !is.null(input$ratios_quadrant))
    {
      shinyjs::showElement('ratios_filtered')
    }
    else if(is.null(input$ratios_ArticleID) & is.null(input$ratios_keywords) & is.null(input$ratios_country) & is.null(input$ratios_gender) & is.null(input$ratios_age) &
            is.null(input$ratios_int) & (input$ratios_year[1]<=1976 & input$ratios_year[2]>=2017)& is.null(input$ratios_quadrant))
    {
      shinyjs::hideElement('ratios_filtered')
    }
  )  
  #weights
  output$weights_filtered<-renderText({paste('<center>','<font size="3">', '<b>','Limited subset: showing ', nrow(weights_table()), " of ", nrow(weightsfull), " total weights.")})
  observe(
    if(!is.null(input$weights_ArticleID) | !is.null(input$weights_keywords) | !is.null(input$weights_healthstate) | (input$weights_utilityweight[1]>-1 | input$weights_utilityweight[2]<1) |
       !is.null(input$weights_source) | (input$weights_year[1]>1976 | input$weights_year[2]<2017)| !is.null(input$weights_disease))
    {
      shinyjs::showElement('weights_filtered')
    }
    else if(is.null(input$weights_ArticleID) & is.null(input$weights_keywords) & is.null(input$weights_healthstate) & (input$weights_utilityweight[1]<=-1 | input$weights_utilityweight[2]>=1) &
            is.null(input$weights_source) & (input$weights_year[1]<=1976 & input$weights_year[2]>=2017)& is.null(input$weights_disease))
    {
      shinyjs::hideElement('weights_filtered')
    }
  )  
  
  #reactive articleID
  rArticleID <- reactiveValues(ArticleID = '')
  
  ####METHODS####
  #methods dataframe, reactive to ArticleID filter
  methods_table<-reactive({
    
    methods_subset<-methodsdf
    
    if(!is.null(input$methods_ArticleID)){
      methods_subset<-subset(methods_subset, methods_subset$ArticleID %in% input$methods_ArticleID)
    }
    
    if(!is.null(input$methods_keywords)){
      keywords_subset_methods<-subset(keywords, keywords$Keywords %in% input$methods_keywords)
      methods_subset<-subset(methods_subset, methods_subset$ArticleID %in% keywords_subset_methods$ArticleID)
    }
    
    if(!is.null(input$methods_country)){
      methods_subset<-subset(methods_subset, Country_Afghanistan %in% input$methods_country | 	Country_Albania %in% input$methods_country 
                             | 	Country_Algeria %in% input$methods_country | 	Country_AmericanSamoa %in% input$methods_country 
                             | 	Country_Andorra %in% input$methods_country | 	Country_Angola %in% input$methods_country 
                             | 	Country_AntiguaandBarbuda %in% input$methods_country | 	Country_Argentina %in% input$methods_country 
                             | 	Country_Armenia %in% input$methods_country | 	Country_Aruba %in% input$methods_country 
                             | 	Country_Australia %in% input$methods_country | 	Country_Austria %in% input$methods_country 
                             | 	Country_Azerbaijan %in% input$methods_country | 	Country_Bahamas %in% input$methods_country 
                             | 	Country_Bahrain %in% input$methods_country | 	Country_Bangladesh %in% input$methods_country 
                             | 	Country_Barbados %in% input$methods_country | 	Country_Belarus %in% input$methods_country 
                             | 	Country_Belgium %in% input$methods_country | 	Country_Belize %in% input$methods_country 
                             | 	Country_Benin %in% input$methods_country | 	Country_Bermuda %in% input$methods_country 
                             | 	Country_Bhutan %in% input$methods_country | 	Country_Bolivia %in% input$methods_country 
                             | 	Country_BosniaandHerzegovina %in% input$methods_country | 	Country_Botswana %in% input$methods_country 
                             | 	Country_Brazil %in% input$methods_country | 	Country_BritishVirginIslands %in% input$methods_country 
                             | 	Country_Brunei %in% input$methods_country | 	Country_Bulgaria %in% input$methods_country 
                             | 	Country_BurkinaFaso %in% input$methods_country | 	Country_Burundi %in% input$methods_country 
                             | 	Country_CaboVerde %in% input$methods_country | 	Country_Cambodia %in% input$methods_country 
                             | 	Country_Cameroon %in% input$methods_country | 	Country_Canada %in% input$methods_country 
                             | 	Country_CaymanIslands %in% input$methods_country | 	Country_CentralAfricanRepublic %in% input$methods_country 
                             | 	Country_Chad %in% input$methods_country | 	Country_ChannelIslands %in% input$methods_country 
                             | 	Country_Chile %in% input$methods_country | 	Country_China %in% input$methods_country 
                             | 	Country_Colombia %in% input$methods_country | 	Country_Comoros %in% input$methods_country 
                             | 	Country_CongoDemRep %in% input$methods_country | 	Country_CongoRep %in% input$methods_country 
                             | 	Country_CostaRica %in% input$methods_country | 	Country_CotedIvoire %in% input$methods_country 
                             | 	Country_Croatia %in% input$methods_country | 	Country_Cuba %in% input$methods_country 
                             | 	Country_Curaco %in% input$methods_country | 	Country_Cyprus %in% input$methods_country 
                             | 	Country_CzechRepublic %in% input$methods_country | 	Country_Denmark %in% input$methods_country 
                             | 	Country_Djibouti %in% input$methods_country | 	Country_Dominica %in% input$methods_country 
                             | 	Country_DominicanRepublic %in% input$methods_country | 	Country_Ecuador %in% input$methods_country 
                             | 	Country_EgyptArabRep %in% input$methods_country | 	Country_ElSalvador %in% input$methods_country 
                             | 	Country_EquatorialGuinea %in% input$methods_country | 	Country_Eritrea %in% input$methods_country 
                             | 	Country_Estonia %in% input$methods_country | 	Country_Ethiopia %in% input$methods_country 
                             | 	Country_FaroeIslands %in% input$methods_country | 	Country_Fiji %in% input$methods_country 
                             | 	Country_Finland %in% input$methods_country | 	Country_France %in% input$methods_country 
                             | 	Country_FrenchPolynesia %in% input$methods_country | 	Country_Gabon %in% input$methods_country 
                             | 	Country_GambiaThe %in% input$methods_country | 	Country_Georgia %in% input$methods_country 
                             | 	Country_Germany %in% input$methods_country | 	Country_Ghana %in% input$methods_country 
                             | 	Country_Gibraltar %in% input$methods_country | 	Country_Greece %in% input$methods_country 
                             | 	Country_Greenland %in% input$methods_country | 	Country_Grenada %in% input$methods_country 
                             | 	Country_Guam %in% input$methods_country | 	Country_Guatemala %in% input$methods_country 
                             | 	Country_Guinea %in% input$methods_country | 	Country_GuineaBissau %in% input$methods_country 
                             | 	Country_Guyana %in% input$methods_country | 	Country_Haiti %in% input$methods_country 
                             | 	Country_Honduras %in% input$methods_country | 	Country_HongKongSARChina %in% input$methods_country 
                             | 	Country_Hungary %in% input$methods_country | 	Country_Iceland %in% input$methods_country 
                             | 	Country_India %in% input$methods_country | 	Country_Indonesia %in% input$methods_country 
                             | 	Country_IranIslamicRep %in% input$methods_country | 	Country_Iraq %in% input$methods_country 
                             | 	Country_Ireland %in% input$methods_country | 	Country_IsleofMan %in% input$methods_country 
                             | 	Country_Israel %in% input$methods_country | 	Country_Italy %in% input$methods_country 
                             | 	Country_Jamaica %in% input$methods_country | 	Country_Japan %in% input$methods_country 
                             | 	Country_Jordan %in% input$methods_country | 	Country_Kazakhstan %in% input$methods_country 
                             | 	Country_Kenya %in% input$methods_country | 	Country_Kiribati %in% input$methods_country 
                             | 	Country_KoreaDemPeoplesRep %in% input$methods_country | 	Country_KoreaRep %in% input$methods_country
                             | 	Country_Kosovo %in% input$methods_country | 	Country_Kuwait %in% input$methods_country 
                             | 	Country_KyrgyzRepublic %in% input$methods_country | 	Country_LaoPDR %in% input$methods_country 
                             | 	Country_Latvia %in% input$methods_country | 	Country_Lebanon %in% input$methods_country 
                             | 	Country_Lesotho %in% input$methods_country | 	Country_Liberia %in% input$methods_country 
                             | 	Country_Libya %in% input$methods_country | 	Country_Liechtenstein %in% input$methods_country
                             | 	Country_Lithuania %in% input$methods_country | 	Country_Luxembourg %in% input$methods_country 
                             | 	Country_MacaoSARChina %in% input$methods_country | 	Country_MacedoniaFYR %in% input$methods_country
                             | 	Country_Madagascar %in% input$methods_country | 	Country_Malawi %in% input$methods_country 
                             | 	Country_Malaysia %in% input$methods_country | 	Country_Maldives %in% input$methods_country 
                             | 	Country_Mali %in% input$methods_country | 	Country_Malta %in% input$methods_country 
                             | 	Country_MarshallIslands %in% input$methods_country | 	Country_Mauritania %in% input$methods_country 
                             | 	Country_Mauritius %in% input$methods_country | 	Country_Mexico %in% input$methods_country 
                             | 	Country_MicronesiaFedSts %in% input$methods_country | 	Country_Moldova %in% input$methods_country 
                             | 	Country_Monaco %in% input$methods_country | 	Country_Mongolia %in% input$methods_country 
                             | 	Country_Montenegro %in% input$methods_country | 	Country_Morocco %in% input$methods_country 
                             | 	Country_Mozambique %in% input$methods_country | 	Country_Myanmar %in% input$methods_country 
                             | 	Country_Namibia %in% input$methods_country | 	Country_Nauru %in% input$methods_country 
                             | 	Country_Nepal %in% input$methods_country | 	Country_Netherlands %in% input$methods_country 
                             | 	Country_NewCaledonia %in% input$methods_country | 	Country_NewZealand %in% input$methods_country 
                             | 	Country_Nicaragua %in% input$methods_country | 	Country_Niger %in% input$methods_country 
                             | 	Country_Nigeria %in% input$methods_country | 	Country_NorthernMarianaIslands %in% input$methods_country 
                             | 	Country_Norway %in% input$methods_country | 	Country_Oman %in% input$methods_country 
                             | 	Country_Pakistan %in% input$methods_country | 	Country_Palau %in% input$methods_country 
                             | 	Country_Panama %in% input$methods_country | 	Country_PapuaNewGuinea %in% input$methods_country 
                             | 	Country_Paraguay %in% input$methods_country | 	Country_Peru %in% input$methods_country 
                             | 	Country_Philippines %in% input$methods_country | 	Country_Poland %in% input$methods_country 
                             | 	Country_Portugal %in% input$methods_country | 	Country_PuertoRico %in% input$methods_country 
                             | 	Country_Qatar %in% input$methods_country | 	Country_Romania %in% input$methods_country 
                             | 	Country_RussianFederation %in% input$methods_country | 	Country_Rwanda %in% input$methods_country 
                             | 	Country_Samoa %in% input$methods_country | 	Country_SanMarino %in% input$methods_country 
                             | 	Country_SaoTomeandPrincipe %in% input$methods_country | 	Country_SaudiArabia %in% input$methods_country 
                             | 	Country_Senegal %in% input$methods_country | 	Country_Serbia %in% input$methods_country 
                             | 	Country_Seychelles %in% input$methods_country | 	Country_SierraLeone %in% input$methods_country 
                             | 	Country_Singapore %in% input$methods_country | 	Country_SintMaartenDutchpart %in% input$methods_country
                             | 	Country_SlovakRepublic %in% input$methods_country | 	Country_Slovenia %in% input$methods_country 
                             | 	Country_SolomonIslands %in% input$methods_country | 	Country_Somalia %in% input$methods_country
                             | 	Country_SouthAfrica %in% input$methods_country | 	Country_SouthSudan %in% input$methods_country 
                             | 	Country_Spain %in% input$methods_country | 	Country_SriLanka %in% input$methods_country 
                             | 	Country_StKittsandNevis %in% input$methods_country | 	Country_StLucia %in% input$methods_country
                             | 	Country_StMartinFrenchpart %in% input$methods_country | 	Country_StVincentandtheGrenadine %in% input$methods_country 
                             | 	Country_Sudan %in% input$methods_country | 	Country_Suriname %in% input$methods_country
                             | 	Country_Swaziland %in% input$methods_country | 	Country_Sweden %in% input$methods_country
                             | 	Country_Switzerland %in% input$methods_country | 	Country_SyrianArabRepublic %in% input$methods_country 
                             | 	Country_Tajikistan %in% input$methods_country | 	Country_Tanzania %in% input$methods_country 
                             | 	Country_Thailand %in% input$methods_country | 	Country_TimorLeste %in% input$methods_country 
                             | 	Country_Togo %in% input$methods_country | 	Country_Tonga %in% input$methods_country 
                             | 	Country_TrinidadandTobago %in% input$methods_country | 	Country_Tunisia %in% input$methods_country 
                             | 	Country_Turkey %in% input$methods_country | 	Country_Turkmenistan %in% input$methods_country 
                             | 	Country_TurksandCaicosIslands %in% input$methods_country | 	Country_Tuvalu %in% input$methods_country 
                             | 	Country_Uganda %in% input$methods_country | 	Country_Ukraine %in% input$methods_country 
                             | 	Country_UnitedArabEmirates %in% input$methods_country | 	Country_UnitedKingdom %in% input$methods_country 
                             | 	Country_UnitedStates %in% input$methods_country | 	Country_Uruguay %in% input$methods_country 
                             | 	Country_Uzbekistan %in% input$methods_country | 	Country_Vanuatu %in% input$methods_country 
                             | 	Country_VenezuelaRB %in% input$methods_country | 	Country_Vietnam %in% input$methods_country 
                             | 	Country_VirginIslandsUS %in% input$methods_country | 	Country_WestBankandGaza %in% input$methods_country 
                             | 	Country_YemenRep %in% input$methods_country | 	Country_Zambia %in% input$methods_country 
                             | 	Country_Zimbabwe %in% input$methods_country | 	Country_TaiwanChina %in% input$methods_country 
                             | 	Country_CookIslands %in% input$methods_country | 	Country_Niue %in% input$methods_country 
                             | 	Country_WallisandFutuna %in% input$methods_country | 	Country_NA %in% input$methods_country)
    }
    
    if(!is.null(input$methods_year)){
      methods_subset<-subset(methods_subset, (methods_subset$PubYear >= input$methods_year[1])&(methods_subset$PubYear <= input$methods_year[2]))
    }
    
    if(!is.null(input$methods_author)){
      methods_subset<-subset(methods_subset, methods_subset$PrimAuthLastName %in% input$methods_author)
    }
    
    if(!is.null(input$methods_inttype)){
      methods_subset<-subset(methods_subset, methods_subset$InterventionCareDelivery %in% input$methods_inttype| methods_subset$InterventionDiagnostic %in% input$methods_inttype|
                               methods_subset$InterventionHealthEducationOrBe %in% input$methods_inttype| methods_subset$InterventionImmunization %in% input$methods_inttype|
                               methods_subset$InterventionMedicalDevice %in% input$methods_inttype|  methods_subset$InterventionMedicalProcedure %in% input$methods_inttype|
                               methods_subset$InterventionPharmaceutical %in% input$methods_inttype| methods_subset$InterventionScreening %in% input$methods_inttype|
                               methods_subset$InterventionScreening %in% input$methods_inttype| methods_subset$InterventionSurgical %in% input$methods_inttype|
                               methods_subset$InterventionOther %in% input$methods_inttype| methods_subset$InterventionNoneNA %in% input$methods_inttype)
    }
    
    if(!is.null(input$methods_prev)){
      methods_subset<-subset(methods_subset, methods_subset$PreventionStage1 %in% input$methods_prev|methods_subset$PreventionStage2 %in% input$methods_prev|
                               methods_subset$PreventionStage3 %in% input$methods_prev)
    }
    
    if(!is.null(input$methods_icd10)){
      methods_subset<-subset(methods_subset, methods_subset$DisChapter1 %in% input$methods_icd10| methods_subset$DisCode1 %in% input$methods_icd10)
    }
    
    if(!is.null(input$methods_perspective)){
      methods_subset<-subset(methods_subset, methods_subset$ReaderPerspective %in% input$methods_perspective)
    }
    
    if(!is.null(input$methods_rating)){
      methods_subset<-subset(methods_subset, (methods_subset$Rating >= input$methods_rating[1])&(methods_subset$Rating <= input$methods_rating[2]))
    }
    
    #order by descending articleID
    methods_subset<-methods_subset[order(methods_subset$ArticleID, decreasing = TRUE), ]
    
    #limit to 100
    if(nrow(methods_subset)>100) methods_subset<-methods_subset[1:100,]
    
    return_table<-data.frame(
      methods_subset$ArticleID,
      methods_subset$citation,
      methods_subset$display_title,
      methods_subset$Country_display,
      ifelse(is.na(methods_subset$DisCode1),methods_subset$DisChapter1,methods_subset$DisCode1),
      methods_subset$PreventionStage_string,
      methods_subset$intervention_string,
      methods_subset$ReaderPerspective,
      methods_subset$Rating
    )
    
    colnames(return_table)<-c("ArticleID", "Citation","Article Title", "Country", "Disease (ICD10)","Prevention Stage", "Intervention Type", 
                              "Perspective", "Quality Rating")
    
    return(return_table)  
    
  })
  
  #output methods dataframe
  output$methods_data <- DT::renderDataTable(
    methods_table(),
    server = TRUE, escape = FALSE, rownames = FALSE,
    options = list(
      #disable search
      searching = FALSE,
      #default to show 10 per page
      pageLength = 10
    )
  )
  
  ####RATIOS####
  #ratios dataframe, subset per ArticleID filter
  ratios_table<-reactive({
    
    ratios_subset<-ratiosdf
    
    if(!is.null(input$ratios_keywords)){
      keywords_subset_ratios<-subset(keywords, keywords$Keywords %in% input$ratios_keywords)
      ratios_subset<-subset(ratios_subset, ratios_subset$ArticleID %in% keywords_subset_ratios$ArticleID)
    }
    
    if(!is.null(input$ratios_ArticleID)){
      ratios_subset<-subset(ratios_subset, ratios_subset$ArticleID %in% input$ratios_ArticleID)
    }
    
    if(!is.null(input$ratios_country)){
      ratios_subset<-subset(ratios_subset, Country_Afghanistan %in% input$ratios_country | 	Country_Albania %in% input$ratios_country 
                            | 	Country_Algeria %in% input$ratios_country | 	Country_AmericanSamoa %in% input$ratios_country 
                            | 	Country_Andorra %in% input$ratios_country | 	Country_Angola %in% input$ratios_country 
                            | 	Country_AntiguaandBarbuda %in% input$ratios_country | 	Country_Argentina %in% input$ratios_country 
                            | 	Country_Armenia %in% input$ratios_country | 	Country_Aruba %in% input$ratios_country 
                            | 	Country_Australia %in% input$ratios_country | 	Country_Austria %in% input$ratios_country 
                            | 	Country_Azerbaijan %in% input$ratios_country | 	Country_Bahamas %in% input$ratios_country 
                            | 	Country_Bahrain %in% input$ratios_country | 	Country_Bangladesh %in% input$ratios_country 
                            | 	Country_Barbados %in% input$ratios_country | 	Country_Belarus %in% input$ratios_country 
                            | 	Country_Belgium %in% input$ratios_country | 	Country_Belize %in% input$ratios_country 
                            | 	Country_Benin %in% input$ratios_country | 	Country_Bermuda %in% input$ratios_country 
                            | 	Country_Bhutan %in% input$ratios_country | 	Country_Bolivia %in% input$ratios_country 
                            | 	Country_BosniaandHerzegovina %in% input$ratios_country | 	Country_Botswana %in% input$ratios_country 
                            | 	Country_Brazil %in% input$ratios_country | 	Country_BritishVirginIslands %in% input$ratios_country 
                            | 	Country_Brunei %in% input$ratios_country | 	Country_Bulgaria %in% input$ratios_country 
                            | 	Country_BurkinaFaso %in% input$ratios_country | 	Country_Burundi %in% input$ratios_country 
                            | 	Country_CaboVerde %in% input$ratios_country | 	Country_Cambodia %in% input$ratios_country 
                            | 	Country_Cameroon %in% input$ratios_country | 	Country_Canada %in% input$ratios_country 
                            | 	Country_CaymanIslands %in% input$ratios_country | 	Country_CentralAfricanRepublic %in% input$ratios_country 
                            | 	Country_Chad %in% input$ratios_country | 	Country_ChannelIslands %in% input$ratios_country 
                            | 	Country_Chile %in% input$ratios_country | 	Country_China %in% input$ratios_country 
                            | 	Country_Colombia %in% input$ratios_country | 	Country_Comoros %in% input$ratios_country 
                            | 	Country_CongoDemRep %in% input$ratios_country | 	Country_CongoRep %in% input$ratios_country 
                            | 	Country_CostaRica %in% input$ratios_country | 	Country_CotedIvoire %in% input$ratios_country 
                            | 	Country_Croatia %in% input$ratios_country | 	Country_Cuba %in% input$ratios_country 
                            | 	Country_Curaco %in% input$ratios_country | 	Country_Cyprus %in% input$ratios_country 
                            | 	Country_CzechRepublic %in% input$ratios_country | 	Country_Denmark %in% input$ratios_country 
                            | 	Country_Djibouti %in% input$ratios_country | 	Country_Dominica %in% input$ratios_country 
                            | 	Country_DominicanRepublic %in% input$ratios_country | 	Country_Ecuador %in% input$ratios_country 
                            | 	Country_EgyptArabRep %in% input$ratios_country | 	Country_ElSalvador %in% input$ratios_country 
                            | 	Country_EquatorialGuinea %in% input$ratios_country | 	Country_Eritrea %in% input$ratios_country 
                            | 	Country_Estonia %in% input$ratios_country | 	Country_Ethiopia %in% input$ratios_country 
                            | 	Country_FaroeIslands %in% input$ratios_country | 	Country_Fiji %in% input$ratios_country 
                            | 	Country_Finland %in% input$ratios_country | 	Country_France %in% input$ratios_country 
                            | 	Country_FrenchPolynesia %in% input$ratios_country | 	Country_Gabon %in% input$ratios_country 
                            | 	Country_GambiaThe %in% input$ratios_country | 	Country_Georgia %in% input$ratios_country 
                            | 	Country_Germany %in% input$ratios_country | 	Country_Ghana %in% input$ratios_country 
                            | 	Country_Gibraltar %in% input$ratios_country | 	Country_Greece %in% input$ratios_country 
                            | 	Country_Greenland %in% input$ratios_country | 	Country_Grenada %in% input$ratios_country 
                            | 	Country_Guam %in% input$ratios_country | 	Country_Guatemala %in% input$ratios_country 
                            | 	Country_Guinea %in% input$ratios_country | 	Country_GuineaBissau %in% input$ratios_country 
                            | 	Country_Guyana %in% input$ratios_country | 	Country_Haiti %in% input$ratios_country 
                            | 	Country_Honduras %in% input$ratios_country | 	Country_HongKongSARChina %in% input$ratios_country 
                            | 	Country_Hungary %in% input$ratios_country | 	Country_Iceland %in% input$ratios_country 
                            | 	Country_India %in% input$ratios_country | 	Country_Indonesia %in% input$ratios_country 
                            | 	Country_IranIslamicRep %in% input$ratios_country | 	Country_Iraq %in% input$ratios_country 
                            | 	Country_Ireland %in% input$ratios_country | 	Country_IsleofMan %in% input$ratios_country 
                            | 	Country_Israel %in% input$ratios_country | 	Country_Italy %in% input$ratios_country 
                            | 	Country_Jamaica %in% input$ratios_country | 	Country_Japan %in% input$ratios_country 
                            | 	Country_Jordan %in% input$ratios_country | 	Country_Kazakhstan %in% input$ratios_country 
                            | 	Country_Kenya %in% input$ratios_country | 	Country_Kiribati %in% input$ratios_country 
                            | 	Country_KoreaDemPeoplesRep %in% input$ratios_country | 	Country_KoreaRep %in% input$ratios_country
                            | 	Country_Kosovo %in% input$ratios_country | 	Country_Kuwait %in% input$ratios_country 
                            | 	Country_KyrgyzRepublic %in% input$ratios_country | 	Country_LaoPDR %in% input$ratios_country 
                            | 	Country_Latvia %in% input$ratios_country | 	Country_Lebanon %in% input$ratios_country 
                            | 	Country_Lesotho %in% input$ratios_country | 	Country_Liberia %in% input$ratios_country 
                            | 	Country_Libya %in% input$ratios_country | 	Country_Liechtenstein %in% input$ratios_country
                            | 	Country_Lithuania %in% input$ratios_country | 	Country_Luxembourg %in% input$ratios_country 
                            | 	Country_MacaoSARChina %in% input$ratios_country | 	Country_MacedoniaFYR %in% input$ratios_country
                            | 	Country_Madagascar %in% input$ratios_country | 	Country_Malawi %in% input$ratios_country 
                            | 	Country_Malaysia %in% input$ratios_country | 	Country_Maldives %in% input$ratios_country 
                            | 	Country_Mali %in% input$ratios_country | 	Country_Malta %in% input$ratios_country 
                            | 	Country_MarshallIslands %in% input$ratios_country | 	Country_Mauritania %in% input$ratios_country 
                            | 	Country_Mauritius %in% input$ratios_country | 	Country_Mexico %in% input$ratios_country 
                            | 	Country_MicronesiaFedSts %in% input$ratios_country | 	Country_Moldova %in% input$ratios_country 
                            | 	Country_Monaco %in% input$ratios_country | 	Country_Mongolia %in% input$ratios_country 
                            | 	Country_Montenegro %in% input$ratios_country | 	Country_Morocco %in% input$ratios_country 
                            | 	Country_Mozambique %in% input$ratios_country | 	Country_Myanmar %in% input$ratios_country 
                            | 	Country_Namibia %in% input$ratios_country | 	Country_Nauru %in% input$ratios_country 
                            | 	Country_Nepal %in% input$ratios_country | 	Country_Netherlands %in% input$ratios_country 
                            | 	Country_NewCaledonia %in% input$ratios_country | 	Country_NewZealand %in% input$ratios_country 
                            | 	Country_Nicaragua %in% input$ratios_country | 	Country_Niger %in% input$ratios_country 
                            | 	Country_Nigeria %in% input$ratios_country | 	Country_NorthernMarianaIslands %in% input$ratios_country 
                            | 	Country_Norway %in% input$ratios_country | 	Country_Oman %in% input$ratios_country 
                            | 	Country_Pakistan %in% input$ratios_country | 	Country_Palau %in% input$ratios_country 
                            | 	Country_Panama %in% input$ratios_country | 	Country_PapuaNewGuinea %in% input$ratios_country 
                            | 	Country_Paraguay %in% input$ratios_country | 	Country_Peru %in% input$ratios_country 
                            | 	Country_Philippines %in% input$ratios_country | 	Country_Poland %in% input$ratios_country 
                            | 	Country_Portugal %in% input$ratios_country | 	Country_PuertoRico %in% input$ratios_country 
                            | 	Country_Qatar %in% input$ratios_country | 	Country_Romania %in% input$ratios_country 
                            | 	Country_RussianFederation %in% input$ratios_country | 	Country_Rwanda %in% input$ratios_country 
                            | 	Country_Samoa %in% input$ratios_country | 	Country_SanMarino %in% input$ratios_country 
                            | 	Country_SaoTomeandPrincipe %in% input$ratios_country | 	Country_SaudiArabia %in% input$ratios_country 
                            | 	Country_Senegal %in% input$ratios_country | 	Country_Serbia %in% input$ratios_country 
                            | 	Country_Seychelles %in% input$ratios_country | 	Country_SierraLeone %in% input$ratios_country 
                            | 	Country_Singapore %in% input$ratios_country | 	Country_SintMaartenDutchpart %in% input$ratios_country
                            | 	Country_SlovakRepublic %in% input$ratios_country | 	Country_Slovenia %in% input$ratios_country 
                            | 	Country_SolomonIslands %in% input$ratios_country | 	Country_Somalia %in% input$ratios_country
                            | 	Country_SouthAfrica %in% input$ratios_country | 	Country_SouthSudan %in% input$ratios_country 
                            | 	Country_Spain %in% input$ratios_country | 	Country_SriLanka %in% input$ratios_country 
                            | 	Country_StKittsandNevis %in% input$ratios_country | 	Country_StLucia %in% input$ratios_country
                            | 	Country_StMartinFrenchpart %in% input$ratios_country | 	Country_StVincentandtheGrenadine %in% input$ratios_country 
                            | 	Country_Sudan %in% input$ratios_country | 	Country_Suriname %in% input$ratios_country
                            | 	Country_Swaziland %in% input$ratios_country | 	Country_Sweden %in% input$ratios_country
                            | 	Country_Switzerland %in% input$ratios_country | 	Country_SyrianArabRepublic %in% input$ratios_country 
                            | 	Country_Tajikistan %in% input$ratios_country | 	Country_Tanzania %in% input$ratios_country 
                            | 	Country_Thailand %in% input$ratios_country | 	Country_TimorLeste %in% input$ratios_country 
                            | 	Country_Togo %in% input$ratios_country | 	Country_Tonga %in% input$ratios_country 
                            | 	Country_TrinidadandTobago %in% input$ratios_country | 	Country_Tunisia %in% input$ratios_country 
                            | 	Country_Turkey %in% input$ratios_country | 	Country_Turkmenistan %in% input$ratios_country 
                            | 	Country_TurksandCaicosIslands %in% input$ratios_country | 	Country_Tuvalu %in% input$ratios_country 
                            | 	Country_Uganda %in% input$ratios_country | 	Country_Ukraine %in% input$ratios_country 
                            | 	Country_UnitedArabEmirates %in% input$ratios_country | 	Country_UnitedKingdom %in% input$ratios_country 
                            | 	Country_UnitedStates %in% input$ratios_country | 	Country_Uruguay %in% input$ratios_country 
                            | 	Country_Uzbekistan %in% input$ratios_country | 	Country_Vanuatu %in% input$ratios_country 
                            | 	Country_VenezuelaRB %in% input$ratios_country | 	Country_Vietnam %in% input$ratios_country 
                            | 	Country_VirginIslandsUS %in% input$ratios_country | 	Country_WestBankandGaza %in% input$ratios_country 
                            | 	Country_YemenRep %in% input$ratios_country | 	Country_Zambia %in% input$ratios_country 
                            | 	Country_Zimbabwe %in% input$ratios_country | 	Country_TaiwanChina %in% input$ratios_country 
                            | 	Country_CookIslands %in% input$ratios_country | 	Country_Niue %in% input$ratios_country 
                            | 	Country_WallisandFutuna %in% input$ratios_country | 	Country_NA %in% input$ratios_country)
    }
    
    if(!is.null(input$ratios_gender)){
      ratios_subset<-subset(ratios_subset, ratios_subset$display_gender %in% input$ratios_gender)
    }
    
    if(!is.null(input$ratios_age)){
      ratios_subset<-subset(ratios_subset, ratios_subset$TargetPopulationAge_018 %in% input$ratios_age |
                              ratios_subset$TargetPopulationAge_1940 %in% input$ratios_age |
                              ratios_subset$TargetPopulationAge_4164 %in% input$ratios_age |
                              ratios_subset$TargetPopulationAge_65 %in% input$ratios_age |
                              ratios_subset$TargetPopulationAge_adult %in% input$ratios_age |
                              ratios_subset$TargetPopulationAge_unknown %in% input$ratios_age)
    }
    
    if(!is.null(input$ratios_int)){
      ratios_subset<-subset(ratios_subset, ratiosdf$InterventionCareDelivery %in% input$ratios_int | ratiosdf$InterventionDiagnostic%in% input$ratios_int |
                              ratiosdf$InterventionHealthEducationOrBe%in% input$ratios_int |
                              ratiosdf$InterventionImmunization%in% input$ratios_int |
                              ratiosdf$InterventionMedicalDevice%in% input$ratios_int |
                              ratiosdf$InterventionMedicalProcedure%in% input$ratios_int |
                              ratiosdf$InterventionPharmaceutical%in% input$ratios_int |
                              ratiosdf$InterventionScreening%in% input$ratios_int |
                              ratiosdf$InterventionSurgical%in% input$ratios_int |
                              ratiosdf$InterventionOther%in% input$ratios_int |
                              ratiosdf$InterventionNoneNA%in% input$ratios_int |
                              ratiosdf$InterventionMultiple%in% input$ratios_int)
    }
    
    if(!is.null(input$ratios_year)){
      ratios_subset<-subset(ratios_subset, (ratios_subset$PubYear >= input$ratios_year[1])&(ratios_subset$PubYear <= input$ratios_year[2]))
    }
    
    if(!is.null(input$ratios_quadrant)){
      ratios_subset<-subset(ratios_subset, ratios_subset$Quadrant_string %in% input$ratios_quadrant)
    }
    
    #remove if ICER is NA
    ratios_subset<-subset(ratios_subset, !is.na(DisplayRatio))
    
    #order by descending articleID
    ratios_subset<-ratios_subset[order(ratios_subset$ArticleID, decreasing = TRUE), ]
    
    #limit to 100
    if(nrow(ratios_subset)>100) {ratios_subset<-ratios_subset[1:100,]}
    
    #subset output table
    return_table_ratios<-data.frame(
      ratios_subset$ArticleID,
      ratios_subset$citation,
      ratios_subset$TargetPopulation,
      ratios_subset$display_int,
      ratios_subset$ComparatorPhrase,
      ratios_subset$DisplayRatio
    )
    
    colnames(return_table_ratios)<-c("ArticleID", "Citation","Target Population", "Intervention", "Comparator", "ICER (2017 USD)")
    
    return(return_table_ratios)
    
  })
  
  #output ratios dataframe
  output$ratios_data <- DT::renderDataTable(
    ratios_table(), server = TRUE, escape = FALSE, rownames = FALSE,
    options = list(
      #disable search
      searching = FALSE,
      #default to show 10 per page
      pageLength = 10
    )
  )
  
  ####WEIGHTS####
  #ratios dataframe, subset per ArticleID filter
  weights_table<-reactive({
    
    weights_subset<-weightsdf
    
    if(!is.null(input$weights_keywords)){
      keywords_subset_weights<-subset(keywords, keywords$Keywords %in% input$weights_keywords)
      weights_subset<-subset(weights_subset, weights_subset$ArticleID %in% keywords_subset_weights$ArticleID)
    }
    
    if(!is.null(input$weights_ArticleID)){
      weights_subset<-subset(weights_subset, weights_subset$ArticleID %in% input$weights_ArticleID)
    }
    
    if(!is.null(input$weights_healthstate)){
      weights_subset<-subset(weights_subset, weights_subset$HealthState %in% input$weights_healthstate)
    }
    
    if(!is.null(input$weights_utilityweight)){
      weights_subset<-subset(weights_subset, (weights_subset$Weight >= input$weights_utilityweight[1])&(weights_subset$Weight <= input$weights_utilityweight[2]))
    }
    
    if(!is.null(input$weights_source)){
      weights_subset<-subset(weights_subset, weights_subset$WeightDataFromSecondarySource %in% input$weights_source |
                               weights_subset$WeightDataFromPrimarySource %in% input$weights_source |
                               weights_subset$WeightDataFromNoneNS %in% input$weights_source |
                               weights_subset$source %in% input$weights_source)
    }
    
    if(!is.null(input$weights_year)){
      weights_subset<-subset(weights_subset, (weights_subset$PubYear >= input$weights_year[1])&(weights_subset$PubYear <= input$weights_year[2]))
    }
    
    if(!is.null(input$weights_disease)){
      weights_subset<-subset(weights_subset, weights_subset$DisChapter1 %in% input$weights_disease|weights_subset$DisCode1 %in% input$weights_disease)
    }
    
    #order by descending articleID
    weights_subset<-weights_subset[order(weights_subset$ArticleID, decreasing = TRUE), ]
    
    #limit to 100
    if(nrow(weights_subset)>100) weights_subset<-weights_subset[1:100,]
    
    return_table_weights<-data.frame(
      weights_subset$ArticleID,
      weights_subset$citation,
      weights_subset$HealthState,
      weights_subset$Weight,
      weights_subset$source,
      ifelse(is.na(weights_subset$DisCode1),weights_subset$DisChapter1,weights_subset$DisCode1), 
      weights_subset$WeightID
    )
    
    colnames(return_table_weights)<-c("ArticleID","Citation","Health State", "Utility weight", "Weight source","Disease", "WeightID")
    
    return(return_table_weights)
    
  })
  
  #output ratios dataframe
  output$weights_data <- DT::renderDataTable(
    weights_table(), server = TRUE, escape = FALSE,rownames = FALSE,
    options = list(
      #disable search
      searching = FALSE,
      #default to show 10 per page
      pageLength = 10,
      #hide column 7 (weightID)
      columnDefs = (list(list(visible = FALSE, targets=c(6))))
    )
  )
  
  ##METHODS -> RATIOS##
  #react to view ratios click button by switching tabs and populating selectize input for ArticleID
  observeEvent(input$methods_viewratios,{
    
    #clear ratios tab selectize inputs
    updateSelectizeInput(session, "ratios_ArticleID", choices = c('',ratiosdf$ArticleID), server = TRUE)
    updateSelectizeInput(session, "ratios_keywords", choices = c('', keywords$Keywords), server = TRUE)
    updateSelectizeInput(session, "ratios_country", choices = c('', "Afghanistan",	"Albania",	"Algeria",	"American Samoa",	"Andorra",	 "Angola",	"Antigua and Barbuda",	 "Argentina",	"Armenia",	 "Aruba",	 "Australia",	"Austria",	"Azerbaijan",	"Bahamas",	"Bahrain",	"Bangladesh",	"Barbados",	"Belarus",	"Belgium",	 "Belize",	 "Benin",	"Bermuda",	 "Bhutan",	"Bolivia",	 "Bosnia and Herzegovina",	"Botswana",	 "Brazil",	 "British Virgin Islands",	 "Brunei",	"Bulgaria",	 "Burkina Faso",	"Burundi",	"Cabo Verde",	"Cambodia",	"Cameroon",	 "Canada",	 "Cayman Islands",	"Central African Republic",	"Chad",	 "Channel Islands",	 "Chile",	 "China",	"Colombia",	"Comoros",	"Democratic Republic of Congo",	 "Congo",	"Costa Rica",	"Cote dIvoire",	"Croatia",	"Cuba",	 "Curaco",	 "Cyprus",	"Czech Republic",	"Denmark",	"Djibouti",	"Dominica",	 "Dominican Republic",	"Ecuador",	 "Egypt",	"El Salvador",	 "Equatorial Guinea",	"Eritrea",	"Estonia",	"Ethiopia",	 "Faroe Islands",	"Fiji",	"Finland",	 "France",	 "French Polynesia",	 "Gabon",	"The Gambia",	"Georgia",	"Germany",	 "Ghana",	 "Gibraltar",	 "Greece",	 "Greenland",	"Grenada",	"Guam",	"Guatemala",	 "Guinea",	"Guinea Bissau",	 "Guyana",	 "Haiti",	 "Honduras",	"Hong Kong",	"Hungary",	"Iceland",	 "India",	"Indonesia",	"Iran",	"Iraq",	"Ireland",	"Isle of Man",	 "Israel",	 "Italy",	"Jamaica",	 "Japan",	 "Jordan",	"Kazakhstan",	 "Kenya",	"Kiribati",	"North Korea",	 "South Korea",	 "Kosovo",	 "Kuwait",	 "Kyrgyz Republic",	"Laos",	 "Latvia",	"Lebanon",	"Lesotho",	"Liberia",	 "Libya",	"Liechtenstein",	 "Lithuania",	"Luxembourg",	 "Macao",	 "Macedonia",	"Madagascar",	 "Malawi",	"Malaysia",	"Maldives",	"Mali",	 "Malta",	 "Marshall Islands",	"Mauritania",	 "Mauritius",	 "Mexico",	"Micronesia",	"Moldova",	 "Monaco",	"Mongolia",	"Montenegro",	"Morocco",	"Mozambique",	"Myanmar",	"Namibia",	 "Nauru",	 "Nepal",	"Netherlands",	"New Caledonia",	 "New Zealand",	 "Nicaragua",	 "Niger",	"Nigeria",	"Northern Mariana Islands",	 "Norway",	"Oman",	"Pakistan",	 "Palau",	 "Panama",	 "Papua New Guinea",	"Paraguay",	"Peru",	"Philippines",	 "Poland",	"Portugal",	"Puerto Rico",	 "Qatar",	"Romania",	"Russian Federation",	 "Rwanda",	 "Samoa",	"San Marino",	 "Sao Tome and Principe",	 "Saudi Arabia",	"Senegal",	 "Serbia",	 "Seychelles",	 "Sierra Leone",	 "Singapore",	 "Sint Maarten Dutchpart",	"Slovak Republic",	"Slovenia",	"Solomon Islands",	"Somalia",	"South Africa",	"South Sudan",	"Spain",	"Sri Lanka",	"St Kitts and Nevis",	"St Lucia",	"St Martin",	"St Vincent and the Grenadines",	"Sudan",	"Suriname",	"Swaziland",	 "Sweden",	"Switzerland",	"Syria",	"Tajikistan",	"Tanzania",	"Thailand",	 "Timor Leste",	 "Togo",	"Tonga",	"Trinidad and Tobago",	"Tunisia",	 "Turkey",	"Turkmenistan",	 "Turks and Caicos",	 "Tuvalu",	 "Uganda",	"Ukraine",	"United Arab Emirates",	 "United Kingdom",	"United States",	"Uruguay",	 "Uzbekistan",	"Vanuatu",	"Venezuela",	"Vietnam",	"Virgin Islands US",	"Virgin Islands US",	 "Yemen",	 "Zambia",	"Zimbabwe",	 "Taiwan",	"Cook Islands",	"Niue",	 "Wallis and Futuna",	"NA"), server = TRUE)
    updateSelectizeInput(session, "ratios_gender", choices = c('',"Both genders", "Female", "Male", "Gender not specified"), server = TRUE)
    updateSelectizeInput(session, "ratios_age", choices = c('',"0-18 years","19-40 years","41-64 years","65+ years","Adult (unspecified)","Age unknown"), server = TRUE)
    updateSelectizeInput(session, "ratios_int", choices = c('', "Care Delivery",	"Diagnostic",	"Health Education or Behavior",	"Immunization",	"Medical Device",	"Medical Procedure",	"Pharmaceutical",	"Screening",	"Surgical",	"Other",	"None",	"Multiple Intervention Types"), server = TRUE)
    updateSliderInput(session, 'ratios_year', min = 1976, max = 2018, value = c(1976, 2018), step = 1)
    updateSelectizeInput(session, "ratios_quadrant", choices = c('', "Health gain/More cost", "Health loss/More cost", "Health gain/Less cost", "Health loss/Less cost"), server = TRUE)
    
    #make ArticleID vector reactive
    reactive_ArticleID<-reactive({
      methods_table()$ArticleID
    })
    
    #require table to be populated in order to switch tabs
    req(reactive_ArticleID())
    
    #update selectize input
    updateSelectizeInput(session, "ratios_ArticleID", choices = c('',ratiosdf$ArticleID), server = TRUE, selected = reactive_ArticleID()) 
    
    #switch to new tab
    newtab <- switch(input$tabs, "methods" = "ratios","ratios" = "methods")
    updateTabItems(session, "tabs", newtab)
  })
  
  ##METHODS -> WEIGHTS##
  #react to view weights click button by switching tabs and populating selectize input for ArticleID
  observeEvent(input$methods_viewweights,{
    
    #clear weights selectize inputs
    updateSelectizeInput(session, "weights_keywords", choices = c('',keywords$Keywords), server = TRUE)
    updateSelectizeInput(session, "weights_ArticleID", choices = c('',weightsdf$ArticleID), server = TRUE)
    updateSelectizeInput(session, "weights_healthstate", choices = c('',weightsdf$HealthState), server = TRUE)
    updateSliderInput(session, "weights_utilityweight", min = -1, max = 1, value = c(-1, 1), step = .01)
    updateSelectizeInput(session, "weights_source", choices = c('',"Primary source","Secondary source","Both primary and secondary","Source not collected"), server = TRUE)
    updateSliderInput(session, 'weights_year', min = 1976, max = 2018, value = c(1976, 2018), step = 1)
    updateSelectizeInput(session, "weights_disease", choices = c('',weightsdf$DisChapter1, weightsdf$DisCode1), server = TRUE)
    
    #make ArticleID vector reactive
    reactive_ArticleID<-reactive({
      methods_table()$ArticleID
    })
    
    #require table to be populated in order to switch tabs
    req(reactive_ArticleID())
    
    #update selectize input
    updateSelectizeInput(session, "weights_ArticleID", choices = c('',weightsdf$ArticleID), server = TRUE, selected = reactive_ArticleID())    
    
    #switch to new tab
    newtab <- switch(input$tabs, "methods" = "weights","weights" = "methods")
    updateTabItems(session, "tabs", newtab)
  })
  
  ##RATIOS -> METHODS##
  #react to view methods click button by switching tabs and populating selectize input for ArticleID
  observeEvent(input$ratios_viewmethods,{
    
    #clear methods selectize inputs
    updateSelectizeInput(session, "methods_keywords", choices = c('',keywords$Keywords), server = TRUE)
    updateSelectizeInput(session, "methods_ArticleID", choices = c('',methodsdf$ArticleID), server = TRUE)
    updateSelectizeInput(session, "methods_title", choices = c('',methodsdf$Title), server = TRUE)
    updateSelectizeInput(session, "methods_country", choices = c('', "Afghanistan",	"Albania",	"Algeria",	"American Samoa",	"Andorra",	 "Angola",	"Antigua and Barbuda",	 "Argentina",	"Armenia",	 "Aruba",	 "Australia",	"Austria",	"Azerbaijan",	"Bahamas",	"Bahrain",	"Bangladesh",	"Barbados",	"Belarus",	"Belgium",	 "Belize",	 "Benin",	"Bermuda",	 "Bhutan",	"Bolivia",	 "Bosnia and Herzegovina",	"Botswana",	 "Brazil",	 "British Virgin Islands",	 "Brunei",	"Bulgaria",	 "Burkina Faso",	"Burundi",	"Cabo Verde",	"Cambodia",	"Cameroon",	 "Canada",	 "Cayman Islands",	"Central African Republic",	"Chad",	 "Channel Islands",	 "Chile",	 "China",	"Colombia",	"Comoros",	"Democratic Republic of Congo",	 "Congo",	"Costa Rica",	"Cote dIvoire",	"Croatia",	"Cuba",	 "Curaco",	 "Cyprus",	"Czech Republic",	"Denmark",	"Djibouti",	"Dominica",	 "Dominican Republic",	"Ecuador",	 "Egypt",	"El Salvador",	 "Equatorial Guinea",	"Eritrea",	"Estonia",	"Ethiopia",	 "Faroe Islands",	"Fiji",	"Finland",	 "France",	 "French Polynesia",	 "Gabon",	"The Gambia",	"Georgia",	"Germany",	 "Ghana",	 "Gibraltar",	 "Greece",	 "Greenland",	"Grenada",	"Guam",	"Guatemala",	 "Guinea",	"Guinea Bissau",	 "Guyana",	 "Haiti",	 "Honduras",	"Hong Kong",	"Hungary",	"Iceland",	 "India",	"Indonesia",	"Iran",	"Iraq",	"Ireland",	"Isle of Man",	 "Israel",	 "Italy",	"Jamaica",	 "Japan",	 "Jordan",	"Kazakhstan",	 "Kenya",	"Kiribati",	"North Korea",	 "South Korea",	 "Kosovo",	 "Kuwait",	 "Kyrgyz Republic",	"Laos",	 "Latvia",	"Lebanon",	"Lesotho",	"Liberia",	 "Libya",	"Liechtenstein",	 "Lithuania",	"Luxembourg",	 "Macao",	 "Macedonia",	"Madagascar",	 "Malawi",	"Malaysia",	"Maldives",	"Mali",	 "Malta",	 "Marshall Islands",	"Mauritania",	 "Mauritius",	 "Mexico",	"Micronesia",	"Moldova",	 "Monaco",	"Mongolia",	"Montenegro",	"Morocco",	"Mozambique",	"Myanmar",	"Namibia",	 "Nauru",	 "Nepal",	"Netherlands",	"New Caledonia",	 "New Zealand",	 "Nicaragua",	 "Niger",	"Nigeria",	"Northern Mariana Islands",	 "Norway",	"Oman",	"Pakistan",	 "Palau",	 "Panama",	 "Papua New Guinea",	"Paraguay",	"Peru",	"Philippines",	 "Poland",	"Portugal",	"Puerto Rico",	 "Qatar",	"Romania",	"Russian Federation",	 "Rwanda",	 "Samoa",	"San Marino",	 "Sao Tome and Principe",	 "Saudi Arabia",	"Senegal",	 "Serbia",	 "Seychelles",	 "Sierra Leone",	 "Singapore",	 "Sint Maarten Dutchpart",	"Slovak Republic",	"Slovenia",	"Solomon Islands",	"Somalia",	"South Africa",	"South Sudan",	"Spain",	"Sri Lanka",	"St Kitts and Nevis",	"St Lucia",	"St Martin",	"St Vincent and the Grenadines",	"Sudan",	"Suriname",	"Swaziland",	 "Sweden",	"Switzerland",	"Syria",	"Tajikistan",	"Tanzania",	"Thailand",	 "Timor Leste",	 "Togo",	"Tonga",	"Trinidad and Tobago",	"Tunisia",	 "Turkey",	"Turkmenistan",	 "Turks and Caicos",	 "Tuvalu",	 "Uganda",	"Ukraine",	"United Arab Emirates",	 "United Kingdom",	"United States",	"Uruguay",	 "Uzbekistan",	"Vanuatu",	"Venezuela",	"Vietnam",	"Virgin Islands US",	"Virgin Islands US",	 "Yemen",	 "Zambia",	"Zimbabwe",	 "Taiwan",	"Cook Islands",	"Niue",	 "Wallis and Futuna",	"NA"), server = TRUE)
    updateSelectizeInput(session, "methods_author",choices = c('',methodsdf$PrimAuthLastName), server = TRUE)
    updateSelectizeInput(session, "methods_inttype", choices = c('', "Care Delivery","Diagnostic","Health Education/Behavior","Immunization","Medical Device", "Medical Procedure","Pharmaceutical","Screening","Surgical","Other","None/NA"), server = TRUE)
    updateSelectizeInput(session, "methods_prev", choices = c('','Primary', 'Secondary', 'Tertiary'), server = TRUE)
    updateSelectizeInput(session, "methods_icd10", choices = c('',methodsdf$DisCode1, methodsdf$DisChapter1), server = TRUE)
    updateSliderInput(session, 'methods_year', min = 1976, max = 2018, value = c(1976, 2018), step = 1)
    updateSelectizeInput(session, "methods_perspective", choices = c('',"Societal","Health Care Payer","Limited Societal","Health Care Sector","Not stated","Other"), server = TRUE)
    updateSliderInput(session, 'methods_rating', min = 0, max = 7, value = c(0, 7), step = 0.25)
    
    #make ArticleID vector reactive
    reactive_ArticleID<-reactive({
      ratios_table()$ArticleID
    })
    
    #require table to be populated in order to switch tabs
    req(reactive_ArticleID())
    
    #update selectize input
    updateSelectizeInput(session, "methods_ArticleID", choices = c('',methodsdf$ArticleID), server = TRUE, selected = reactive_ArticleID())    
    
    #switch to new tab
    newtab <- switch(input$tabs, "ratios" = "methods","methods" = "ratios")
    updateTabItems(session, "tabs", newtab)
  })
  
  ##RATIOS -> WEIGHTS##
  #react to view weights click button by switching tabs and populating selectize input for ArticleID
  observeEvent(input$ratios_viewweights,{
    
    #clear weights selectize inputs
    updateSelectizeInput(session, "weights_keywords", choices = c('',keywords$Keywords), server = TRUE)
    updateSelectizeInput(session, "weights_ArticleID", choices = c('',weightsdf$ArticleID), server = TRUE)
    updateSelectizeInput(session, "weights_healthstate", choices = c('',weightsdf$HealthState), server = TRUE)
    updateSliderInput(session, "weights_utilityweight", min = -1, max = 1, value = c(-1, 1), step = .01)
    updateSelectizeInput(session, "weights_source", choices = c('',"Primary source","Secondary source","Both primary and secondary","Source not collected"), server = TRUE)
    updateSliderInput(session, 'weights_year', min = 1976, max = 2018, value = c(1976, 2018), step = 1)
    updateSelectizeInput(session, "weights_disease", choices = c('',weightsdf$DisChapter1, weightsdf$DisCode1), server = TRUE)
    
    #make ArticleID vector reactive
    reactive_ArticleID<-reactive({
      ratios_table()$ArticleID
    })
    
    #require table to be populated in order to switch tabs
    req(reactive_ArticleID())
    
    #update selectize input
    updateSelectizeInput(session, "weights_ArticleID", choices = c('',weightsdf$ArticleID), server = TRUE, selected = reactive_ArticleID())    
    
    #switch to new tab
    newtab <- switch(input$tabs, "ratios" = "weights","weights" = "ratios")
    updateTabItems(session, "tabs", newtab)
  })
  
  ##WEIGHTS -> METHODS##
  #react to view methods click button by switching tabs and populating selectize input for ArticleID
  observeEvent(input$weights_viewmethods,{
    
    #clear methods selectize inputs
    updateSelectizeInput(session, "methods_keywords", choices = c('',keywords$Keywords), server = TRUE)
    updateSelectizeInput(session, "methods_ArticleID", choices = c('',methodsdf$ArticleID), server = TRUE)
    updateSelectizeInput(session, "methods_title", choices = c('',methodsdf$Title), server = TRUE)
    updateSelectizeInput(session, "methods_country", choices = c('', "Afghanistan",	"Albania",	"Algeria",	"American Samoa",	"Andorra",	 "Angola",	"Antigua and Barbuda",	 "Argentina",	"Armenia",	 "Aruba",	 "Australia",	"Austria",	"Azerbaijan",	"Bahamas",	"Bahrain",	"Bangladesh",	"Barbados",	"Belarus",	"Belgium",	 "Belize",	 "Benin",	"Bermuda",	 "Bhutan",	"Bolivia",	 "Bosnia and Herzegovina",	"Botswana",	 "Brazil",	 "British Virgin Islands",	 "Brunei",	"Bulgaria",	 "Burkina Faso",	"Burundi",	"Cabo Verde",	"Cambodia",	"Cameroon",	 "Canada",	 "Cayman Islands",	"Central African Republic",	"Chad",	 "Channel Islands",	 "Chile",	 "China",	"Colombia",	"Comoros",	"Democratic Republic of Congo",	 "Congo",	"Costa Rica",	"Cote dIvoire",	"Croatia",	"Cuba",	 "Curaco",	 "Cyprus",	"Czech Republic",	"Denmark",	"Djibouti",	"Dominica",	 "Dominican Republic",	"Ecuador",	 "Egypt",	"El Salvador",	 "Equatorial Guinea",	"Eritrea",	"Estonia",	"Ethiopia",	 "Faroe Islands",	"Fiji",	"Finland",	 "France",	 "French Polynesia",	 "Gabon",	"The Gambia",	"Georgia",	"Germany",	 "Ghana",	 "Gibraltar",	 "Greece",	 "Greenland",	"Grenada",	"Guam",	"Guatemala",	 "Guinea",	"Guinea Bissau",	 "Guyana",	 "Haiti",	 "Honduras",	"Hong Kong",	"Hungary",	"Iceland",	 "India",	"Indonesia",	"Iran",	"Iraq",	"Ireland",	"Isle of Man",	 "Israel",	 "Italy",	"Jamaica",	 "Japan",	 "Jordan",	"Kazakhstan",	 "Kenya",	"Kiribati",	"North Korea",	 "South Korea",	 "Kosovo",	 "Kuwait",	 "Kyrgyz Republic",	"Laos",	 "Latvia",	"Lebanon",	"Lesotho",	"Liberia",	 "Libya",	"Liechtenstein",	 "Lithuania",	"Luxembourg",	 "Macao",	 "Macedonia",	"Madagascar",	 "Malawi",	"Malaysia",	"Maldives",	"Mali",	 "Malta",	 "Marshall Islands",	"Mauritania",	 "Mauritius",	 "Mexico",	"Micronesia",	"Moldova",	 "Monaco",	"Mongolia",	"Montenegro",	"Morocco",	"Mozambique",	"Myanmar",	"Namibia",	 "Nauru",	 "Nepal",	"Netherlands",	"New Caledonia",	 "New Zealand",	 "Nicaragua",	 "Niger",	"Nigeria",	"Northern Mariana Islands",	 "Norway",	"Oman",	"Pakistan",	 "Palau",	 "Panama",	 "Papua New Guinea",	"Paraguay",	"Peru",	"Philippines",	 "Poland",	"Portugal",	"Puerto Rico",	 "Qatar",	"Romania",	"Russian Federation",	 "Rwanda",	 "Samoa",	"San Marino",	 "Sao Tome and Principe",	 "Saudi Arabia",	"Senegal",	 "Serbia",	 "Seychelles",	 "Sierra Leone",	 "Singapore",	 "Sint Maarten Dutchpart",	"Slovak Republic",	"Slovenia",	"Solomon Islands",	"Somalia",	"South Africa",	"South Sudan",	"Spain",	"Sri Lanka",	"St Kitts and Nevis",	"St Lucia",	"St Martin",	"St Vincent and the Grenadines",	"Sudan",	"Suriname",	"Swaziland",	 "Sweden",	"Switzerland",	"Syria",	"Tajikistan",	"Tanzania",	"Thailand",	 "Timor Leste",	 "Togo",	"Tonga",	"Trinidad and Tobago",	"Tunisia",	 "Turkey",	"Turkmenistan",	 "Turks and Caicos",	 "Tuvalu",	 "Uganda",	"Ukraine",	"United Arab Emirates",	 "United Kingdom",	"United States",	"Uruguay",	 "Uzbekistan",	"Vanuatu",	"Venezuela",	"Vietnam",	"Virgin Islands US",	"Virgin Islands US",	 "Yemen",	 "Zambia",	"Zimbabwe",	 "Taiwan",	"Cook Islands",	"Niue",	 "Wallis and Futuna",	"NA"), server = TRUE)
    updateSelectizeInput(session, "methods_author",choices = c('',methodsdf$PrimAuthLastName), server = TRUE)
    updateSelectizeInput(session, "methods_inttype", choices = c('', "Care Delivery","Diagnostic","Health Education/Behavior","Immunization","Medical Device", "Medical Procedure","Pharmaceutical","Screening","Surgical","Other","None/NA"), server = TRUE)
    updateSelectizeInput(session, "methods_prev", choices = c('','Primary', 'Secondary', 'Tertiary'), server = TRUE)
    updateSelectizeInput(session, "methods_icd10", choices = c('',methodsdf$DisCode1, methodsdf$DisChapter1), server = TRUE)
    updateSliderInput(session, 'methods_year', min = 1976, max = 2018, value = c(1976, 2018), step = 1)
    updateSelectizeInput(session, "methods_perspective", choices = c('',"Societal","Health Care Payer","Limited Societal","Health Care Sector","Not stated","Other"), server = TRUE)
    updateSliderInput(session, 'methods_rating', min = 0, max = 7, value = c(0, 7), step = 0.25)
    
    #make ArticleID vector reactive
    reactive_ArticleID<-reactive({
      weights_table()$ArticleID
    })
    
    #require table to be populated in order to switch tabs
    req(reactive_ArticleID())
    
    #update selectize input
    updateSelectizeInput(session, "methods_ArticleID", choices = c('',methodsdf$ArticleID), server = TRUE, selected = reactive_ArticleID())    
    
    #switch to new tab
    newtab <- switch(input$tabs, "weights" = "methods","methods" = "weights")
    updateTabItems(session, "tabs", newtab)
  })
  
  ##WEIGHTS -> RATIOS##
  #react to view ratios click button by switching tabs and populating selectize input for ArticleID
  observeEvent(input$weights_viewratios,{
    
    #clear ratios selectize inputs
    updateSelectizeInput(session, "ratios_keywords", choices = c('',keywords$Keywords), server = TRUE)
    updateSelectizeInput(session, "ratios_ArticleID", choices = c('',ratiosdf$ArticleID), server = TRUE)
    updateSelectizeInput(session, "ratios_country", choices = c('', "Afghanistan",	"Albania",	"Algeria",	"American Samoa",	"Andorra",	 "Angola",	"Antigua and Barbuda",	 "Argentina",	"Armenia",	 "Aruba",	 "Australia",	"Austria",	"Azerbaijan",	"Bahamas",	"Bahrain",	"Bangladesh",	"Barbados",	"Belarus",	"Belgium",	 "Belize",	 "Benin",	"Bermuda",	 "Bhutan",	"Bolivia",	 "Bosnia and Herzegovina",	"Botswana",	 "Brazil",	 "British Virgin Islands",	 "Brunei",	"Bulgaria",	 "Burkina Faso",	"Burundi",	"Cabo Verde",	"Cambodia",	"Cameroon",	 "Canada",	 "Cayman Islands",	"Central African Republic",	"Chad",	 "Channel Islands",	 "Chile",	 "China",	"Colombia",	"Comoros",	"Democratic Republic of Congo",	 "Congo",	"Costa Rica",	"Cote dIvoire",	"Croatia",	"Cuba",	 "Curaco",	 "Cyprus",	"Czech Republic",	"Denmark",	"Djibouti",	"Dominica",	 "Dominican Republic",	"Ecuador",	 "Egypt",	"El Salvador",	 "Equatorial Guinea",	"Eritrea",	"Estonia",	"Ethiopia",	 "Faroe Islands",	"Fiji",	"Finland",	 "France",	 "French Polynesia",	 "Gabon",	"The Gambia",	"Georgia",	"Germany",	 "Ghana",	 "Gibraltar",	 "Greece",	 "Greenland",	"Grenada",	"Guam",	"Guatemala",	 "Guinea",	"Guinea Bissau",	 "Guyana",	 "Haiti",	 "Honduras",	"Hong Kong",	"Hungary",	"Iceland",	 "India",	"Indonesia",	"Iran",	"Iraq",	"Ireland",	"Isle of Man",	 "Israel",	 "Italy",	"Jamaica",	 "Japan",	 "Jordan",	"Kazakhstan",	 "Kenya",	"Kiribati",	"North Korea",	 "South Korea",	 "Kosovo",	 "Kuwait",	 "Kyrgyz Republic",	"Laos",	 "Latvia",	"Lebanon",	"Lesotho",	"Liberia",	 "Libya",	"Liechtenstein",	 "Lithuania",	"Luxembourg",	 "Macao",	 "Macedonia",	"Madagascar",	 "Malawi",	"Malaysia",	"Maldives",	"Mali",	 "Malta",	 "Marshall Islands",	"Mauritania",	 "Mauritius",	 "Mexico",	"Micronesia",	"Moldova",	 "Monaco",	"Mongolia",	"Montenegro",	"Morocco",	"Mozambique",	"Myanmar",	"Namibia",	 "Nauru",	 "Nepal",	"Netherlands",	"New Caledonia",	 "New Zealand",	 "Nicaragua",	 "Niger",	"Nigeria",	"Northern Mariana Islands",	 "Norway",	"Oman",	"Pakistan",	 "Palau",	 "Panama",	 "Papua New Guinea",	"Paraguay",	"Peru",	"Philippines",	 "Poland",	"Portugal",	"Puerto Rico",	 "Qatar",	"Romania",	"Russian Federation",	 "Rwanda",	 "Samoa",	"San Marino",	 "Sao Tome and Principe",	 "Saudi Arabia",	"Senegal",	 "Serbia",	 "Seychelles",	 "Sierra Leone",	 "Singapore",	 "Sint Maarten Dutchpart",	"Slovak Republic",	"Slovenia",	"Solomon Islands",	"Somalia",	"South Africa",	"South Sudan",	"Spain",	"Sri Lanka",	"St Kitts and Nevis",	"St Lucia",	"St Martin",	"St Vincent and the Grenadines",	"Sudan",	"Suriname",	"Swaziland",	 "Sweden",	"Switzerland",	"Syria",	"Tajikistan",	"Tanzania",	"Thailand",	 "Timor Leste",	 "Togo",	"Tonga",	"Trinidad and Tobago",	"Tunisia",	 "Turkey",	"Turkmenistan",	 "Turks and Caicos",	 "Tuvalu",	 "Uganda",	"Ukraine",	"United Arab Emirates",	 "United Kingdom",	"United States",	"Uruguay",	 "Uzbekistan",	"Vanuatu",	"Venezuela",	"Vietnam",	"Virgin Islands US",	"Virgin Islands US",	 "Yemen",	 "Zambia",	"Zimbabwe",	 "Taiwan",	"Cook Islands",	"Niue",	 "Wallis and Futuna",	"NA"), server = TRUE)
    updateSelectizeInput(session, "ratios_gender", choices = c('',"Both genders", "Female", "Male", "Gender not specified"), server = TRUE)
    updateSelectizeInput(session, "ratios_age", choices = c('',"0-18 years","19-40 years","41-64 years","65+ years","Adult (unspecified)","Age unknown"), server = TRUE)
    updateSelectizeInput(session, "ratios_int", choices = c('', "Care Delivery",	"Diagnostic",	"Health Education or Behavior",	"Immunization",	"Medical Device",	"Medical Procedure",	"Pharmaceutical",	"Screening",	"Surgical",	"Other",	"None",	"Multiple Intervention Types"), server = TRUE)
    updateSliderInput(session, 'ratios_year', min = 1976, max = 2018, value = c(1976, 2018), step = 1)
    updateSelectizeInput(session, "ratios_quadrant", choices = c('', "Health gain/More cost", "Health loss/More cost", "Health gain/Less cost", "Health loss/Less cost"), server = TRUE)
    
    #make ArticleID vector reactive
    reactive_ArticleID<-reactive({
      weights_table()$ArticleID
    })
    
    #require table to be populated in order to switch tabs
    req(reactive_ArticleID())
    
    #update selectize input
    updateSelectizeInput(session, "ratios_ArticleID", choices = c('',ratiosdf$ArticleID), server = TRUE, selected = reactive_ArticleID())    
    
    #switch to new tab
    newtab <- switch(input$tabs, "weights" = "ratios","ratios" = "weights")
    updateTabItems(session, "tabs", newtab)
  })
  
  #sponsorship blurbs
  premium_access<-a("premium access.", href = "https://cevr.tuftsmedicalcenter.org/sponsorship", target="_blank")
  db_manager<-a("database manager.", href = "mailto:jlannon@tuftsmedicalcenter.org")
  output$sponsorship_blurb<-renderText({paste("Results limited to 100 entries. To view all results, please consider ", premium_access, "Your company may already have premium access. Inquiries please contact ", db_manager, '<br>','<br>')})
  output$ratios_sponsorship_blurb<-renderText({paste("Results limited to 100 entries. To view all results, please consider ", premium_access, "Your company may already have premium access. Inquiries please contact ", db_manager, '<br>','<br>')})
  output$weights_sponsorship_blurb<-renderText({paste("Results limited to 100 entries. To view all results, please consider ", premium_access, "Your company may already have premium access. Inquiries please contact ", db_manager, '<br>','<br>')})
  
  
} #close server

# Run the application 
shinyApp(ui = ui, server = server)

