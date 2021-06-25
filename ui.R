library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyanimate)

source('preprocessing.R')

ui <- fluidPage( 
  
  
  theme = shinytheme("cyborg"),
  
  img(src = "cad.gif", align = "right", height='47px', width='80px',
      style = 'margin-right: 2em;
        margin-top: 4em'),
  
  img(src = "cad.gif", align = "left", height='47px', width='80px',
      style = 'margin-left: 2em;
        margin-top: 4em'),
  
  # App title ----
  
  titlePanel(h1("Producer Prices Division - Data Visualization Tool",
                style='background-color: midnightblue;
                     text-align: center;
                     color: white;
                     font-size: 40px;
                     padding: 40px'),
             "PPD Visualization Tool"),
  
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("Home",
                       br(),
                       # img(src = "bannersc.png", height='160px', width='600px',
                       #     style = 'display: block;
                       #     margin-left: auto;
                       #     margin-right: auto;'),
                       # img(src = "penguin.gif", height='200px', width='140px',
                       #     style = 'display: block;
                       #          margin-left: 2em;
                       #          margin-top: 6em;'),
                       withAnim(),
                       div(id = 'animator',h1("Welcome! Bienvenue!", align = "center")),
                       # br(),
                       # uiOutput("images", align = "center")),
                       br(),
                       br(),
                       div(id = 'animator2', uiOutput("images", align = "center")),
                       
                       br(),
                       br(),
                       br(),
                       div(p("The Producer Prices Division’s Data Visualization 
                                               Tool (", strong("PPD-VT"), ") compiles data from 23 Statistics Canada 
                                               surveys and allows the user to select various figures 
                                               within those surveys to visualize their index and percentage 
                                               change evolution over time. This R Shiny application is 
                                               constantly updated as it web-scrapes public CODR tables 
                                               on a daily basis. ", style = 'text-align: justify;
                                                                             color: white;
                                                                             background-color: black;
                                                                             margin-left: 2em;
                                                                             margin-right: 2em;
                                                                             font-size:20px
                                                                             ')),
                       br(),
                       div(p("Besides serving as a data visualization tool, this application also presents 
                         a brief description of each survey and allows the user to download the data used to generate 
                         the plots. Additionnally, the ", strong("Editor"), " tab allows the user to compare different
                         figures across the 23 surveys.", style = 'text-align: justify;
                                                                             color: white;
                                                                             background-color: black;
                                                                             margin-left: 2em;
                                                                             margin-right: 2em;
                                                                             font-size:20px
                                                                             ')),
                       br(),
                       div(p("For further assistance and more information about PPD-VT, please refer yourself to the ", strong("Contact"), " tab.", style = 'text-align: justify;
                                                                             color: white;
                                                                             background-color: black;
                                                                             margin-left: 2em;
                                                                             margin-right: 2em;
                                                                             font-size:20px
                                                                             ')),
                       br(),
                       hr(),
                       br(),
                       div(id = 'animaespi111', h4(tags$u("Learn how to use the App in less than 3 minutes with the following video tutorial!"),
                                                   style = 'margin-left: 1em')),
                       
                       div(tags$video(src = 'videoppdvt.mp4', type = "video/mp4", controls = "controls", width = '1400px', height = '850px'), style = 'text-align: center'),
                       br(),
                       hr(),
                       br(),
                       div(p("Special thanks to Roland Hebert, Xin Hua, and Dragos Ifrim for their valuable feedback in the development
                         of this Application."), style = 'text-align: center;
                                                                             color: white;
                                                                             background-color: black;
                                                                             margin-left: 2em;
                                                                             margin-right: 2em;
                                                                             font-size:20px
                                                                             '),
                       br()
                       
                       
                       
                       
              ),
              tabPanel("AESPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       withAnim(),
                       div(id = 'animaespi', h4("Architectural, Engineering and Related Services Price Index (AESPI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Architectural, Engineering
                           and Related Services Price Index (AESPI) is a longitudinal quarterly
                           survey that collects information on the prices of architectural,
                           engineering, and surveying and mapping services. Architectural
                           services are divided into two categories, Architecture and Landscape
                           Architecture. Survey and mapping services are delineated into
                           geophysical and non-geophysical categories. The AESPI series is a
                           useful indicator of economic activity in the architectural, engineering
                           and related services industry, and can also prove helpful as a
                           supplementary tool for performance evaluation, cost monitoring,
                           contract assessment and benchmark comparisons. In addition, the
                           index is used by the Canadian System of Macroeconomic Accounts to
                           arrive at estimates of real value-added Gross Domestic Product (GDP)
                           for the industry and to measure changes in productivity.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Quarterly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&Id=1291660", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlaespi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       # selectInput("regaespi", "Select Region:",
                       #             choices = unique(aespi$`GEO`),
                       #             selected = "Canada",
                       #             multiple = FALSE),
                       selectInput("prodaespi", "Select a figure:",
                                   choices = unique(aespi_special$`NAICS`),
                                   selected = "Architectural, engineering and related services price index (Canada)",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotaespi", height = 650),
                       br(),
                       plotlyOutput("percentaespi", height = 650),
                       # 
                       br()
              ),
              
              tabPanel("ASPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animaspi', h4("Accounting Services Price Index (ASPI)",
                                               style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Accounting Services Price Index 
                           (ASPI) is a longitudinal annual survey that collects information on 
                           the prices of accounting services. The ASPI series is a useful indicator 
                           of economic activity in the accounting services industry, and can 
                           also prove helpful as a supplementary tool for performance evaluation, 
                           cost monitoring, contract assessment and benchmark comparisons. 
                           In addition, the indexes are used by the Canada's System of 
                           Macroeconomic Accounts to arrive at estimates of real value added 
                           Gross Domestic Product (GDP) for the industry and to measure changes 
                           in productivity.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Annual.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=2334", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlaspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       
                       # selectInput("prodaspi", "Select a figure:",
                       #             choices = unique(aspi$`Class of service`),
                       #             selected = "Accounting Services Price Index",
                       #             multiple = TRUE, 
                       #             width = "100%"),
                       
                       
                       selectizeInput(
                         inputId = "prodaspi", 
                         label = "Select a figure:", 
                         choices = unique(aspi$`Class of service`), 
                         selected = "Accounting Services Price Index",
                         multiple = TRUE,
                         width = "100%"
                       ),
                       
                       
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       #br(),
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotaspi", height = 650),
                       br(),
                       plotlyOutput("percentaspi", height = 650),
                       br()
                       # )
                       # )
              ),
              tabPanel("CIMERLSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       
                       br(),
                       div(id = 'animcimerlspi', h4("Commercial and Industrial Machinery and Equipment Rental and Leasing Services Price Index (CIMERLSPI)",
                                                    style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "This survey collects information 
                           needed to produce indexes that measure the monthly changes in the 
                           prices for the commercial and industrial machinery and equipment 
                           rental and leasing industry. The estimates are produced on a 
                           quarterly basis. The indexes on monthly changes in the prices 
                           for the commercial and industrial machinery and equipment rental 
                           and leasing industry series is a useful indicator of the industry's 
                           economic activity, and can also prove helpful as a tool for 
                           performance evaluation, cost monitoring, contract assessment, 
                           and benchmark comparisons. In addition, the indexes are used as 
                           price deflators by the Canadian System of National Accounts in 
                           order to produce estimates of real output for the industry.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly. Quarterly data is also available.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=5137", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlcimerlspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodcimerlspi", "Select a figure:",
                                   choices = unique(cimerlspi$`North American Industry Classification System (NAICS)`),
                                   selected = "Commercial and industrial machinery and equipment rental and leasing [5324]",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotcimerlspi", height = 650),
                       br(),
                       plotlyOutput("percentcimerlspi", height = 650),
                       br()
                       # )
                       # )
              ),
              tabPanel("CMSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       
                       br(),
                       div(id = 'animcmspi', h4("Couriers and Messengers Services Price Index (CMSPI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Couriers and Messengers 
                           Services Price Index (CMSPI) is a monthly price index measuring 
                           the change over time in prices for courier and messenger services 
                           provided by long and short distance delivery companies to 
                           Canadian-based business clients. The courier services portion 
                           includes deliveries within and between Canadian cities and 
                           provinces/territories, as well as some international deliveries. 
                           The local messenger portion tracks price change for within-city 
                           deliveries only. The CMSPI series is a useful indicator of 
                           economic activity for the couriers and messengers services industry.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=5064", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlcmspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodcmspi", "Select a figure:",
                                   choices = unique(cmspi$`North American Industry Classification System (NAICS)`),
                                   selected = "Local messengers and local delivery [4922]",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotcmspi", height = 650),
                       br(),
                       plotlyOutput("percentcmspi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              
              
              tabPanel("COSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animcospi', h4("Consulting Services Price Index (COSPI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Consulting Services Price 
                           Index measures quarterly price changes for various consulting 
                           services such as management, environmental, and scientific and 
                           technical consulting services. This price index is a useful 
                           indicator of economic activity in the consulting services industry, 
                           and can also prove helpful as a supplementary tool for performance 
                           evaluation, cost monitoring, contract assessment and benchmark 
                           comparisons. In addition, the indexes are used by the Canadian 
                           System of Macroeconomic Accounts to arrive at estimates of real 
                           value-added for the industry and to measure changes in productivity 
                           in this industry.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Quarterly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&Id=1174370", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlcospi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       selectInput("prodcospi", "Select a figure:",
                                   choices = unique(cospi$`Class of service`),
                                   selected = "Total price",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotcospi", height = 650),
                       br(),
                       plotlyOutput("percentcospi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              tabPanel("CPPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animcppi', h4("Computer and Peripherals Price Indexes (CPPI)",
                                               style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Computer and Peripherals 
                           Price Indexes (CPPI) are monthly series measuring changes over 
                           time in the price of computers, computer peripherals and smartphones 
                           sold to governments, businesses and households. The methodology for 
                           producing these series employs the hedonic method, the result being 
                           an index series that tracks pure price change. These index series are 
                           used by economists, industry analysts and the general public to track 
                           and comprehend events and trends in this important contributor to the 
                           Information and Communication Technology (ICT) sector.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=5032", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlcppi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodcppi", "Select a figure:",
                                   choices = unique(cppi$`Type of peripheral`),
                                   selected = "Computer monitors and computer printers [E12]",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotcppi", height = 650),
                       br(),
                       plotlyOutput("percentcppi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              tabPanel("CRSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animcrspi', h4("Commercial Rents Services Price Index (CRSPI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Commercial Rents Services 
                           Price Index measures the change over time in the net effective 
                           rent for occupied commercial building space in Canada. The 
                           estimates are produced on a quarterly basis. Prices collected 
                           are the average rents, measured in price per square foot, for 
                           a sample of commercial buildings. The price index for the industry 
                           can be used in conjunction with other service price indexes to 
                           monitor inflation and is also used by the Canadian System of 
                           National Accounts to deflate this sector of the economy.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly. Quarterly data is also available.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=5123", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlcrspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       actionButton("crspitimechangequarterly", "Quarterly Data"),
                       actionButton("crspitimechangemonthly", "Monthly Data"),
                       selectInput("prodcrspi", "Select a figure:",
                                   choices = unique(crspi_special$`Building Type`),
                                   selected = "Total, building type (Canada)",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotcrspi", height = 650),
                       br(),
                       plotlyOutput("percentcrspi", height = 650),
                       
                       
                       br()
                       
              ),
              
              tabPanel("CSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animcspi', h4("Commercial Software Price Index (CSPI)",
                                               style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Commercial Software Price Index (CSPI) 
                           is a monthly series measuring the change in the purchase price of 
                           software typically bought by businesses and governments. The index 
                           series is used by economists, industry analysts and the general public 
                           to track and comprehend events and trends in this important contributor 
                           to the Information, Communication Technology (ICT) sector.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=5068", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlcspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodcspi", "Select a figure:",
                                   choices = unique(cspi$`Index`),
                                   selected = "Commercial software",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotcspi", height = 650),
                       br(),
                       plotlyOutput("percentcspi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              
              tabPanel("CUWRI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animcuwri', h4("Construction Union Wage Rate Index (CUWRI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Construction Union Wage Rate Index measures 
                           monthly changes over time in the collective agreement hourly rates, where they exist, 
                           for 17 trades engaged in building construction in 25 census metropolitan areas. These 
                           series can be employed in several ways, including keeping users abreast of pay scale 
                           changes within the unionized construction work force, identifying differences between 
                           trades and between regions, incorporating them into the escalation clauses of construction 
                           contracts and time series analysis.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=2307", "here."),
                         'To download the data used to generate the plots, please press the button \"Download". Please note that this data
                           also contains figures at lower geographical aggregation levels (provincial and big cities).',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlcuwri", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodcuwri", "Select a construction trade:",
                                   choices = unique(cuwri_special$`Construction trades`),
                                   selected = "Reinforcing steel erector (Basic construction union wage rate indexes - Canada)",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotcuwri", height = 650),
                       br(),
                       plotlyOutput("percentcuwri", height = 650),
                       
                       br()
              ),
              
              
              
              tabPanel("EPSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animepspi', h4("Electric Power Selling Price Indexes for Non-residential Customers (EPSPI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Electric Power Selling Price Index (EPSPI) 
                           is a monthly series measuring the price movements of sales of electricity 
                           by distributors to commercial and industrial users. The index is a useful 
                           measure of the change in the cost of electric power to the non-residential 
                           customer.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=2325&wbdisable=true", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlepspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodepspi", "Select a figure:",
                                   choices = unique(epspi_special$`Index`),
                                   selected = "Electric power selling price under 5000kw (Canada)",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotepspi", height = 650),
                       br(),
                       plotlyOutput("percentepspi", height = 650),
                       
                       
                       br()
                       
              ),
              
              tabPanel("FHMCFSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animf', h4("For-hire Motor Carrier Freight Services Price Index (FHMCFSPI)",
                                            style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The for-hire motor carrier freight 
                           industry is a vital part of the Canadian economy and the services 
                           that the businesses provide is crucial for an effective and efficient 
                           flow of goods. The For-hire Motor Carrier Freight Services Price Report 
                           survey collects prices of service/shipment transactions which are 
                           essential to the creation of a price index for this sector. The 
                           index measures the movement of prices for the services that are 
                           provided by the trucking industry. The For-hire Motor Carrier Freight 
                           Services Price Index (FHMCFSPI) can be used by businesses to measure 
                           their performance against industry standards, to plan marketing 
                           strategies or to prepare business plans for investors. Governments 
                           use index data to develop national and regional economic policies 
                           and to develop programs to promote domestic and international 
                           competitiveness. The data are also used by trade associations, 
                           business analysts and investors to study the economic performance 
                           and characteristics of the industry.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly. Quarterly data is also available.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&Id=1287882", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlf", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       selectInput("prodf", "Select a figure:",
                                   choices = unique(f$`North American Industry Classification System (NAICS)`),
                                   selected = "Specialized freight (except used goods) trucking, local [48422]",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotf", height = 650),
                       br(),
                       plotlyOutput("percentf", height = 650),
                       br()
                       # )
                       # )
              ),
              
              tabPanel("FIPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animfipi', h4("Farm Input Price Index (FIPI)",
                                               style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Farm Input Price Index (FIPI) 
                           is an indicator of the change in input costs faced by Canadian 
                           farmers. As such, the FIPI can be used to monitor price changes, 
                           which are considered in the operations of marketing boards and in 
                           price stabilization programs. Governments use index data to develop 
                           national and regional economic policies related to the agriculture 
                           sector.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Quarterly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=2305", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlfipi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       selectInput("prodfipi", "Select a figure:",
                                   choices = unique(fipi_special$`Price index`),
                                   selected = "Farm input total (Canada)",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotfipi", height = 650),
                       br(),
                       plotlyOutput("percentfipi", height = 650),
                       
                       br()
                       
                       # )
                       # )
              ),
              
              tabPanel("IBSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animibspi', h4("Investment Banking Services Price Index (IBSPI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Investment Banking Services Price 
                           Index (IBSPI) measures annual price changes over time for investment 
                           banking services in Canada. Prices are derived as a weighted average 
                           of percentage commissions for new issues of equity and debt on Canadian 
                           financial markets. The primary purpose of this measure is to deflate the 
                           associated commodity output in the Canadian System of Macroeconomic 
                           Accounts (CSMA).",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Annual.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=5239", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlibspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodibspi", "Select a figure:",
                                   choices = unique(ibspi$`GEO`),
                                   selected = "Canada",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotibspi", height = 650),
                       br(),
                       plotlyOutput("percentibspi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              tabPanel("IPPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animippi', h4("Industrial Product Price Index (IPPI)", 
                                               style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Industrial Product Price Index (IPPI) 
                                   measures price changes for major commodities sold by 
                                   manufacturers operating in Canada. The prices collected 
                                   are for goods sold at the factory gate. As a result, 
                                   the prices covered by the IPPI refer not to what a 
                                   purchaser pays, but to what the producer receives. 
                                   They exclude all indirect taxes, such as sales taxes 
                                   and tariffs as this money does not go to the factors 
                                   of production (i.e. labour, capital, or profit). They 
                                   also exclude any transportation service performed by a 
                                   common carrier beyond the factory gate and any distribution 
                                   services performed by the retail or wholesale trade 
                                   industries.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=2318", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlippi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       
                       
                       
                       
                       selectInput("prod", "Select a figure:",
                                   choices = unique(ippi$`North American Product Classification System (NAPCS)`),
                                   selected = "Total Industrial product price index (IPPI), excluding energy and petroleum products",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       #br(),
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plot", height = 650),
                       br(),
                       plotlyOutput("percentplot", height = 650),
                       br()
                       # )
                       # )
              ),
              
              tabPanel("IPSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animipspi', h4("Informatics Professional Services Price Indexes (IPSPI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The informatics professional 
                           services price indexes measure annual price changes for various 
                           informatics services such as data processing and hosting; 
                           software and software licensing; computer systems design; 
                           and custom software design services . These price indexes 
                           are useful indicators of economic activity in the informatics 
                           services industry, and can also prove helpful as a supplementary 
                           tool for performance evaluation, cost monitoring, contract 
                           assessment and benchmark comparisons. In addition, the indexes 
                           are used by the Canadian System of Macroeconomic Accounts (CSMA) 
                           to arrive at estimates of real value added for the industry and 
                           to measure changes in productivity in this industry.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Annual.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=2333", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlipspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       selectInput("prodipspi", "Select a figure:",
                                   choices = unique(ipspi$`North American Product Classification System (NAPCS)`),
                                   selected = "Data processing, hosting, and related services [75111]",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotipspi", height = 650),
                       br(),
                       plotlyOutput("percentipspi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              tabPanel("MEPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animmepi', h4("Machinery and Equipment Price Index (MEPI)",
                                               style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Machinery and Equipment Price Index (MEPI), 
                           provides quarterly estimates of price changes for machinery and equipment 
                           purchased by industries in Canada. The MEPI is used by the Canadian 
                           System of National Accounts (CSNA) to calculate constant price estimates 
                           of final demand purchases of capitalized machinery and equipment, through 
                           the deflation process. As an economic indicator, the MEPI provides 
                           information on the changing costs of capital investment by industries 
                           in Canada. This information is organized from both the industry and 
                           commodity perspectives. In addition, the MEPI tracks these price 
                           movements on a domestic and on an imported basis.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Quarterly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=2312", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlmepi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodmepi", "Select a figure:",
                                   choices = unique(mepi_special$Commodity),
                                   selected = "Carpets and rugs (Total domestic and imported)",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotmepi", height = 650),
                       br(),
                       plotlyOutput("percentmepi", height = 650),
                       
                       
                       br()
                       # )
                       # )
              ),
              
              tabPanel("NHPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animnhpi', h4("New Housing Price Index (NHPI)",
                                               style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The New Housing Price Index (NHPI) 
                           is a monthly series that measures changes over time in the contractors' 
                           selling prices of new residential houses, where detailed specifications 
                           pertaining to each house remain the same between two consecutive months.
                           The survey covers the following dwelling types: single homes, semi-detached 
                           homes and townhouses. The survey also collects contractors' estimates of 
                           the current value (evaluated at market price) of the land. These estimates 
                           are independently indexed to provide the published series for land. The 
                           current value of the structure is also independently indexed and is 
                           presented as the house series.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=2310", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlnhpi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodnhpi", "Select a figure:",
                                   choices = unique(nhpi_special$`New housing price indexes`),
                                   selected = "Total (house and land) (Canada)",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotnhpi", height = 650),
                       br(),
                       plotlyOutput("percentnhpi", height = 650),
                       
                       
                       br()
                       # )
                       # )
              ),
              
              tabPanel("NLSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animnlspi', h4("New Lending Services Price Index (NLSPI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The NLSPI measures monthly price 
                           changes over time for new lending services in Canada; the estimates 
                           are produced on a quarterly basis. Prices are derived as the difference 
                           between annual percentage rates for new loan products and weighted 
                           averages of yields on financial market instruments. The variables 
                           used to derive the prices are weighted annual percentage rates for 
                           new lending services, funds advanced by product, and market rates. 
                           The primary purpose of this measure is to provide supplemental 
                           information to help inform the deflation of output in the Canadian 
                           System of Macroeconomic Accounts (CSMA) for BS5221A0 Banking and Other 
                           Depository Credit Intermediation. The index represents new lending only, 
                           and as such has limited coverage in relation to the overall activity of 
                           the industry which includes services provided on existing loans as well 
                           as other activities.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly. Quarterly data is also available.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=5207", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlnlspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodnlspi", "Select a figure:",
                                   choices = unique(nlspi$`GEO`),
                                   selected = "Canada",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotnlspi", height = 650),
                       br(),
                       plotlyOutput("percentnlspi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              
              
              
              
              tabPanel("PASPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animpaspi', h4("Passenger Air Services Price Index (PASPI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Passenger Air Services Price 
                           Index is an annual series measuring the price change for base air 
                           fares (i.e. fares excluding taxes and surcharges), providing 
                           indications of the overall trend of domestic and international 
                           fares over time. Data collected for Canadian Level I carriers are 
                           used to produce annual price indexes. The Passenger Air Services 
                           Price Index can be used by businesses to measure their performance 
                           against industry standards, to plan marketing strategies or to prepare 
                           business plans for investors. Governments use index data to develop 
                           national and regional economic policies and to develop programs to 
                           promote domestic and international competitiveness. The data are also 
                           used by trade associations, business analysts and investors to study 
                           the economic activity, performance and characteristics of the industry.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Inactive.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Annual.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&Id=1276170", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlpaspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodpaspi", "Select a figure:",
                                   choices = unique(paspi$`Sector`),
                                   selected = "Total, domestic and international",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotpaspi", height = 650),
                       br(),
                       plotlyOutput("percentpaspi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              tabPanel("RMPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animrmpi', h4("Raw Materials Price Index (RMPI)",
                                               style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Raw Materials Price Index (RMPI) measures 
                           price changes for raw materials purchased for further processing by 
                           manufacturers operating in Canada. As a purchasers' price index, prices 
                           include all charges purchasers incur to bring a commodity to the 
                           establishment gate. They include transportation charges, net taxes 
                           paid, custom duties, as well as subsidies, if applicable. The RMPI 
                           is produced and published together with the IPPI as it meets many 
                           of the same interest and needs. The index provides the movement 
                           in prices for a group of major input products into goods produced 
                           in Canada. It helps cover the spectrum of price changes in the 
                           Canadian economy, and is valuable directly and in relation to the 
                           price movements of products derived in part from these materials. 
                           In addition, the RMPI series supports the Canadian System of 
                           Macro-Economic Accounts (CSMA), where it is used in the calculation 
                           of real Gross Domestic Product (GDP). Together, these indicators 
                           serve as an important indicator of the health of the economy.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=2306", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlrmpi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       selectInput("prodrmpi", "Select a figure:",
                                   choices = unique(rmpi$`North American Product Classification System (NAPCS)`),
                                   selected = "Total Raw materials price indexes (RMPI), excluding crude energy products",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotrmpi", height = 650),
                       br(),
                       plotlyOutput("percentrmpi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              tabPanel("RSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animrspi', h4("Retail Service Price Index (RSPI)", style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "This index measures the price movements 
                           of services provided by retailers, and is used to answer questions 
                           related to inflation, real output and productivity of the retail sector. 
                           The RSPI can also be used by businesses to measure their performance 
                           against industry trends, as well as for international comparisons of 
                           productivity, inflation and trade.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly. Quarterly data is also available.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=5135", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlrspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodrspi", "Select a figure:",
                                   choices = unique(rspi$`North American Industry Classification System (NAICS)`),
                                   selected = "Retail trade [44-45]",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotrspi", height = 650),
                       br(),
                       plotlyOutput("percentrspi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              tabPanel("TASPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animtaspi', h4("Traveller Accommodation Services Price Index (TASPI)",
                                                style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The Traveller Accommodation Services 
                           Price Index (TASPI) is a monthly series measuring the price change for 
                           short-term accommodation services. These services comprise the provision 
                           of rooms for an overnight or short stay without any meals or other 
                           services (i.e. parking, Internet, etc.). The index reflects changes 
                           in room rates excluding all taxes, and covers hotel and motel lodging 
                           services. The TASPI is a useful indicator of the economic activity in 
                           the short-term traveller accommodation services industry and the tourism 
                           sector in general. The series can also be used as a measure of one 
                           important component of the business travel cost. In addition, the 
                           TASPI is used by the Canadian System of National Accounts for the 
                           estimation of the real value of the gross output of the short-term 
                           traveller accommodation service industry through deflation.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Inactive.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly. Quarterly data is also available.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=2336", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dltaspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodtaspi", "Select a figure:",
                                   choices = unique(taspi_special$`Client groups`),
                                   selected = "Total, all client groups (Canada)",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plottaspi", height = 650),
                       br(),
                       plotlyOutput("percenttaspi", height = 650),
                       
                       br()
                       
                       # )
                       # )
              ),
              
              tabPanel("WSPI",
                       # Sidebar layout with input and output definitions ----
                       # sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       # sidebarPanel(
                       
                       # Input: Select the random distribution type ----
                       br(),
                       div(id = 'animwspi', h4("Wholesale Services Price Index (WSPI)",
                                               style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "The purpose of this survey is 
                           to collect and compile data to measure the monthly change in 
                           the movement of the price of wholesale services. These prices 
                           are combined and chained to form a price index. The estimates 
                           are produced on a quarterly basis. These price data are combined 
                           to estimate a price index for the wholesale services sector that 
                           can be joined with other business service indexes to provide 
                           better estimates of real output and productivity, monitor inflation 
                           and feed an important research agenda at Statistics Canada.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Status:")), "Active.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Frequency:")), "Monthly. Quarterly data is also available.",
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       p("For more information please click ", tags$a(href = "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=5106", "here."),
                         'To download the data used to generate the plots, please press the button \"Download".',
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       downloadButton("dlwspi", "Download", style = 'margin-left: 2em'),
                       tags$hr(),
                       
                       selectInput("prodwspi", "Select a figure:",
                                   choices = unique(wspi$`North American Industry Classification System (NAICS)`),
                                   selected = "Wholesale trade [41]",
                                   multiple = TRUE, 
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("plotwspi", height = 650),
                       br(),
                       plotlyOutput("percentwspi", height = 650),
                       br()
                       # )
                       # )
              ),
              
              tabPanel("Editor",
                       br(),
                       div(id = 'animeditor', h4("Customize your own visualization!",
                                                 style = 'margin-left: 1em')),
                       br(),
                       p(strong(tags$u("Description:")), "This tab allows the user to select and sketch any figure from the 23 compiled surveys. To easily identify 
                           a particular survey's figure, please search-name it in the Input bar. Please recall that the percentage change plot will follow the same timeliness 
                           as the corresponding selected figure, i.e., if the selected figure has monthly data, then the second plot will reflect its monthly percentage change.",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Warning:")), "Comparison among certain surveys may not be useful as it can induce misleading analysis. Please be warned that 
                           there can exist discrepancies between surveys in the ", strong("timeliness"), " (monthly, quarterly or annual),", strong("classification type"), " (NAPCS, 
                           NAICS, Class of service, etc.), and", strong("reference year"),".",
                         style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '),
                       p(strong(tags$u("Suggested comparisons:")), "The following are examples of surveys which have same timeliness, classification type, and reference year:",
                         tags$ul(tags$li("IPPI and RMPI.", style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   '), tags$li("FHMCFSPI and WSPI.", style = 'text-align: justify;
                                   color: white;
                                   background-color: black;
                                   margin-left: 2em;
                                   margin-right: 2em
                                   ')),
                         style = 'text-align: justify;
                                           color: white;
                                           background-color: black;
                                           margin-left: 2em;
                                           margin-right: 2em
                                   '),
                       tags$hr(),
                       # selectInput("prodeditorsurvey", "Select the surveys you wish to compare:",
                       #             choices = unique(mega_dataset$`Survey`),
                       #             selected = "AESPI",
                       #             multiple = TRUE,
                       #             width = "100%"),
                       selectInput("prodeditor", "Select a figure:",
                                   choices = unique(mega_dataset$`Figure`),
                                   selected = "Architectural, engineering and related services price index (Canada) [AESPI] - NAICS",
                                   multiple = TRUE,
                                   width = "100%"),
                       
                       # uiOutput("prodnum"),
                       
                       # br() element to introduce extra vertical spacing ----
                       
                       
                       # ),
                       
                       # Main panel for displaying outputs ----
                       # mainPanel(
                       plotlyOutput("ploteditor", height = 650),
                       br(),
                       plotlyOutput("percentmega_dataset", height = 650),
                       br()
              ),
              
              
              tabPanel("Contact", 
                       tableOutput("table"),
                       br(),
                       br(),
                       div(p("If you encounter any bugs in the Application or
                         have any additional questions regarding its functioning, please
                         contact _____________ at _____________ for further assistance.", style = 'text-align: justify;
                                                                             color: white;
                                                                             background-color: black;
                                                                             margin-left: 2em;
                                                                             margin-right: 2em;
                                                                             font-size:20px
                                                                             ')),
                       img(src = "thumbsup.png", height='400px', width='400px',
                           style = 'display: block;
                                 margin-left: auto;
                                 margin-right: auto;')
                       
              )
              
              
  ))
