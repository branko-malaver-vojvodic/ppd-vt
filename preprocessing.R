#####Installing packages

# install.packages(c("leaflet","RColorBrewer","shinycssloaders", 
#                    "shinyjs", "shinythemes", "tidyverse"))
# install.packages(c("cansim", "tidyverse", "rgdal", "rmapshaper", "janitor"))


library(cansim)
library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)

ippi_path <- './data/ippi.csv'

download_ippi <- function() {
  # Industrial product price index, by major product group, monthly
  ippi_raw <- get_cansim("18-10-0029-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Product Classification System (NAPCS)`,
           VALUE, STATUS, 
           `Classification Code for North American Product Classification System (NAPCS)`)%>%
    filter(!is.na(VALUE))%>% 
    filter(`North American Product Classification System (NAPCS)`
           %in% "Total Industrial product price index (IPPI), excluding energy and petroleum products") 
  
  # Industrial product price index, by product, monthly
  # This table misses "Total industry IPPI excluding energy and petroleum"
  ippi_prod <- get_cansim("18-10-0030-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Product Classification System (NAPCS)`,
           VALUE, STATUS, 
           `Classification Code for North American Product Classification System (NAPCS)`)%>%
    filter(!is.na(VALUE)) %>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Diesel fuel [26122]", "Diesel fuel"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Diesel fuel", "Diesel fuel [26122]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Kerosene (except jet fuel) [261321]", "Kerosene (except jet fuel)"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Kerosene (except jet fuel)", "Kerosene (except jet fuel) [261321]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Light fuel oils (except kerosene and diesel) [261322]", "Light fuel oils (except kerosene and diesel)"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Light fuel oils (except kerosene and diesel)", "Light fuel oils (except kerosene and diesel) [261322]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Heavy fuel oils [26133]", "Heavy fuel oils"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Heavy fuel oils", "Heavy fuel oils [26133]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Fresh and frozen beef and veal [17211]", "Fresh and frozen beef and veal"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Fresh and frozen beef and veal", "Fresh and frozen beef and veal [17211]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Fresh and frozen pork [17212]", "Fresh and frozen pork"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Fresh and frozen pork", "Fresh and frozen pork [17212]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Complete cattle feed [181121]", "Complete cattle feed"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Complete cattle feed", "Complete cattle feed [181121]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Margarine, butter blends, and butter substitutes [182111]", "Margarine, butter blends, and butter substitutes"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Margarine, butter blends, and butter substitutes", "Margarine, butter blends, and butter substitutes [182111]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Bread, rolls and flatbreads [18313]", "Bread, rolls and flatbreads"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Bread, rolls and flatbreads", "Bread, rolls and flatbreads [18313]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Chocolate and chocolate-type confectionery products [183221]", "Chocolate and chocolate-type confectionery products"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Chocolate and chocolate-type confectionery products", "Chocolate and chocolate-type confectionery products [183221]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Coffee, prepared for retail sale (except concentrates) [191111]", "Coffee, prepared for retail sale (except concentrates)"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Coffee, prepared for retail sale (except concentrates)", "Coffee, prepared for retail sale (except concentrates) [191111]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Cigarettes [212111]", "Cigarettes"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Cigarettes", "Cigarettes [212111]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Men's suits, sport jackets and blazers [231112]", "Men's suits, sport jackets and blazers"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Men's suits, sport jackets and blazers", "Men's suits, sport jackets and blazers [231112]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Softwood lumber (except tongue and groove and other edge worked lumber) [24112]", "Softwood lumber (except tongue and groove and other edge worked lumber)"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Softwood lumber (except tongue and groove and other edge worked lumber)", "Softwood lumber (except tongue and groove and other edge worked lumber) [24112]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Facial tissue [252141]", "Facial tissue"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Facial tissue", "Facial tissue [252141]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Household refrigerators and freezers [382122]", "Household refrigerators and freezers"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Household refrigerators and freezers", "Household refrigerators and freezers [382122]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Corrugated and solid fibre boxes [474211]", "Corrugated and solid fibre boxes"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Corrugated and solid fibre boxes", "Corrugated and solid fibre boxes [474211]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Fresh and frozen chicken [172131]", "Fresh and frozen chicken"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Fresh and frozen chicken", "Fresh and frozen chicken [172131]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Fresh and frozen turkey [172132]", "Fresh and frozen turkey"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Fresh and frozen turkey", "Fresh and frozen turkey [172132]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Ice cream, sherbet and similar frozen desserts [17314]", "Ice cream, sherbet and similar frozen desserts"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Ice cream, sherbet and similar frozen desserts", "Ice cream, sherbet and similar frozen desserts [17314]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Dry pasta [183122]", "Dry pasta"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Dry pasta", "Dry pasta [183122]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Stemmed, redried or reconstituted tobacco [21212]", "Stemmed, redried or reconstituted tobacco"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Stemmed, redried or reconstituted tobacco", "Stemmed, redried or reconstituted tobacco [21212]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Coated fabrics and coated yarns [221116]", "Coated fabrics and coated yarns"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Coated fabrics and coated yarns", "Coated fabrics and coated yarns [221116]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Carpets, rugs and mats [23211]", "Carpets, rugs and mats"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Carpets, rugs and mats", "Carpets, rugs and mats [23211]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Men's pants (except jeans) [231114]", "Men's pants (except jeans)"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Men's pants (except jeans)", "Men's pants (except jeans) [231114]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Protective footwear, of rubber or plastics [231213]", "Protective footwear, of rubber or plastics"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Protective footwear, of rubber or plastics", "Protective footwear, of rubber or plastics [231213]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Aromatic hydrocarbon gases [263212]", "Aromatic hydrocarbon gases"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Aromatic hydrocarbon gases", "Aromatic hydrocarbon gases [263212]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Mixed fertilizers [272114]", "Mixed fertilizers"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Mixed fertilizers", "Mixed fertilizers [272114]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Pesticides and other agricultural chemicals [27212]", "Pesticides and other agricultural chemicals"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Pesticides and other agricultural chemicals", "Pesticides and other agricultural chemicals [27212]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Printing, writing and stamping inks [272132]", "Printing, writing and stamping inks"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Printing, writing and stamping inks", "Printing, writing and stamping inks [272132]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Perfumes and colognes [274123]", "Perfumes and colognes"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Perfumes and colognes", "Perfumes and colognes [274123]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Polyethylene resins [281112]", "Polyethylene resins"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Polyethylene resins", "Polyethylene resins [281112]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Laminated plastic profile shapes [282321]", "Laminated plastic profile shapes"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Laminated plastic profile shapes", "Laminated plastic profile shapes [282321]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Pneumatic tires for passenger cars and similar vehicles [283111]", "Pneumatic tires for passenger cars and similar vehicles"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Pneumatic tires for passenger cars and similar vehicles", "Pneumatic tires for passenger cars and similar vehicles [283111]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Polyethylene plastic pipes and tubes (except flexible) [464111]", "Polyethylene plastic pipes and tubes (except flexible)"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Polyethylene plastic pipes and tubes (except flexible)", "Polyethylene plastic pipes and tubes (except flexible) [464111]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Other hardwood plywood and plywood products [241234]", "Other hardwood plywood and plywood products"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Other hardwood plywood and plywood products", "Other hardwood plywood and plywood products [241234]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Other softwood plywood and plywood products [241236]", "Other softwood plywood and plywood products"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Other softwood plywood and plywood products", "Other softwood plywood and plywood products [241236]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Wooden doors [462111]", "Wooden doors"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Wooden doors", "Wooden doors [462111]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Wood chips [25111]", "Wood chips"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Wood chips", "Wood chips [25111]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Newsprint [25121]", "Newsprint"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Newsprint", "Newsprint [25121]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Metal windows and doors [46613]", "Metal windows and doors"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Metal windows and doors", "Metal windows and doors [46613]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Passenger cars [41111]", "Passenger cars"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Passenger cars", "Passenger cars [41111]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Motor vehicle brakes and brake systems [41354]", "Motor vehicle brakes and brake systems"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Motor vehicle brakes and brake systems", "Motor vehicle brakes and brake systems [41354]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Industrial and commercial fans and blowers [344121]", "Industrial and commercial fans and blowers"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Industrial and commercial fans and blowers", "Industrial and commercial fans and blowers [344121]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Ball and roller bearings [34513]", "Ball and roller bearings"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Ball and roller bearings", "Ball and roller bearings [34513]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Materials handling trucks and tractors [345421]", "Materials handling trucks and tractors"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Materials handling trucks and tractors", "Materials handling trucks and tractors [345421]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Power and distribution transformers [381221]", "Power and distribution transformers"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Power and distribution transformers", "Power and distribution transformers [381221]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Ready-mixed concrete [46512]", "Ready-mixed concrete"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Ready-mixed concrete", "Ready-mixed concrete [46512]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Lime [465131]", "Lime"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Lime", "Lime [465131]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Prefabricated metal buildings and components [47111]", "Prefabricated metal buildings and components"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Prefabricated metal buildings and components", "Prefabricated metal buildings and components [47111]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Caskets and coffins [47513]", "Caskets and coffins"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Caskets and coffins", "Caskets and coffins [47513]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Motor gasoline [26121]", "Motor gasoline"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Motor gasoline", "Motor gasoline [26121]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Plastic pipe fittings and unions [464113]", "Plastic pipe fittings and unions"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Plastic pipe fittings and unions", "Plastic pipe fittings and unions [464113]"))%>%
    
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Waferboard, particle board and medium density fibreboard (MDF) [241221]", "Waferboard, particle board and medium density fibreboard (MDF)"))%>%
    mutate(`North American Product Classification System (NAPCS)`=replace(`North American Product Classification System (NAPCS)`, `North American Product Classification System (NAPCS)` == "Waferboard, particle board and medium density fibreboard (MDF)", "Waferboard, particle board and medium density fibreboard (MDF) [241221]"))
  
  ippi <- rbind(ippi_prod,ippi_raw)
  
  return(ippi)
}

rspi_path <- './data/rspi.csv'

download_rspi <- function() {
  # Industrial product price index, by major product group, monthly
  rspi <- get_cansim("18-10-0251-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Industry Classification System (NAICS)`,
           VALUE, STATUS, 
           `Classification Code for North American Industry Classification System (NAICS)`)%>%
    filter(!is.na(VALUE))
}


rspi_path_q <- './data/rspi_q.csv'

download_rspi_q <- function() {
  # Industrial product price index, by major product group, monthly
  rspi_q <- get_cansim("18-10-0252-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Industry Classification System (NAICS)`,
           VALUE, STATUS, 
           `Classification Code for North American Industry Classification System (NAICS)`)%>%
    filter(!is.na(VALUE))
}




aespi_path <- './data/aespi.csv'

download_aespi <- function() {
  # Industrial product price index, by major product group, monthly
  aespi <- get_cansim("18-10-0164-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Industry Classification System (NAICS)`,
           VALUE, STATUS, 
           `Classification Code for North American Industry Classification System (NAICS)`)%>%
    filter(!is.na(VALUE))
}

aspi_path <- './data/aspi.csv'

download_aspi <- function() {
  # Industrial product price index, by major product group, monthly
  aspi <- get_cansim("18-10-0021-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Class of service`,
           VALUE, STATUS, 
           `Classification Code for Class of service`)%>%
    filter(!is.na(VALUE))
}

cmspi_path <- './data/cmspi.csv'

download_cmspi <- function() {
  # Industrial product price index, by major product group, monthly
  cmspi <- get_cansim("18-10-0072-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Industry Classification System (NAICS)`,
           VALUE, STATUS, 
           `Classification Code for North American Industry Classification System (NAICS)`)%>%
    filter(!is.na(VALUE))
}

cimerlspi_path <- './data/cimerlspi.csv'

download_cimerlspi <- function() {
  # Industrial product price index, by major product group, monthly
  cimerlspi <- get_cansim("18-10-0036-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Industry Classification System (NAICS)`,
           VALUE, STATUS, 
           `Classification Code for North American Industry Classification System (NAICS)`)%>%
    filter(!is.na(VALUE))
}

cimerlspi_path_q <- './data/cimerlspi_q.csv'

download_cimerlspi_q <- function() {
  # Industrial product price index, by major product group, monthly
  cimerlspi_q <- get_cansim("18-10-0064-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Industry Classification System (NAICS)`,
           VALUE, STATUS, 
           `Classification Code for North American Industry Classification System (NAICS)`)%>%
    filter(!is.na(VALUE))
}




cospi_path <- './data/cospi.csv'

download_cospi <- function() {
  # Industrial product price index, by major product group, monthly
  cospi <- get_cansim("18-10-0178-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Class of service`,
           VALUE, STATUS, 
           `Classification Code for Class of service`)%>%
    filter(!is.na(VALUE))
}


crspi_path <- './data/crspi.csv'

download_crspi <- function() {
  # Industrial product price index, by major product group, monthly
  crspi <- get_cansim("18-10-0255", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Building Type`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}


crspi_path_q <- './data/crspi_q.csv'

download_crspi_q <- function() {
  # Industrial product price index, by major product group, monthly
  crspi_q <- get_cansim("18-10-0260-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Building Type`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}



f_path <- './data/fhmcfspi.csv'

download_f <- function() {
  # Industrial product price index, by major product group, monthly
  f <- get_cansim("18-10-0043-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Industry Classification System (NAICS)`,
           VALUE, STATUS, 
           `Classification Code for North American Industry Classification System (NAICS)`)%>%
    filter(!is.na(VALUE))
}


f_path_q <- './data/fhmcfspi_q.csv'

download_f_q <- function() {
  # Industrial product price index, by major product group, monthly
  f_q <- get_cansim("18-10-0044-02", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Industry Classification System (NAICS)`,
           VALUE, STATUS, 
           `Classification Code for North American Industry Classification System (NAICS)`)%>%
    filter(!is.na(VALUE))
}



rmpi_path <- './data/rmpi.csv'

download_rmpi <- function() {
  # Industrial product price index, by major product group, monthly
  rmpi <- get_cansim("18-10-0034-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Product Classification System (NAPCS)`,
           VALUE, STATUS, 
           `Classification Code for North American Product Classification System (NAPCS)`)%>%
    filter(!is.na(VALUE))
}


ipspi_path <- './data/ipspi.csv'

download_ipspi <- function() {
  # Industrial product price index, by major product group, monthly
  ipspi <- get_cansim("18-10-0138-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Product Classification System (NAPCS)`,
           VALUE, STATUS, 
           `Classification Code for North American Product Classification System (NAPCS)`)%>%
    filter(!is.na(VALUE))
}


wspi_path <- './data/wspi.csv'

download_wspi <- function() {
  # Industrial product price index, by major product group, monthly
  wspi <- get_cansim("18-10-0253-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Industry Classification System (NAICS)`,
           VALUE, STATUS, 
           `Classification Code for North American Industry Classification System (NAICS)`)%>%
    filter(!is.na(VALUE))
}



wspi_path_q <- './data/wspi_q.csv'

download_wspi_q <- function() {
  # Industrial product price index, by major product group, monthly
  wspi_q <- get_cansim("18-10-0254-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `North American Industry Classification System (NAICS)`,
           VALUE, STATUS, 
           `Classification Code for North American Industry Classification System (NAICS)`)%>%
    filter(!is.na(VALUE))
}



mepi_path <- './data/mepi.csv'

download_mepi <- function() {
  # Industrial product price index, by major product group, monthly
  mepi <- get_cansim("18-10-0057-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, Commodity,`Machinery and equipment, domestic and imported`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}


cppi_path <- './data/cppi.csv'

download_cppi <- function() {
  # Industrial product price index, by major product group, monthly
  cppi <- get_cansim("18-10-0208-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Type of peripheral`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}


fipi_path <- './data/fipi.csv'

download_fipi <- function() {
  # Industrial product price index, by major product group, monthly
  fipi <- get_cansim("18-10-0258-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Price index`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}


paspi_path <- './data/paspi.csv'

download_paspi <- function() {
  # Industrial product price index, by major product group, monthly
  paspi <- get_cansim("18-10-0033-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Sector`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}




cspi_path <- './data/cspi.csv'

download_cspi <- function() {
  # Industrial product price index, by major product group, monthly
  cspi <- get_cansim("18-10-0061-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Index`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}



epspi_path <- './data/epspi.csv'

download_epspi <- function() {
  # Industrial product price index, by major product group, monthly
  epspi <- get_cansim("18-10-0204-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Index`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}


taspi_path <- './data/taspi.csv'

download_taspi <- function() {
  # Industrial product price index, by major product group, monthly
  taspi <- get_cansim("18-10-0249-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Client groups`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}


taspi_path_q <- './data/taspi_q.csv'

download_taspi_q <- function() {
  # Industrial product price index, by major product group, monthly
  taspi_q <- get_cansim("18-10-0250-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Client groups`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}


nlspi_path <- './data/nlspi.csv'

download_nlspi <- function() {
  # Industrial product price index, by major product group, monthly
  nlspi <- get_cansim("18-10-0246-01", refresh=TRUE)%>%
    select(REF_DATE, GEO,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}


nlspi_path_q <- './data/nlspi_q.csv'

download_nlspi_q <- function() {
  # Industrial product price index, by major product group, monthly
  nlspi_q <- get_cansim("18-10-0262-02", refresh=TRUE)%>%
    select(REF_DATE, GEO,
           VALUE, STATUS, `Lending services price index`)%>%
    filter(!is.na(VALUE))
}




ibspi_path <- './data/ibspi.csv'

download_ibspi <- function() {
  # Industrial product price index, by major product group, monthly
  ibspi <- get_cansim("18-10-0257-01", refresh=TRUE)%>%
    select(REF_DATE, GEO,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}


nhpi_path <- './data/nhpi.csv'

download_nhpi <- function() {
  # Industrial product price index, by major product group, monthly
  nhpi <- get_cansim("18-10-0205-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `New housing price indexes`,
           VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}


cuwri_path <- './data/cuwri.csv'

download_cuwri <- function() {
  # Industrial product price index, by major product group, monthly
  cuwri <- get_cansim("18-10-0140-01", refresh=TRUE)%>%
    select(REF_DATE, GEO, `Construction trades`,
           `Type of wage indexes`, VALUE, STATUS)%>%
    filter(!is.na(VALUE))
}
































update_dataset <- function() {
  
  current_time <- Sys.time()
  
  file_time_ippi <- file.info(ippi_path)$mtime 
  
  file_time_rspi <- file.info(rspi_path)$mtime
  
  file_time_rspi_q <- file.info(rspi_path_q)$mtime
  
  file_time_aespi <- file.info(aespi_path)$mtime
  
  file_time_aspi <- file.info(aspi_path)$mtime
  
  file_time_cmspi <- file.info(cmspi_path)$mtime
  
  file_time_cimerlspi <- file.info(cimerlspi_path)$mtime
  
  file_time_cimerlspi_q <- file.info(cimerlspi_path_q)$mtime
  
  file_time_cospi <- file.info(cospi_path)$mtime
  
  file_time_crspi <- file.info(crspi_path)$mtime
  
  file_time_crspi_q <- file.info(crspi_path_q)$mtime
  
  file_time_f <- file.info(f_path)$mtime
  
  file_time_f_q <- file.info(f_path_q)$mtime
  
  file_time_rmpi <- file.info(rmpi_path)$mtime
  
  file_time_ipspi <- file.info(ipspi_path)$mtime
  
  file_time_wspi <- file.info(wspi_path)$mtime
  
  file_time_wspi_q <- file.info(wspi_path_q)$mtime
  
  file_time_mepi <- file.info(mepi_path)$mtime
  
  file_time_cppi <- file.info(cppi_path)$mtime
  
  file_time_fipi <- file.info(fipi_path)$mtime
  
  file_time_paspi <- file.info(paspi_path)$mtime
  
  file_time_cspi <- file.info(cspi_path)$mtime
  
  file_time_epspi <- file.info(epspi_path)$mtime
  
  file_time_taspi <- file.info(taspi_path)$mtime
  
  file_time_taspi_q <- file.info(taspi_path_q)$mtime
  
  file_time_nlspi <- file.info(nlspi_path)$mtime
  
  file_time_nlspi_q <- file.info(nlspi_path_q)$mtime
  
  file_time_ibspi <- file.info(ibspi_path)$mtime
  
  file_time_nhpi <- file.info(nhpi_path)$mtime
  
  file_time_cuwri <- file.info(cuwri_path)$mtime
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  diff_ippi <- difftime(current_time, file_time_ippi) %>% as.numeric(units = 'hours')
  diff_rspi <- difftime(current_time, file_time_rspi) %>% as.numeric(units = 'hours')
  diff_rspi_q <- difftime(current_time, file_time_rspi_q) %>% as.numeric(units = 'hours')
  
  diff_aespi <- difftime(current_time, file_time_aespi) %>% as.numeric(units = 'hours')
  diff_aspi <- difftime(current_time, file_time_aspi) %>% as.numeric(units = 'hours')
  diff_cmspi <- difftime(current_time, file_time_cmspi) %>% as.numeric(units = 'hours')
  diff_cimerlspi <- difftime(current_time, file_time_cimerlspi) %>% as.numeric(units = 'hours')
  diff_cimerlspi_q <- difftime(current_time, file_time_cimerlspi_q) %>% as.numeric(units = 'hours')
  
  diff_cospi <- difftime(current_time, file_time_cospi) %>% as.numeric(units = 'hours')
  diff_crspi <- difftime(current_time, file_time_crspi) %>% as.numeric(units = 'hours')
  diff_crspi_q <- difftime(current_time, file_time_crspi_q) %>% as.numeric(units = 'hours')
  
  diff_f <- difftime(current_time, file_time_f) %>% as.numeric(units = 'hours')
  diff_f_q <- difftime(current_time, file_time_f_q) %>% as.numeric(units = 'hours')
  
  diff_rmpi <- difftime(current_time, file_time_rmpi) %>% as.numeric(units = 'hours')
  diff_ipspi <- difftime(current_time, file_time_ipspi) %>% as.numeric(units = 'hours')
  diff_wspi <- difftime(current_time, file_time_wspi) %>% as.numeric(units = 'hours')
  diff_wspi_q <- difftime(current_time, file_time_wspi_q) %>% as.numeric(units = 'hours')
  
  diff_mepi <- difftime(current_time, file_time_mepi) %>% as.numeric(units = 'hours')
  diff_cppi <- difftime(current_time, file_time_cppi) %>% as.numeric(units = 'hours')
  diff_fipi <- difftime(current_time, file_time_fipi) %>% as.numeric(units = 'hours')
  diff_paspi <- difftime(current_time, file_time_paspi) %>% as.numeric(units = 'hours')
  diff_cspi <- difftime(current_time, file_time_cspi) %>% as.numeric(units = 'hours')
  diff_epspi <- difftime(current_time, file_time_epspi) %>% as.numeric(units = 'hours')
  diff_taspi <- difftime(current_time, file_time_taspi) %>% as.numeric(units = 'hours')
  diff_taspi_q <- difftime(current_time, file_time_taspi_q) %>% as.numeric(units = 'hours')
  
  diff_nlspi <- difftime(current_time, file_time_nlspi) %>% as.numeric(units = 'hours')
  diff_nlspi_q <- difftime(current_time, file_time_nlspi_q) %>% as.numeric(units = 'hours')
  
  diff_ibspi <- difftime(current_time, file_time_ibspi) %>% as.numeric(units = 'hours')
  diff_nhpi <- difftime(current_time, file_time_nhpi) %>% as.numeric(units = 'hours')
  diff_cuwri <- difftime(current_time, file_time_cuwri) %>% as.numeric(units = 'hours')
  
  
  
  
  
  
  
  
  # Figuring out how many times I should be downloading the data
  # release <- as.POSIXct("2020-07-13 10:51:40 EDT")
  # current_time - file_time
  # release - file_time > 24
  
  
  # Releases IPPI:
  # FLASH: Monday, 2020-07-13
  # Friday, 2020-07-31
  
  if (is.na(diff_ippi) | diff_ippi >= 24) {
    ippi <- download_ippi()
    write_csv(ippi, ippi_path)
  }
  
  if (is.na(diff_rspi) | diff_rspi >= 24) {
    rspi <- download_rspi()
    write_csv(rspi, rspi_path)
  }
  if (is.na(diff_rspi_q) | diff_rspi_q >= 24) {
    rspi_q <- download_rspi_q()
    write_csv(rspi_q, rspi_path_q)
  }
  if (is.na(diff_aespi) | diff_aespi >= 24) {
    aespi <- download_aespi()
    write_csv(aespi, aespi_path)
  }
  if (is.na(diff_aspi) | diff_aspi >= 24) {
    aspi <- download_aspi()
    write_csv(aspi, aspi_path)
  }
  if (is.na(diff_cmspi) | diff_cmspi >= 24) {
    cmspi <- download_cmspi()
    write_csv(cmspi, cmspi_path)
  }
  if (is.na(diff_cimerlspi) | diff_cimerlspi >= 24) {
    cimerlspi <- download_cimerlspi()
    write_csv(cimerlspi, cimerlspi_path)
  }
  if (is.na(diff_cimerlspi_q) | diff_cimerlspi_q >= 24) {
    cimerlspi_q <- download_cimerlspi_q()
    write_csv(cimerlspi_q, cimerlspi_path_q)
  }
  if (is.na(diff_cospi) | diff_cospi >= 24) {
    cospi <- download_cospi()
    write_csv(cospi, cospi_path)
  }
  if (is.na(diff_crspi) | diff_crspi >= 24) {
    crspi <- download_crspi()
    write_csv(crspi, crspi_path)
  }
  if (is.na(diff_crspi_q) | diff_crspi_q >= 24) {
    crspi_q <- download_crspi_q()
    write_csv(crspi_q, crspi_path_q)
  }
  if (is.na(diff_f) | diff_f >= 24) {
    f <- download_f()
    write_csv(f, f_path)
  }
  if (is.na(diff_f_q) | diff_f_q >= 24) {
    f_q <- download_f_q()
    write_csv(f_q, f_path_q)
  }
  if (is.na(diff_rmpi) | diff_rmpi >= 24) {
    rmpi <- download_rmpi()
    write_csv(rmpi, rmpi_path)
  }
  if (is.na(diff_ipspi) | diff_ipspi >= 24) {
    ipspi <- download_ipspi()
    write_csv(ipspi, ipspi_path)
  }
  if (is.na(diff_wspi) | diff_wspi >= 24) {
    wspi <- download_wspi()
    write_csv(wspi, wspi_path)
  }
  if (is.na(diff_wspi_q) | diff_wspi_q >= 24) {
    wspi_q <- download_wspi_q()
    write_csv(wspi_q, wspi_path_q)
  }
  if (is.na(diff_mepi) | diff_mepi >= 24) {
    mepi <- download_mepi()
    write_csv(mepi, mepi_path)
  }
  if (is.na(diff_cppi) | diff_cppi >= 24) {
    cppi <- download_cppi()
    write_csv(cppi, cppi_path)
  }
  if (is.na(diff_fipi) | diff_fipi >= 24) {
    fipi <- download_fipi()
    write_csv(fipi, fipi_path)
  }
  if (is.na(diff_paspi) | diff_paspi >= 24) {
    paspi <- download_paspi()
    write_csv(paspi, paspi_path)
  }
  if (is.na(diff_cspi) | diff_cspi >= 24) {
    cspi <- download_cspi()
    write_csv(cspi, cspi_path)
  }
  if (is.na(diff_epspi) | diff_epspi >= 24) {
    epspi <- download_epspi()
    write_csv(epspi, epspi_path)
  }
  if (is.na(diff_taspi) | diff_taspi >= 24) {
    taspi <- download_taspi()
    write_csv(taspi, taspi_path)
  }
  if (is.na(diff_taspi_q) | diff_taspi_q >= 24) {
    taspi_q <- download_taspi_q()
    write_csv(taspi_q, taspi_path_q)
  }
  if (is.na(diff_nlspi) | diff_nlspi >= 24) {
    nlspi <- download_nlspi()
    write_csv(nlspi, nlspi_path)
  }
  if (is.na(diff_nlspi_q) | diff_nlspi_q >= 24) {
    nlspi_q <- download_nlspi_q()
    write_csv(nlspi_q, nlspi_path_q)
  }
  if (is.na(diff_ibspi) | diff_ibspi >= 24) {
    ibspi <- download_ibspi()
    write_csv(ibspi, ibspi_path)
  }
  if (is.na(diff_nhpi) | diff_nhpi >= 24) {
    nhpi <- download_nhpi()
    write_csv(nhpi, nhpi_path)
  }
  if (is.na(diff_cuwri) | diff_cuwri >= 24) {
    cuwri <- download_cuwri()
    write_csv(cuwri, cuwri_path)
  }
  
}

update_dataset()

ippi <- read_csv(ippi_path)
rspi <- read_csv(rspi_path)
rspi_q <- read_csv(rspi_path_q)


aespi <- read_csv(aespi_path)
aspi <- read_csv(aspi_path)
cmspi <- read_csv(cmspi_path)
cimerlspi <- read_csv(cimerlspi_path)
cimerlspi_q <- read_csv(cimerlspi_path_q)

cospi <- read_csv(cospi_path)
crspi <- read_csv(crspi_path)
crspi_q <- read_csv(crspi_path_q)


f <- read_csv(f_path)
f_q <- read_csv(f_path_q)

rmpi <- read_csv(rmpi_path)
ipspi <- read_csv(ipspi_path)
wspi <- read_csv(wspi_path)
wspi_q <- read_csv(wspi_path_q)

mepi <- read_csv(mepi_path)
cppi <- read_csv(cppi_path)
fipi <- read_csv(fipi_path)
paspi <- read_csv(paspi_path)
cspi <- read_csv(cspi_path)
epspi <- read_csv(epspi_path)
taspi <- read_csv(taspi_path)
taspi_q <- read_csv(taspi_path_q)

nlspi <- read_csv(nlspi_path)
nlspi_q <- read_csv(nlspi_path_q)

ibspi <- read_csv(ibspi_path)
nhpi <- read_csv(nhpi_path)
cuwri <- read_csv(cuwri_path)


aespi_mega <- aespi %>% mutate(Survey = "AESPI") %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`,
                                                                          " (", `GEO`,") [AESPI] - NAICS")) %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  rename(Figure =`North American Industry Classification System (NAICS)`)


aespi_special <- aespi %>% mutate(Survey = "AESPI") %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`,
                                                                          " (", `GEO`,")")) %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  rename(NAICS =`North American Industry Classification System (NAICS)`)



aspi_mega <- aspi %>% mutate(Survey = "ASPI") %>%
  select(Survey, `Class of service`, REF_DATE, VALUE) %>%
  mutate(`Class of service` = paste0(`Class of service`," [ASPI] - class of service")) %>%
  mutate(`REF_DATE` = paste0(`REF_DATE`,"-12")) %>%
  rename(Figure =`Class of service`)



cimerlspi_mega <- cimerlspi %>% mutate(Survey = "CIMERLSPI") %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`," [CIMERLSPI] - NAICS (Monthly)"))%>%
  rename(Figure =`North American Industry Classification System (NAICS)`)

cimerlspi_mega_q <- cimerlspi_q %>% mutate(Survey = "CIMERLSPI") %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`," [CIMERLSPI] - NAICS (Quarterly)"))%>%
  rename(Figure =`North American Industry Classification System (NAICS)`)

cmspi_mega <- cmspi %>% mutate(Survey = "CMSPI") %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`," [CMSPI] - NAICS"))%>%
  rename(Figure =`North American Industry Classification System (NAICS)`)

cospi_mega <- cospi %>% mutate(Survey = "COSPI") %>%
  select(Survey, `Class of service`, REF_DATE, VALUE) %>%
  mutate(`Class of service` = paste0(`Class of service`," [COSPI] - Class of service"))%>%
  rename(Figure =`Class of service`)

cppi_mega <- cppi %>% mutate(Survey = "CPPI") %>%
  select(Survey, `Type of peripheral`, REF_DATE, VALUE) %>%
  mutate(`Type of peripheral` = paste0(`Type of peripheral`," [CPPI] - Peripheral"))%>%
  rename(Figure =`Type of peripheral`)

crspi_mega <- crspi %>% mutate(Survey = "CRSPI") %>%
  mutate(`Building Type` = paste0(`Building Type`, " (", `GEO`, ") [CRSPI] - Building Type (Monthly)")) %>%
  select(Survey, `Building Type`, REF_DATE, VALUE)%>%
  rename(Figure =`Building Type`)


crspi_mega_q <- crspi_q %>% mutate(Survey = "CRSPI") %>%
  mutate(`Building Type` = paste0(`Building Type`, " (", `GEO`, ") [CRSPI] - Building Type (Quarterly)")) %>%
  select(Survey, `Building Type`, REF_DATE, VALUE)%>%
  rename(Figure =`Building Type`)



crspi_special <- crspi %>% mutate(Survey = "CRSPI") %>%
  mutate(`Building Type` = paste0(`Building Type`, " (", `GEO`, ")")) %>%
  select(Survey, `Building Type`, REF_DATE, VALUE)

crspi_special_q <- crspi_q %>% mutate(Survey = "CRSPI") %>%
  mutate(`Building Type` = paste0(`Building Type`, " (", `GEO`, ")")) %>%
  select(Survey, `Building Type`, REF_DATE, VALUE)



cspi_mega <- cspi %>% mutate(Survey = "CSPI") %>%
  select(Survey, `Index`, REF_DATE, VALUE) %>%
  mutate(`Index` = paste0(`Index`," [CSPI]"))%>%
  rename(Figure =`Index`)

cuwri_mega <- cuwri %>% mutate(Survey = "CUWRI") %>%
  mutate(`Construction trades` = paste0(`Construction trades`, " (", `Type of wage indexes`,
                                        " - ", `GEO`,") [CUWRI] - Const. trades" )) %>%
  select(Survey, `Construction trades`, REF_DATE, VALUE)%>%
  rename(Figure =`Construction trades`)

cuwri_special <- cuwri %>% mutate(Survey = "CUWRI") %>%
  mutate(`Construction trades` = paste0(`Construction trades`, " (", `Type of wage indexes`,
                                        " - ", `GEO`,")" )) %>%
  select(Survey, `Construction trades`, REF_DATE, VALUE)


epspi_mega <- epspi %>% mutate(Survey = "EPSPI") %>%
  mutate(`Index` = paste0(`Index`, " (", `GEO`, ") [EPSPI]" )) %>%
  select(Survey, `Index`, REF_DATE, VALUE)%>%
  rename(Figure =`Index`)

epspi_special <- epspi %>% mutate(Survey = "EPSPI") %>%
  mutate(`Index` = paste0(`Index`, " (", `GEO`, ")" )) %>%
  select(Survey, `Index`, REF_DATE, VALUE)


fhmcfspi_mega <- f %>% mutate(Survey = "FHMCFSPI") %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`," [FHMCFSPI] - NAICS (Monthly)"))%>%
  rename(Figure =`North American Industry Classification System (NAICS)`)


fhmcfspi_mega_q <- f_q %>% mutate(Survey = "FHMCFSPI") %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`," [FHMCFSPI] - NAICS (Quarterly)"))%>%
  rename(Figure =`North American Industry Classification System (NAICS)`)


fipi_mega <- fipi %>% mutate(Survey = "FIPI") %>%
  mutate(`Price index` = paste0(`Price index`, " (", `GEO`, ") [FIPI] - Price index" )) %>%
  select(Survey, `Price index`, REF_DATE, VALUE)%>%
  rename(Figure =`Price index`)

fipi_special <- fipi %>% mutate(Survey = "FIPI") %>%
  mutate(`Price index` = paste0(`Price index`, " (", `GEO`, ")" )) %>%
  select(Survey, `Price index`, REF_DATE, VALUE)

ibspi_mega <- ibspi %>% mutate(Survey = "IBSPI") %>%
  select(Survey, REF_DATE, VALUE, GEO) %>%
  mutate(`GEO` = paste0(`GEO`," [IBSPI]")) %>%
  mutate(`REF_DATE` = paste0(`REF_DATE`,"-12"))%>%
  rename(Figure =`GEO`)

ippi_mega <- ippi %>% mutate(Survey = "IPPI") %>%
  select(Survey, `North American Product Classification System (NAPCS)`, REF_DATE, VALUE) %>%
  mutate(`North American Product Classification System (NAPCS)` = 
           paste0(`North American Product Classification System (NAPCS)`," [IPPI] - NAPCS"))%>%
  rename(Figure =`North American Product Classification System (NAPCS)`)

ipspi_mega <- ipspi %>% mutate(Survey = "IPSPI") %>%
  select(Survey, `North American Product Classification System (NAPCS)`, REF_DATE, VALUE) %>%
  mutate(`North American Product Classification System (NAPCS)` = paste0(`North American Product Classification System (NAPCS)`," [IPSPI] - NAPCS")) %>%
  mutate(`REF_DATE` = paste0(`REF_DATE`,"-12"))%>%
  rename(Figure =`North American Product Classification System (NAPCS)`)

mepi_mega <- mepi %>% mutate(Survey = "MEPI") %>%
  mutate(`Commodity` = paste0(`Commodity`, " (", `Machinery and equipment, domestic and imported`, ") [MEPI] - Commodity" )) %>%
  select(Survey, `Commodity`, REF_DATE, VALUE)%>%
  rename(Figure =`Commodity`)

mepi_special <- mepi %>% mutate(Survey = "MEPI") %>%
  mutate(`Commodity` = paste0(`Commodity`, " (", `Machinery and equipment, domestic and imported`, ")" )) %>%
  select(Survey, `Commodity`, REF_DATE, VALUE)

nhpi_mega <- nhpi %>% mutate(Survey = "NHPI") %>%
  mutate(`New housing price indexes` = paste0(`New housing price indexes`, " (", `GEO`, ") [NHPI] - New housing price indexes" )) %>%
  select(Survey, `New housing price indexes`, REF_DATE, VALUE)%>%
  rename(Figure =`New housing price indexes`)

nhpi_special <- nhpi %>% mutate(Survey = "NHPI") %>%
  mutate(`New housing price indexes` = paste0(`New housing price indexes`, " (", `GEO`, ")" )) %>%
  select(Survey, `New housing price indexes`, REF_DATE, VALUE)


nlspi_mega <- nlspi %>% mutate(Survey = "NLSPI") %>%
  select(Survey, REF_DATE, VALUE, GEO) %>%
  mutate(`GEO` = paste0(`GEO`," [NLSPI] (Monthly)"))%>%
  rename(Figure =`GEO`)


nlspi_mega_q <- nlspi_q %>% mutate(Survey = "NLSPI") %>%
  mutate(`GEO` = paste0(`GEO`, " (", `Lending services price index` , ") [NLSPI] (Quarterly)")) %>% 
  select(Survey, GEO, REF_DATE, VALUE) %>%
  rename(Figure =`GEO`)

nlspi_special_q <- nlspi_q %>% mutate(Survey = "NLSPI") %>%
  mutate(`GEO` = paste0(`GEO`, " (", `Lending services price index` , ")" )) %>% 
  select(Survey, REF_DATE, VALUE, GEO)



paspi_mega <- paspi %>% mutate(Survey = "PASPI") %>%
  select(Survey, Sector, REF_DATE, VALUE) %>%
  mutate(`Sector` = paste0(`Sector`," [PASPI] - Sector")) %>%
  mutate(`REF_DATE` = paste0(`REF_DATE`,"-12"))%>%
  rename(Figure =`Sector`)

rmpi_mega <- rmpi %>% mutate(Survey = "RMPI") %>%
  select(Survey, `North American Product Classification System (NAPCS)`, REF_DATE, VALUE) %>%
  mutate(`North American Product Classification System (NAPCS)` = paste0(`North American Product Classification System (NAPCS)`," [RMPI] - NAPCS"))%>%
  rename(Figure =`North American Product Classification System (NAPCS)`)

rspi_mega <- rspi %>% mutate(Survey = "RSPI") %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`," [RSPI] - NAICS (Monthly)"))%>%
  rename(Figure =`North American Industry Classification System (NAICS)`)

rspi_mega_q <- rspi_q %>% mutate(Survey = "RSPI") %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`," [RSPI] - NAICS (Quarterly)"))%>%
  rename(Figure =`North American Industry Classification System (NAICS)`)

taspi_mega <- taspi %>% mutate(Survey = "TASPI") %>%
  mutate(`Client groups` = paste0(`Client groups`, " (", `GEO`, ") [TASPI] - Client groups (Monthly)" )) %>%
  select(Survey, `Client groups`, REF_DATE, VALUE)%>%
  rename(Figure =`Client groups`)

taspi_mega_q <- taspi_q %>% mutate(Survey = "TASPI") %>%
  mutate(`Client groups` = paste0(`Client groups`, " (", `GEO`, ") [TASPI] - Client groups (Quarterly)" )) %>%
  select(Survey, `Client groups`, REF_DATE, VALUE)%>%
  rename(Figure =`Client groups`)

taspi_special <- taspi %>% mutate(Survey = "TASPI") %>%
  mutate(`Client groups` = paste0(`Client groups`, " (", `GEO`, ")" )) %>%
  select(Survey, `Client groups`, REF_DATE, VALUE)

taspi_special_q <- taspi_q %>% mutate(Survey = "TASPI") %>%
  mutate(`Client groups` = paste0(`Client groups`, " (", `GEO`, ")" )) %>%
  select(Survey, `Client groups`, REF_DATE, VALUE)

wspi_mega <- wspi %>% mutate(Survey = "WSPI") %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`," [WSPI] - NAICS (Monthly)"))%>%
  rename(Figure =`North American Industry Classification System (NAICS)`)

wspi_mega_q <- wspi_q %>% mutate(Survey = "WSPI") %>%
  select(Survey, `North American Industry Classification System (NAICS)`, REF_DATE, VALUE) %>%
  mutate(`North American Industry Classification System (NAICS)` = paste0(`North American Industry Classification System (NAICS)`," [WSPI] - NAICS (Quarterly)"))%>%
  rename(Figure =`North American Industry Classification System (NAICS)`)

mega_dataset <- rbind(aespi_mega,
                      aspi_mega,
                      cimerlspi_mega,
                      cimerlspi_mega_q,
                      
                      cmspi_mega,
                      cospi_mega,
                      cppi_mega,
                      crspi_mega,
                      crspi_mega_q,
                      cspi_mega,
                      cuwri_mega,
                      epspi_mega,
                      fhmcfspi_mega,
                      fhmcfspi_mega_q,
                      
                      fipi_mega,
                      ibspi_mega,
                      ippi_mega,
                      ipspi_mega,
                      mepi_mega,
                      nhpi_mega,
                      nlspi_mega,
                      nlspi_mega_q,
                      
                      paspi_mega,
                      rmpi_mega,
                      rspi_mega,
                      rspi_mega_q,
                      
                      taspi_mega,
                      taspi_mega_q,
                      
                      wspi_mega,
                      wspi_mega_q)

