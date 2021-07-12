library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(leaflet)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(zoo)

# a <- c(shiny, 
#   tidyverse, 
#   ggplot2,
#   shinyjs,
#   shinythemes,
#   zoo,
#   plotly,
#   dplyr,
#   leaflet,
#   shinyWidgets,
#   shinyanimate,
#   shinydashboard)

source('preprocessing.R')

server <- function(input, output, session) {
  
  output$images <- renderUI({
    tags$div(img(src = "penguin.gif", height='200px', width='170px'), 
             img(src = "black.png", height='10px', width='200px'), 
             img(src = "movingplot.gif", height='400px', width='600px'), 
             img(src = "black.png", height='10px', width='200px'),
             img(src = "penguin.gif", height='200px', width='170px'))
  })
  
  # ANIMATION
  
  observe(addHoverAnim(session, 'animator', 'tada'))
  observe(addHoverAnim(session, 'animator2', 'pulse'))
  observe(addScrollAnim(session, 'animaespi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animaspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animcimerlspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animcmspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animcospi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animcppi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animcspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animcrspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animcspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animcuwri', 'fadeInRight'))
  observe(addScrollAnim(session, 'animepspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animf', 'fadeInRight'))
  observe(addScrollAnim(session, 'animfipi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animibspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animippi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animipspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animmepi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animnhpi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animnlspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animpaspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animrmpi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animrspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animtaspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animwspi', 'fadeInRight'))
  observe(addScrollAnim(session, 'animeditor', 'fadeInRight'))
  
  
  # IPPI
  
  output$plot <- renderPlotly({
    validate(
      need(input$prod != "", "Please select at least one entry.")
    )
    ippi_filt <- ippi %>% filter(`North American Product Classification System (NAPCS)`
                                 %in% input$prod) %>% 
      rename(`NAPCS`=`North American Product Classification System (NAPCS)`)
    ippi_filt$REF_DATE <- zoo::as.yearmon(ippi_filt$REF_DATE)
    
    graph <- ggplotly( 
      ggplot(ippi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAPCS`)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("IPPI Monthly Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(ippi_filt$REF_DATE),max(ippi_filt$REF_DATE),6/12))+
        scale_y_continuous(breaks = seq(min(ippi_filt$VALUE),max(ippi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(ippi_filt$REF_DATE), max(ippi_filt$REF_DATE), thickness = 0.1)
    graph
  })
  
  output$percentplot <- renderPlotly({
    validate(
      need(input$prod != "", "Please select at least one entry.")
    )
    ippi_filt <- ippi %>% 
      filter(`North American Product Classification System (NAPCS)` %in% input$prod) %>% 
      group_by(`North American Product Classification System (NAPCS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100) %>% 
      rename(`NAPCS`=`North American Product Classification System (NAPCS)`)
    
    ippi_filt$REF_DATE <- zoo::as.yearmon(ippi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(ippi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAPCS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("IPPI Monthly Percentage Change Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(ippi_filt$REF_DATE),max(ippi_filt$REF_DATE),6/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(ippi_filt$REF_DATE)[2], max(ippi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentplot <- renderPlotly({
    validate(
      need(input$prod != "", "Please select at least one entry.")
    )
    ippi_filt <- ippi %>% 
      filter(`North American Product Classification System (NAPCS)` %in% input$prod) %>% 
      group_by(`North American Product Classification System (NAPCS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100) %>% 
      rename(`NAPCS`=`North American Product Classification System (NAPCS)`)
    
    ippi_filt$REF_DATE <- zoo::as.yearmon(ippi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(ippi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAPCS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("IPPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(ippi_filt$REF_DATE),max(ippi_filt$REF_DATE),6/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(ippi_filt$REF_DATE)[13], max(ippi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  # RSPI
  
  output$plotrspi <- renderPlotly({
    validate(
      need(input$prodrspi != "", "Please select at least one entry.")
    )
    rspi_filt <- rspi %>% filter(`North American Industry Classification System (NAICS)`
                                 %in% input$prodrspi) %>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    rspi_filt$REF_DATE <- zoo::as.yearmon(rspi_filt$REF_DATE)
    
    graphrspi <- ggplotly( 
      ggplot(rspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("RSPI Monthly Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(rspi_filt$REF_DATE),max(rspi_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(rspi_filt$VALUE),max(rspi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(rspi_filt$REF_DATE), max(rspi_filt$REF_DATE), thickness = 0.1)
    graphrspi
  })
  
  output$percentrspi <- renderPlotly({
    validate(
      need(input$prodrspi != "", "Please select at least one entry.")
    )
    rspi_filt <- rspi %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodrspi) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)%>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    
    rspi_filt$REF_DATE <- zoo::as.yearmon(rspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(rspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("RSPI Monthly Percentage Change Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(rspi_filt$REF_DATE),max(rspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(rspi_filt$REF_DATE)[2], max(rspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentrspi <- renderPlotly({
    validate(
      need(input$prodrspi != "", "Please select at least one entry.")
    )
    rspi_filt <- rspi %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodrspi) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)%>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    
    rspi_filt$REF_DATE <- zoo::as.yearmon(rspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(rspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("RSPI Year-to-year Percentage Change,by Months, Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(rspi_filt$REF_DATE),max(rspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(rspi_filt$REF_DATE)[13], max(rspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  output$plotrspi_q <- renderPlotly({
    validate(
      need(input$prodrspi_q != "", "Please select at least one entry.")
    )
    rspi_filt <- rspi_q %>% filter(`North American Industry Classification System (NAICS)`
                                 %in% input$prodrspi_q) %>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    rspi_filt$REF_DATE <- zoo::as.yearmon(rspi_filt$REF_DATE)
    
    graphrspi <- ggplotly( 
      ggplot(rspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("RSPI Quarterly Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(rspi_filt$REF_DATE),max(rspi_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(rspi_filt$VALUE),max(rspi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(rspi_filt$REF_DATE), max(rspi_filt$REF_DATE), thickness = 0.1)
    graphrspi
  })
  
  output$percentrspi_q <- renderPlotly({
    validate(
      need(input$prodrspi_q != "", "Please select at least one entry.")
    )
    rspi_filt <- rspi_q %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodrspi_q) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)%>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    
    rspi_filt$REF_DATE <- zoo::as.yearmon(rspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(rspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("RSPI Quarterly Percentage Change Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(rspi_filt$REF_DATE),max(rspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(rspi_filt$REF_DATE)[2], max(rspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentrspi_q <- renderPlotly({
    validate(
      need(input$prodrspi_q != "", "Please select at least one entry.")
    )
    rspi_filt <- rspi_q %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodrspi_q) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100)%>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    
    rspi_filt$REF_DATE <- zoo::as.yearmon(rspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(rspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("RSPI Year-to-year Percentage Change, by Quarters, Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(rspi_filt$REF_DATE),max(rspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(rspi_filt$REF_DATE)[5], max(rspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  #AESPI
  
  output$plotaespi <- renderPlotly({
    validate(
      need(input$prodaespi != "", "Please select at least one entry.")
    )
    aespi_filt <- aespi_special%>% #filter(`Survey` %in% input$prodeditorsurvey) %>% 
      filter(`NAICS` %in% input$prodaespi)
    
    aespi_filt$REF_DATE <- zoo::as.yearmon(aespi_filt$REF_DATE)
    
    graphaespi <- ggplotly( 
      ggplot(aespi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("AESPI Quarterly Time Series (Reference Year: 2018)")+
        scale_x_yearmon(breaks = seq(min(aespi_filt$REF_DATE),max(aespi_filt$REF_DATE),3/12))+
        scale_y_continuous(breaks = seq(min(aespi_filt$VALUE),max(aespi_filt$VALUE),2))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(aespi_filt$REF_DATE), max(aespi_filt$REF_DATE), thickness = 0.1)
    graphaespi
    
    graphaespi
  })
  
  
  output$percentaespi <- renderPlotly({
    validate(
      need(input$prodaespi != "", "Please select at least one entry.")
    )
    
    aespi_filt <- aespi_special %>%
      filter(`NAICS` %in% input$prodaespi) %>%
      group_by(`NAICS`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    aespi_filt$REF_DATE <- zoo::as.yearmon(aespi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(aespi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("AESPI Quarterly Percentage Change Time Series (Reference Year: 2018)")+
        scale_x_yearmon(breaks = seq(min(aespi_filt$REF_DATE),max(aespi_filt$REF_DATE),3/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(aespi_filt$REF_DATE)[2], max(aespi_filt$REF_DATE), thickness = 0.1)
  })
  
  output$yearpercentaespi <- renderPlotly({
    validate(
      need(input$prodaespi != "", "Please select at least one entry.")
    )
    
    aespi_filt <- aespi_special %>%
      filter(`NAICS` %in% input$prodaespi) %>%
      group_by(`NAICS`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100)
    
    aespi_filt$REF_DATE <- zoo::as.yearmon(aespi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(aespi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("AESPI Year-to-year Percentage Change (by Quarters) Time Series (Reference Year: 2018)")+
        scale_x_yearmon(breaks = seq(min(aespi_filt$REF_DATE),max(aespi_filt$REF_DATE),3/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(aespi_filt$REF_DATE)[5], max(aespi_filt$REF_DATE), thickness = 0.1)
  })
  
  # ASPI
  
  output$plotaspi <- renderPlotly({
    validate(
      need(input$prodaspi != "", "Please select at least one entry.")
    )
    aspi_filt <- aspi %>% filter(`Class of service` %in% input$prodaspi)
    aspi_filt$REF_DATE <- as.numeric(aspi_filt$REF_DATE)

    graphaspi <- ggplotly(
      ggplot(aspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) +
        geom_line(aes(colour = `Class of service`)) +
        xlab("Date") +
        ylab("Index")+
        ggtitle("ASPI Annual Time Series (Reference Year: 2010)")+
        scale_x_continuous(breaks = seq(min(aspi_filt$REF_DATE),max(aspi_filt$REF_DATE),1))+
        #scale_x_discrete(breaks = seq(min(aspi_filt$REF_DATE),max(aspi_filt$REF_DATE),1))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5))) %>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(aspi_filt$REF_DATE), max(aspi_filt$REF_DATE), thickness = 0.1)
  })
  
  output$percentaspi <- renderPlotly({
    validate(
      need(input$prodaspi != "", "Please select at least one entry.")
    )
    aspi_filt <- aspi %>% 
      filter(`Class of service` %in% input$prodaspi) %>% 
      group_by(`Class of service`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    ggplotly( 
      ggplot(aspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Class of service`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("ASPI Annual Percentage Change Time Series (Reference Year: 2010)")+
        scale_x_continuous(breaks = seq(min(aspi_filt$REF_DATE),max(aspi_filt$REF_DATE),1))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(aspi$REF_DATE)[2], max(aspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  # CMSPI
  
  output$plotcmspi <- renderPlotly({
    validate(
      need(input$prodcmspi != "", "Please select at least one entry.")
    )
    cmspi_filt <- cmspi %>% filter(`North American Industry Classification System (NAICS)`
                                   %in% input$prodcmspi) %>% 
      rename(NAICS=`North American Industry Classification System (NAICS)`)
    
    cmspi_filt$REF_DATE <- zoo::as.yearmon(cmspi_filt$REF_DATE)
    
    graphcmspi <- ggplotly( 
      ggplot(cmspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("CMSPI Monthly Time Series (Reference Year: 2003)")+
        scale_x_yearmon(breaks = seq(min(cmspi_filt$REF_DATE),max(cmspi_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(cmspi_filt$VALUE),max(cmspi_filt$VALUE),11))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(cmspi_filt$REF_DATE), max(cmspi_filt$REF_DATE), thickness = 0.1)
    graphcmspi
  })
  
  
  output$percentcmspi <- renderPlotly({
    validate(
      need(input$prodcmspi != "", "Please select at least one entry.")
    )
    cmspi_filt <- cmspi %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodcmspi) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100) %>% 
      rename(NAICS=`North American Industry Classification System (NAICS)`)
    
    cmspi_filt$REF_DATE <- zoo::as.yearmon(cmspi_filt$REF_DATE)
    
    ggplotly(
      ggplot(cmspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CMSPI Monthly Percentage Change Time Series (Reference Year: 2003)")+
        scale_x_yearmon(breaks = seq(min(cmspi_filt$REF_DATE),max(cmspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cmspi_filt$REF_DATE)[2], max(cmspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentcmspi <- renderPlotly({
    validate(
      need(input$prodcmspi != "", "Please select at least one entry.")
    )
    cmspi_filt <- cmspi %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodcmspi) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100) %>% 
      rename(NAICS=`North American Industry Classification System (NAICS)`)
    
    cmspi_filt$REF_DATE <- zoo::as.yearmon(cmspi_filt$REF_DATE)
    
    ggplotly(
      ggplot(cmspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CMSPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2003)")+
        scale_x_yearmon(breaks = seq(min(cmspi_filt$REF_DATE),max(cmspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cmspi_filt$REF_DATE)[13], max(cmspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  # CIMERLSPI
  
  output$plotcimerlspi <- renderPlotly({
    validate(
      need(input$prodcimerlspi != "", "Please select at least one entry.")
    )
    cimerlspi_filt <- cimerlspi %>% filter(`North American Industry Classification System (NAICS)`
                                           %in% input$prodcimerlspi) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    cimerlspi_filt$REF_DATE <- zoo::as.yearmon(cimerlspi_filt$REF_DATE)
    
    graphcimerlspi <- ggplotly( 
      ggplot(cimerlspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("CIMERLSPI Monthly Time Series (Reference Year: 2007)")+
        scale_x_yearmon(breaks = seq(min(cimerlspi_filt$REF_DATE),max(cimerlspi_filt$REF_DATE),2/12))+
        #limits = c(min(cimerlspi_filt$REF_DATE), max(cimerlspi_filt$REF_DATE)))+
        scale_y_continuous(breaks = seq(min(cimerlspi_filt$VALUE), max(cimerlspi_filt$VALUE), 2))+
        #scale_x_date(date_breaks  ="2 months")+ # Sol1
        #scale_x_discrete(breaks = seq(min(cimerlspi_filt$REF_DATE),max(cimerlspi_filt$REF_DATE),1))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(cimerlspi_filt$REF_DATE), max(cimerlspi_filt$REF_DATE), thickness = 0.1)
    graphcimerlspi
  })
  
  
  output$percentcimerlspi <- renderPlotly({
    validate(
      need(input$prodcimerlspi != "", "Please select at least one entry.")
    )
    cimerlspi_filt <- cimerlspi %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodcimerlspi) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    
    cimerlspi_filt$REF_DATE <- zoo::as.yearmon(cimerlspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cimerlspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CIMERLSPI Monthly Percentage Change Time Series (Reference Year: 2007)")+
        scale_x_yearmon(breaks = seq(min(cimerlspi_filt$REF_DATE),max(cimerlspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cimerlspi_filt$REF_DATE)[2], max(cimerlspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentcimerlspi <- renderPlotly({
    validate(
      need(input$prodcimerlspi != "", "Please select at least one entry.")
    )
    cimerlspi_filt <- cimerlspi %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodcimerlspi) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    
    cimerlspi_filt$REF_DATE <- zoo::as.yearmon(cimerlspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cimerlspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CIMERLSPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2007)")+
        scale_x_yearmon(breaks = seq(min(cimerlspi_filt$REF_DATE),max(cimerlspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cimerlspi_filt$REF_DATE)[13], max(cimerlspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  
  # Quarterly
  
  
  
  output$plotcimerlspi_q <- renderPlotly({
    validate(
      need(input$prodcimerlspi_q != "", "Please select at least one entry.")
    )
    cimerlspi_filt <- cimerlspi_q %>% filter(`North American Industry Classification System (NAICS)`
                                           %in% input$prodcimerlspi_q) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    cimerlspi_filt$REF_DATE <- zoo::as.yearmon(cimerlspi_filt$REF_DATE)
    
    graphcimerlspi <- ggplotly( 
      ggplot(cimerlspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("CIMERLSPI Quarterly Time Series (Reference Year: 2007)")+
        scale_x_yearmon(breaks = seq(min(cimerlspi_filt$REF_DATE),max(cimerlspi_filt$REF_DATE),2/12))+
        #limits = c(min(cimerlspi_filt$REF_DATE), max(cimerlspi_filt$REF_DATE)))+
        scale_y_continuous(breaks = seq(min(cimerlspi_filt$VALUE), max(cimerlspi_filt$VALUE), 2))+
        #scale_x_date(date_breaks  ="2 months")+ # Sol1
        #scale_x_discrete(breaks = seq(min(cimerlspi_filt$REF_DATE),max(cimerlspi_filt$REF_DATE),1))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(cimerlspi_filt$REF_DATE), max(cimerlspi_filt$REF_DATE), thickness = 0.1)
    graphcimerlspi
  })
  
  
  output$percentcimerlspi_q <- renderPlotly({
    validate(
      need(input$prodcimerlspi_q != "", "Please select at least one entry.")
    )
    cimerlspi_filt <- cimerlspi_q %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodcimerlspi_q) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    
    cimerlspi_filt$REF_DATE <- zoo::as.yearmon(cimerlspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cimerlspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CIMERLSPI Quarterly Percentage Change Time Series (Reference Year: 2007)")+
        scale_x_yearmon(breaks = seq(min(cimerlspi_filt$REF_DATE),max(cimerlspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cimerlspi_filt$REF_DATE)[2], max(cimerlspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentcimerlspi_q <- renderPlotly({
    validate(
      need(input$prodcimerlspi_q != "", "Please select at least one entry.")
    )
    cimerlspi_filt <- cimerlspi_q %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodcimerlspi_q) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    
    cimerlspi_filt$REF_DATE <- zoo::as.yearmon(cimerlspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cimerlspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CIMERLSPI Year-to-year Percentage Change, by Quarters, Time Series (Reference Year: 2007)")+
        scale_x_yearmon(breaks = seq(min(cimerlspi_filt$REF_DATE),max(cimerlspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cimerlspi_filt$REF_DATE)[5], max(cimerlspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  # COSPI
  
  output$plotcospi <- renderPlotly({
    validate(
      need(input$prodcospi != "", "Please select at least one entry.")
    )
    cospi_filt <- cospi %>% filter(`Class of service`
                                   %in% input$prodcospi)
    cospi_filt$REF_DATE <- zoo::as.yearmon(cospi_filt$REF_DATE)
    
    graphcospi <- ggplotly( 
      ggplot(cospi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Class of service`)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("COSPI Quarterly Time Series (Reference Year: 2018)")+
        scale_x_yearmon(breaks = seq(min(cospi_filt$REF_DATE),max(cospi_filt$REF_DATE),3/12))+
        scale_y_continuous(breaks = seq(min(cospi_filt$VALUE),max(cospi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(cospi_filt$REF_DATE), max(cospi_filt$REF_DATE), thickness = 0.1)
    graphcospi
  })
  
  output$percentcospi <- renderPlotly({
    validate(
      need(input$prodcospi != "", "Please select at least one entry.")
    )
    cospi_filt <- cospi %>% 
      filter(`Class of service` %in% input$prodcospi) %>% 
      group_by(`Class of service`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    cospi_filt$REF_DATE <- zoo::as.yearmon(cospi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cospi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Class of service`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("COSPI Quarterly Percentage Change Time Series (Reference Year: 2018)")+
        scale_x_yearmon(breaks = seq(min(cospi_filt$REF_DATE),max(cospi_filt$REF_DATE),3/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cospi_filt$REF_DATE)[2], max(cospi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentcospi <- renderPlotly({
    validate(
      need(input$prodcospi != "", "Please select at least one entry.")
    )
    cospi_filt <- cospi %>% 
      filter(`Class of service` %in% input$prodcospi) %>% 
      group_by(`Class of service`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100)
    
    cospi_filt$REF_DATE <- zoo::as.yearmon(cospi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cospi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Class of service`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("COSPI Year-to-year Percentage Change, by Quarters, Time Series (Reference Year: 2018)")+
        scale_x_yearmon(breaks = seq(min(cospi_filt$REF_DATE),max(cospi_filt$REF_DATE),3/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cospi_filt$REF_DATE)[5], max(cospi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  # CRSPI
  
  
  output$plotcrspi <- renderPlotly({
    validate(
      need(input$prodcrspi != "", "Please select at least one entry.")
    )
    crspi_filt <- crspi_special %>% #filter(`Survey` %in% input$prodeditorsurvey) %>% 
      filter(`Building Type` %in% input$prodcrspi)
    
    crspi_filt$REF_DATE <- zoo::as.yearmon(crspi_filt$REF_DATE)
    
    graphcrspi <- ggplotly( 
      ggplot(crspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Building Type`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("CRSPI Monthly Time Series (Reference Year: 2012)")+
        scale_x_yearmon(breaks = seq(min(crspi_filt$REF_DATE),max(crspi_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(crspi_filt$VALUE),max(crspi_filt$VALUE),2))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(crspi_filt$REF_DATE), max(crspi_filt$REF_DATE), thickness = 0.1)
    graphcrspi
  })
  
  output$percentcrspi <- renderPlotly({
    validate(
      need(input$prodcrspi != "", "Please select at least one entry.")
    )
    crspi_filt <- crspi_special %>% 
      filter(`Building Type` %in% input$prodcrspi) %>%
      group_by(`Building Type`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    crspi_filt$REF_DATE <- zoo::as.yearmon(crspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(crspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Building Type`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CRSPI Monthly Percentage Change Time Series (Reference Year: 2012)")+
        scale_x_yearmon(breaks = seq(min(crspi_filt$REF_DATE),max(crspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(crspi_filt$REF_DATE)[2], max(crspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentcrspi <- renderPlotly({
    validate(
      need(input$prodcrspi != "", "Please select at least one entry.")
    )
    crspi_filt <- crspi_special %>% 
      filter(`Building Type` %in% input$prodcrspi) %>%
      group_by(`Building Type`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)
    
    crspi_filt$REF_DATE <- zoo::as.yearmon(crspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(crspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Building Type`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CRSPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2012)")+
        scale_x_yearmon(breaks = seq(min(crspi_filt$REF_DATE),max(crspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(crspi_filt$REF_DATE)[13], max(crspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  # Quarterly
  
  
  output$plotcrspi_q <- renderPlotly({
    validate(
      need(input$prodcrspi_q != "", "Please select at least one entry.")
    )
    crspi_filt <- crspi_special_q %>% #filter(`Survey` %in% input$prodeditorsurvey) %>% 
      filter(`Building Type` %in% input$prodcrspi_q)
    
    crspi_filt$REF_DATE <- zoo::as.yearmon(crspi_filt$REF_DATE)
    
    graphcrspi <- ggplotly( 
      ggplot(crspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Building Type`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("CRSPI Quarterly Time Series (Reference Year: 2012)")+
        scale_x_yearmon(breaks = seq(min(crspi_filt$REF_DATE),max(crspi_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(crspi_filt$VALUE),max(crspi_filt$VALUE),2))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(crspi_filt$REF_DATE), max(crspi_filt$REF_DATE), thickness = 0.1)
    graphcrspi
  })
  
  output$percentcrspi_q <- renderPlotly({
    validate(
      need(input$prodcrspi_q != "", "Please select at least one entry.")
    )
    crspi_filt <- crspi_special_q %>% 
      filter(`Building Type` %in% input$prodcrspi_q) %>%
      group_by(`Building Type`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    crspi_filt$REF_DATE <- zoo::as.yearmon(crspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(crspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Building Type`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CRSPI Quarterly Percentage Change Time Series (Reference Year: 2012)")+
        scale_x_yearmon(breaks = seq(min(crspi_filt$REF_DATE),max(crspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(crspi_filt$REF_DATE)[2], max(crspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentcrspi_q <- renderPlotly({
    validate(
      need(input$prodcrspi_q != "", "Please select at least one entry.")
    )
    crspi_filt <- crspi_special_q %>% 
      filter(`Building Type` %in% input$prodcrspi_q) %>%
      group_by(`Building Type`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100)
    
    crspi_filt$REF_DATE <- zoo::as.yearmon(crspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(crspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Building Type`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CRSPI Year-to-year Percentage Change, by Quarters, Time Series (Reference Year: 2012)")+
        scale_x_yearmon(breaks = seq(min(crspi_filt$REF_DATE),max(crspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(crspi_filt$REF_DATE)[5], max(crspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  
  
  # FHMCFSPI
  
  output$plotf <- renderPlotly({
    validate(
      need(input$prodf != "", "Please select at least one entry.")
    )
    f_filt <- f %>% filter(`North American Industry Classification System (NAICS)`
                           %in% input$prodf) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    
    f_filt$REF_DATE <- zoo::as.yearmon(f_filt$REF_DATE)
    
    graphf <- ggplotly( 
      ggplot(f_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("FHMCFSPI Monthly Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(f_filt$REF_DATE),max(f_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(f_filt$VALUE),max(f_filt$VALUE),3))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(f_filt$REF_DATE), max(f_filt$REF_DATE), thickness = 0.1)
    graphf
  })
  
  output$percentf <- renderPlotly({
    validate(
      need(input$prodf != "", "Please select at least one entry.")
    )
    f_filt <- f %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodf) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    
    f_filt$REF_DATE <- zoo::as.yearmon(f_filt$REF_DATE)
    
    ggplotly( 
      ggplot(f_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("FHMCFSPI Monthly Percentage Change Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(f_filt$REF_DATE),max(f_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(f_filt$REF_DATE)[2], max(f_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentf <- renderPlotly({
    validate(
      need(input$prodf != "", "Please select at least one entry.")
    )
    f_filt <- f %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodf) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    
    f_filt$REF_DATE <- zoo::as.yearmon(f_filt$REF_DATE)
    
    ggplotly( 
      ggplot(f_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("FHMCFSPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(f_filt$REF_DATE),max(f_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(f_filt$REF_DATE)[13], max(f_filt$REF_DATE), thickness = 0.1)
  })
  
  
  # Quarterly
  
  
  output$plotf_q <- renderPlotly({
    validate(
      need(input$prodf_q != "", "Please select at least one entry.")
    )
    f_filt <- f_q %>% filter(`North American Industry Classification System (NAICS)`
                           %in% input$prodf_q) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    
    f_filt$REF_DATE <- zoo::as.yearmon(f_filt$REF_DATE)
    
    graphf <- ggplotly( 
      ggplot(f_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("FHMCFSPI Quarterly Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(f_filt$REF_DATE),max(f_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(f_filt$VALUE),max(f_filt$VALUE),3))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(f_filt$REF_DATE), max(f_filt$REF_DATE), thickness = 0.1)
    graphf
  })
  
  output$percentf_q <- renderPlotly({
    validate(
      need(input$prodf_q != "", "Please select at least one entry.")
    )
    f_filt <- f_q %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodf_q) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    
    f_filt$REF_DATE <- zoo::as.yearmon(f_filt$REF_DATE)
    
    ggplotly( 
      ggplot(f_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("FHMCFSPI Quarterly Percentage Change Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(f_filt$REF_DATE),max(f_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(f_filt$REF_DATE)[2], max(f_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentf_q <- renderPlotly({
    validate(
      need(input$prodf_q != "", "Please select at least one entry.")
    )
    f_filt <- f_q %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodf_q) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100) %>% 
      rename(`NAICS`=`North American Industry Classification System (NAICS)`)
    
    f_filt$REF_DATE <- zoo::as.yearmon(f_filt$REF_DATE)
    
    ggplotly( 
      ggplot(f_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("FHMCFSPI Year-to-year Percentage Change, by Quarters, Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(f_filt$REF_DATE),max(f_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(f_filt$REF_DATE)[5], max(f_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  
  
  # RMPI
  
  output$plotrmpi <- renderPlotly({
    validate(
      need(input$prodrmpi != "", "Please select at least one entry.")
    )
    rmpi_filt <- rmpi %>% filter(`North American Product Classification System (NAPCS)`
                                 %in% input$prodrmpi) %>% 
      rename(`NAPCS`=`North American Product Classification System (NAPCS)`)
    rmpi_filt$REF_DATE <- zoo::as.yearmon(rmpi_filt$REF_DATE)
    
    graphrmpi <- ggplotly( 
      ggplot(rmpi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAPCS`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("RMPI Monthly Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(rmpi_filt$REF_DATE),max(rmpi_filt$REF_DATE),5/12))+
        scale_y_continuous(breaks = seq(min(rmpi_filt$VALUE),max(rmpi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(rmpi_filt$REF_DATE), max(rmpi_filt$REF_DATE), thickness = 0.1)
    graphrmpi
  })
  
  output$percentrmpi <- renderPlotly({
    validate(
      need(input$prodrmpi != "", "Please select at least one entry.")
    )
    rmpi_filt <- rmpi %>% 
      filter(`North American Product Classification System (NAPCS)` %in% input$prodrmpi) %>% 
      group_by(`North American Product Classification System (NAPCS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)%>% 
      rename(`NAPCS`=`North American Product Classification System (NAPCS)`)
    
    rmpi_filt$REF_DATE <- zoo::as.yearmon(rmpi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(rmpi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAPCS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("RMPI Monthly Percentage Change Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(rmpi_filt$REF_DATE),max(rmpi_filt$REF_DATE),5/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(rmpi_filt$REF_DATE)[2], max(rmpi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentrmpi <- renderPlotly({
    validate(
      need(input$prodrmpi != "", "Please select at least one entry.")
    )
    rmpi_filt <- rmpi %>% 
      filter(`North American Product Classification System (NAPCS)` %in% input$prodrmpi) %>% 
      group_by(`North American Product Classification System (NAPCS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)%>% 
      rename(`NAPCS`=`North American Product Classification System (NAPCS)`)
    
    rmpi_filt$REF_DATE <- zoo::as.yearmon(rmpi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(rmpi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAPCS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("RMPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(rmpi_filt$REF_DATE),max(rmpi_filt$REF_DATE),5/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(rmpi_filt$REF_DATE)[13], max(rmpi_filt$REF_DATE), thickness = 0.1)
  })
  
  # IPSPI
  
  output$plotipspi <- renderPlotly({
    validate(
      need(input$prodipspi != "", "Please select at least one entry.")
    )
    ipspi_filt <- ipspi %>% filter(`North American Product Classification System (NAPCS)`
                                   %in% input$prodipspi)%>% 
      rename(`NAPCS`=`North American Product Classification System (NAPCS)`)
    ipspi_filt$REF_DATE = as.numeric(ipspi_filt$REF_DATE)
    
    graphipspi <- ggplotly( 
      ggplot(ipspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAPCS`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("IPSPI Annual Time Series (Reference Year: 2015)")+
        scale_x_continuous(breaks = seq(min(ipspi_filt$REF_DATE),max(ipspi_filt$REF_DATE),1))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(ipspi_filt$REF_DATE), max(ipspi_filt$REF_DATE), thickness = 0.1)
    graphipspi
  })
  
  output$percentipspi <- renderPlotly({
    validate(
      need(input$prodipspi != "", "Please select at least one entry.")
    )
    ipspi_filt <- ipspi %>% 
      filter(`North American Product Classification System (NAPCS)` %in% input$prodipspi) %>% 
      group_by(`North American Product Classification System (NAPCS)`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)%>% 
      rename(`NAPCS`=`North American Product Classification System (NAPCS)`)
    
    ggplotly( 
      ggplot(ipspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAPCS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("IPSPI Annual Percentage Change Time Series (Reference Year: 2015)")+
        scale_x_continuous(breaks = seq(min(ipspi_filt$REF_DATE),max(ipspi_filt$REF_DATE),1))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(ipspi_filt$REF_DATE)[2], max(ipspi_filt$REF_DATE), thickness = 0.1)
  })
  
  # WSPI
  
  output$plotwspi <- renderPlotly({
    validate(
      need(input$prodwspi != "", "Please select at least one entry.")
    )
    wspi_filt <- wspi %>% filter(`North American Industry Classification System (NAICS)`
                                 %in% input$prodwspi)%>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    wspi_filt$REF_DATE <- zoo::as.yearmon(wspi_filt$REF_DATE)
    
    graphwspi <- ggplotly( 
      ggplot(wspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("WSPI Monthly Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(wspi_filt$REF_DATE),max(wspi_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(wspi_filt$VALUE),max(wspi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(wspi_filt$REF_DATE), max(wspi_filt$REF_DATE), thickness = 0.1)
    graphwspi
  })
  
  output$percentwspi <- renderPlotly({
    validate(
      need(input$prodwspi != "", "Please select at least one entry.")
    )
    wspi_filt <- wspi %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodwspi) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)%>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    
    wspi_filt$REF_DATE <- zoo::as.yearmon(wspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(wspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("WSPI Monthly Percentage Change Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(wspi_filt$REF_DATE),max(wspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(wspi_filt$REF_DATE)[2], max(wspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentwspi <- renderPlotly({
    validate(
      need(input$prodwspi != "", "Please select at least one entry.")
    )
    wspi_filt <- wspi %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodwspi) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)%>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    
    wspi_filt$REF_DATE <- zoo::as.yearmon(wspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(wspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("WSPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(wspi_filt$REF_DATE),max(wspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(wspi_filt$REF_DATE)[13], max(wspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  # Quarterly
  
  
  output$plotwspi_q <- renderPlotly({
    validate(
      need(input$prodwspi_q != "", "Please select at least one entry.")
    )
    wspi_filt <- wspi_q %>% filter(`North American Industry Classification System (NAICS)`
                                 %in% input$prodwspi_q)%>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    wspi_filt$REF_DATE <- zoo::as.yearmon(wspi_filt$REF_DATE)
    
    graphwspi <- ggplotly( 
      ggplot(wspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("WSPI Quarterly Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(wspi_filt$REF_DATE),max(wspi_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(wspi_filt$VALUE),max(wspi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(wspi_filt$REF_DATE), max(wspi_filt$REF_DATE), thickness = 0.1)
    graphwspi
  })
  
  output$percentwspi_q <- renderPlotly({
    validate(
      need(input$prodwspi_q != "", "Please select at least one entry.")
    )
    wspi_filt <- wspi_q %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodwspi_q) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)%>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    
    wspi_filt$REF_DATE <- zoo::as.yearmon(wspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(wspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("WSPI Quarterly Percentage Change Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(wspi_filt$REF_DATE),max(wspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(wspi_filt$REF_DATE)[2], max(wspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentwspi_q <- renderPlotly({
    validate(
      need(input$prodwspi_q != "", "Please select at least one entry.")
    )
    wspi_filt <- wspi_q %>% 
      filter(`North American Industry Classification System (NAICS)` %in% input$prodwspi_q) %>% 
      group_by(`North American Industry Classification System (NAICS)`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100)%>% 
      rename(`NAICS` =`North American Industry Classification System (NAICS)` )
    
    wspi_filt$REF_DATE <- zoo::as.yearmon(wspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(wspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `NAICS`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("WSPI Year-to-year Percentage Change, by Quarters, Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(wspi_filt$REF_DATE),max(wspi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(wspi_filt$REF_DATE)[5], max(wspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  # MEPI
  
  output$plotmepi <- renderPlotly({
    validate(
      need(input$prodmepi != "", "Please select at least one entry.")
    )
    
    mepi_filt <- mepi_special %>% filter(Commodity
                                         %in% input$prodmepi)
    
    mepi_filt$REF_DATE <- zoo::as.yearmon(mepi_filt$REF_DATE)
    
    graphmepi <- ggplotly( 
      ggplot(mepi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = Commodity)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("MEPI Quarterly Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(mepi_filt$REF_DATE),max(mepi_filt$REF_DATE),3/12))+
        scale_y_continuous(breaks = seq(min(mepi_filt$VALUE),max(mepi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(mepi_filt$REF_DATE), max(mepi_filt$REF_DATE), thickness = 0.1)
    graphmepi
    
  })
  
  output$percentmepi <- renderPlotly({
    validate(
      need(input$prodmepi != "", "Please select at least one entry.")
    )
    mepi_filt <- mepi_special %>% 
      filter(`Commodity` %in% input$prodmepi) %>% 
      group_by(Commodity) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    mepi_filt$REF_DATE <- zoo::as.yearmon(mepi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(mepi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Commodity`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("MEPI Quarterly Percentage Change Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(mepi_filt$REF_DATE),max(mepi_filt$REF_DATE),3/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(mepi_filt$REF_DATE)[2], max(mepi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentmepi <- renderPlotly({
    validate(
      need(input$prodmepi != "", "Please select at least one entry.")
    )
    mepi_filt <- mepi_special %>% 
      filter(`Commodity` %in% input$prodmepi) %>% 
      group_by(Commodity) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100)
    
    mepi_filt$REF_DATE <- zoo::as.yearmon(mepi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(mepi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Commodity`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("MEPI Year-to-year Percentage Change, by Quarters, Time Series (Reference Year: 2010)")+
        scale_x_yearmon(breaks = seq(min(mepi_filt$REF_DATE),max(mepi_filt$REF_DATE),3/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(mepi_filt$REF_DATE)[5], max(mepi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  
  # CPPI
  
  output$plotcppi <- renderPlotly({
    validate(
      need(input$prodcppi != "", "Please select at least one entry.")
    )
    cppi_filt <- cppi %>% filter(`Type of peripheral`
                                 %in% input$prodcppi)
    cppi_filt$REF_DATE <- zoo::as.yearmon(cppi_filt$REF_DATE)
    
    graphcppi <- ggplotly( 
      ggplot(cppi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Type of peripheral`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("CPPI Monthly Time Series (Reference Year: 2015)")+
        scale_x_yearmon(breaks = seq(min(cppi_filt$REF_DATE),max(cppi_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(cppi_filt$VALUE),max(cppi_filt$VALUE),3))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(cppi_filt$REF_DATE), max(cppi_filt$REF_DATE), thickness = 0.1)
    graphcppi
  })
  
  output$percentcppi <- renderPlotly({
    validate(
      need(input$prodcppi != "", "Please select at least one entry.")
    )
    cppi_filt <- cppi %>% 
      filter(`Type of peripheral` %in% input$prodcppi) %>% 
      group_by(`Type of peripheral`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    cppi_filt$REF_DATE <- zoo::as.yearmon(cppi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cppi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Type of peripheral`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CPPI Monthly Percentage Change Time Series (Reference Year: 2015)")+
        scale_x_yearmon(breaks = seq(min(cppi_filt$REF_DATE),max(cppi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cppi_filt$REF_DATE)[2], max(cppi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentcppi <- renderPlotly({
    validate(
      need(input$prodcppi != "", "Please select at least one entry.")
    )
    cppi_filt <- cppi %>% 
      filter(`Type of peripheral` %in% input$prodcppi) %>% 
      group_by(`Type of peripheral`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)
    
    cppi_filt$REF_DATE <- zoo::as.yearmon(cppi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cppi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Type of peripheral`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CPPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2015)")+
        scale_x_yearmon(breaks = seq(min(cppi_filt$REF_DATE),max(cppi_filt$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cppi_filt$REF_DATE)[13], max(cppi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  # FIPI
  
  output$plotfipi <- renderPlotly({
    validate(
      need(input$prodfipi != "", "Please select at least one entry.")
    )
    fipi_filt <- fipi_special %>% filter(`Price index`
                                         %in% input$prodfipi)
    fipi_filt$REF_DATE <- zoo::as.yearmon(fipi_filt$REF_DATE)
    
    graphfipi <- ggplotly( 
      ggplot(fipi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Price index`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("FIPI Quarterly Time Series (Reference Year: 2012)")+
        scale_x_yearmon(breaks = seq(min(fipi_filt$REF_DATE),max(fipi_filt$REF_DATE),3/12))+
        scale_y_continuous(breaks = seq(min(fipi_filt$VALUE),max(fipi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(fipi_filt$REF_DATE), max(fipi_filt$REF_DATE), thickness = 0.1)
    graphfipi
  })
  
  output$percentfipi <- renderPlotly({
    validate(
      need(input$prodfipi != "", "Please select at least one entry.")
    )
    fipi_filt <- fipi_special %>% 
      filter(`Price index` %in% input$prodfipi) %>%
      group_by(`Price index`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    fipi_filt$REF_DATE <- zoo::as.yearmon(fipi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(fipi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Price index`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("FIPI Quarterly Percentage Change Time Series (Reference Year: 2012)")+
        scale_x_yearmon(breaks = seq(min(fipi_filt$REF_DATE),max(fipi_filt$REF_DATE),3/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(fipi_filt$REF_DATE)[2], max(fipi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentfipi <- renderPlotly({
    validate(
      need(input$prodfipi != "", "Please select at least one entry.")
    )
    fipi_filt <- fipi_special %>% 
      filter(`Price index` %in% input$prodfipi) %>%
      group_by(`Price index`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100)
    
    fipi_filt$REF_DATE <- zoo::as.yearmon(fipi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(fipi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Price index`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("FIPI Year-to-year Percentage Change, by Quarters, Time Series (Reference Year: 2012)")+
        scale_x_yearmon(breaks = seq(min(fipi_filt$REF_DATE),max(fipi_filt$REF_DATE),3/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(fipi_filt$REF_DATE)[5], max(fipi_filt$REF_DATE), thickness = 0.1)
  })
  
  # PASPI
  
  output$plotpaspi <- renderPlotly({
    validate(
      need(input$prodpaspi != "", "Please select at least one entry.")
    )
    paspi_filt <- paspi %>% filter(`Sector`
                                   %in% input$prodpaspi)
    paspi_filt$REF_DATE = as.numeric(paspi_filt$REF_DATE)
    
    graphpaspi <- ggplotly( 
      ggplot(paspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Sector`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("PASPI Annual Time Series (Reference Year: 2002)")+
        scale_x_continuous(breaks = seq(min(paspi_filt$REF_DATE),max(paspi_filt$REF_DATE),1))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(paspi_filt$REF_DATE), max(paspi_filt$REF_DATE), thickness = 0.1)
    graphpaspi
  })
  
  output$percentpaspi <- renderPlotly({
    validate(
      need(input$prodpaspi != "", "Please select at least one entry.")
    )
    paspi_filt <- paspi %>% 
      filter(`Sector` %in% input$prodpaspi) %>% 
      group_by(`Sector`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    ggplotly( 
      ggplot(paspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Sector`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("PASPI Annual Percentage Change Time Series (Reference Year: 2002)")+
        scale_x_continuous(breaks = seq(min(paspi_filt$REF_DATE),max(paspi_filt$REF_DATE),1))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(paspi_filt$REF_DATE)[2], max(paspi_filt$REF_DATE), thickness = 0.1)
  })
  
  # CSPI
  
  output$plotcspi <- renderPlotly({
    validate(
      need(input$prodcspi != "", "Please select at least one entry.")
    )
    cspi_filt <- cspi %>% filter(`Index`
                                 %in% input$prodcspi)
    cspi_filt$REF_DATE <- zoo::as.yearmon(cspi_filt$REF_DATE)
    
    graphcspi <- ggplotly( 
      ggplot(cspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Index`)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("CSPI Monthly Time Series (Reference Year: 2011)")+
        scale_x_yearmon(breaks = seq(min(cspi_filt$REF_DATE),max(cspi_filt$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(cspi_filt$VALUE),max(cspi_filt$VALUE),2))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(cspi_filt$REF_DATE), max(cspi_filt$REF_DATE), thickness = 0.1)
    graphcspi
  })
  
  
  output$percentcspi <- renderPlotly({
    validate(
      need(input$prodcspi != "", "Please select at least one entry.")
    )
    cspi_filt <- cspi %>% 
      filter(`Index` %in% input$prodcspi) %>% 
      group_by(`Index`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    cspi_filt$REF_DATE <- zoo::as.yearmon(cspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Index`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CSPI Monthly Percentage Change Time Series (Reference Year: 2011)")+
        scale_x_yearmon(breaks = seq(min(cspi_filt$REF_DATE),max(cspi_filt$REF_DATE),2/12))+
        #scale_y_continuous(breaks = seq(min(cspi_filt$pct_change),max(cspi_filt$pct_change),0.02))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cspi_filt$REF_DATE)[2], max(cspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentcspi <- renderPlotly({
    validate(
      need(input$prodcspi != "", "Please select at least one entry.")
    )
    cspi_filt <- cspi %>% 
      filter(`Index` %in% input$prodcspi) %>% 
      group_by(`Index`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)
    
    cspi_filt$REF_DATE <- zoo::as.yearmon(cspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Index`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CSPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2011)")+
        scale_x_yearmon(breaks = seq(min(cspi_filt$REF_DATE),max(cspi_filt$REF_DATE),2/12))+
        #scale_y_continuous(breaks = seq(min(cspi_filt$pct_change),max(cspi_filt$pct_change),0.02))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cspi_filt$REF_DATE)[13], max(cspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  
  # EPSPI
  
  output$plotepspi <- renderPlotly({
    validate(
      need(input$prodepspi != "", "Please select at least one entry.")
    )
    
    epspi_filt <- epspi_special%>% #filter(`Survey` %in% input$prodeditorsurvey) %>% 
      filter(`Index` %in% input$prodepspi)
    epspi_filt$REF_DATE <- zoo::as.yearmon(epspi_filt$REF_DATE)
    
    graphepspi <- ggplotly( 
      ggplot(epspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Index`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("EPSPI Monthly Time Series (Reference Year: 2014)")+
        scale_x_yearmon(breaks = seq(min(epspi_filt$REF_DATE),max(epspi_filt$REF_DATE),4/12))+
        scale_y_continuous(breaks = seq(min(epspi_filt$VALUE),max(epspi_filt$VALUE),4))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(epspi_filt$REF_DATE), max(epspi_filt$REF_DATE), thickness = 0.1)
    graphepspi
  })
  
  output$percentepspi <- renderPlotly({
    validate(
      need(input$prodepspi != "", "Please select at least one entry.")
    )
    epspi_filt <- epspi_special %>% 
      filter(`Index` %in% input$prodepspi) %>%
      group_by(`Index`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    epspi_filt$REF_DATE <- zoo::as.yearmon(epspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(epspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Index`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("EPSPI Monthly Percentage Change Time Series (Reference Year: 2014)")+
        scale_x_yearmon(breaks = seq(min(epspi_filt$REF_DATE),max(epspi_filt$REF_DATE),4/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(epspi_filt$REF_DATE)[2], max(epspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentepspi <- renderPlotly({
    validate(
      need(input$prodepspi != "", "Please select at least one entry.")
    )
    epspi_filt <- epspi_special %>% 
      filter(`Index` %in% input$prodepspi) %>%
      group_by(`Index`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)
    
    epspi_filt$REF_DATE <- zoo::as.yearmon(epspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(epspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Index`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("EPSPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2014)")+
        scale_x_yearmon(breaks = seq(min(epspi_filt$REF_DATE),max(epspi_filt$REF_DATE),4/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(epspi_filt$REF_DATE)[13], max(epspi_filt$REF_DATE), thickness = 0.1)
  })

  
  # TASPI
  
  output$plottaspi <- renderPlotly({
    validate(
      need(input$prodtaspi != "", "Please select at least one entry.")
    )
    
    taspi_filt <- taspi_special %>% filter(`Client groups`
                                           %in% input$prodtaspi)
    taspi_filt$REF_DATE <- zoo::as.yearmon(taspi_filt$REF_DATE)
    
    graphtaspi <- ggplotly( 
      ggplot(taspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Client groups`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("TASPI Monthly Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(taspi_filt$REF_DATE),max(taspi_filt$REF_DATE),3/12))+
        scale_y_continuous(breaks = seq(min(taspi_filt$VALUE),max(taspi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(taspi_filt$REF_DATE), max(taspi_filt$REF_DATE), thickness = 0.1)
    graphtaspi
    
  })
  
  output$percenttaspi <- renderPlotly({
    validate(
      need(input$prodtaspi != "", "Please select at least one entry.")
    )
    
    taspi_filt <- taspi_special %>% 
      filter(`Client groups` %in% input$prodtaspi) %>%
      group_by(`Client groups`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    taspi_filt$REF_DATE <- zoo::as.yearmon(taspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(taspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Client groups`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("TASPI Monthly Percentage Change Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(taspi_filt$REF_DATE),max(taspi_filt$REF_DATE),6/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(taspi_filt$REF_DATE)[2], max(taspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercenttaspi <- renderPlotly({
    validate(
      need(input$prodtaspi != "", "Please select at least one entry.")
    )
    
    taspi_filt <- taspi_special %>% 
      filter(`Client groups` %in% input$prodtaspi) %>%
      group_by(`Client groups`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)
    
    taspi_filt$REF_DATE <- zoo::as.yearmon(taspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(taspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Client groups`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("TASPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(taspi_filt$REF_DATE),max(taspi_filt$REF_DATE),6/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(taspi_filt$REF_DATE)[13], max(taspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  
  output$plottaspi_q <- renderPlotly({
    validate(
      need(input$prodtaspi_q != "", "Please select at least one entry.")
    )
    
    taspi_filt <- taspi_special_q %>% filter(`Client groups`
                                           %in% input$prodtaspi_q)
    taspi_filt$REF_DATE <- zoo::as.yearmon(taspi_filt$REF_DATE)
    
    graphtaspi <- ggplotly( 
      ggplot(taspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Client groups`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("TASPI Quarterly Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(taspi_filt$REF_DATE),max(taspi_filt$REF_DATE),3/12))+
        scale_y_continuous(breaks = seq(min(taspi_filt$VALUE),max(taspi_filt$VALUE),8))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(taspi_filt$REF_DATE), max(taspi_filt$REF_DATE), thickness = 0.1)
    graphtaspi
    
  })
  
  output$percenttaspi_q <- renderPlotly({
    validate(
      need(input$prodtaspi_q != "", "Please select at least one entry.")
    )
    
    taspi_filt <- taspi_special_q %>% 
      filter(`Client groups` %in% input$prodtaspi_q) %>%
      group_by(`Client groups`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    taspi_filt$REF_DATE <- zoo::as.yearmon(taspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(taspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Client groups`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("TASPI Quarterly Percentage Change Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(taspi_filt$REF_DATE),max(taspi_filt$REF_DATE),6/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(taspi_filt$REF_DATE)[2], max(taspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercenttaspi_q <- renderPlotly({
    validate(
      need(input$prodtaspi_q != "", "Please select at least one entry.")
    )
    
    taspi_filt <- taspi_special_q %>% 
      filter(`Client groups` %in% input$prodtaspi_q) %>%
      group_by(`Client groups`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100)
    
    taspi_filt$REF_DATE <- zoo::as.yearmon(taspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(taspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Client groups`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("TASPI Year-to-year Percentage Change, by Quarters, Time Series (Reference Year: 2013)")+
        scale_x_yearmon(breaks = seq(min(taspi_filt$REF_DATE),max(taspi_filt$REF_DATE),6/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(taspi_filt$REF_DATE)[5], max(taspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  
  # NLSPI
  
  output$plotnlspi <- renderPlotly({
    validate(
      need(input$prodnlspi != "", "Please select at least one entry.")
    )
    nlspi_filt <- nlspi %>% filter(`GEO`
                                   %in% input$prodnlspi)
    nlspi_filt$REF_DATE <- zoo::as.yearmon(nlspi_filt$REF_DATE)
    
    graphnlspi <- ggplotly( 
      ggplot(nlspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `GEO`)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("NLSPI Monthly Time Series (Reference Year: 2011)")+
        scale_x_yearmon(breaks = seq(min(nlspi_filt$REF_DATE),max(nlspi_filt$REF_DATE),1/12))+
        scale_y_continuous(breaks = seq(min(nlspi_filt$VALUE),max(nlspi_filt$VALUE),6))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(nlspi_filt$REF_DATE), max(nlspi_filt$REF_DATE), thickness = 0.1)
    graphnlspi
  })
  
  output$percentnlspi <- renderPlotly({
    validate(
      need(input$prodnlspi != "", "Please select at least one entry.")
    )
    nlspi_filt <- nlspi %>% 
      filter(`GEO` %in% input$prodnlspi) %>% 
      group_by(`GEO`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    nlspi_filt$REF_DATE <- zoo::as.yearmon(nlspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(nlspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `GEO`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("NLSPI Monthly Percentage Change Time Series (Reference Year: 2011)")+
        scale_x_yearmon(breaks = seq(min(nlspi_filt$REF_DATE),max(nlspi_filt$REF_DATE),1/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(nlspi_filt$REF_DATE)[2], max(nlspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentnlspi <- renderPlotly({
    validate(
      need(input$prodnlspi != "", "Please select at least one entry.")
    )
    nlspi_filt <- nlspi %>% 
      filter(`GEO` %in% input$prodnlspi) %>% 
      group_by(`GEO`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)
    
    nlspi_filt$REF_DATE <- zoo::as.yearmon(nlspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(nlspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `GEO`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("NLSPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2011)")+
        scale_x_yearmon(breaks = seq(min(nlspi_filt$REF_DATE),max(nlspi_filt$REF_DATE),1/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(nlspi_filt$REF_DATE)[13], max(nlspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  # Quarterly
  
  
  output$plotnlspi_q <- renderPlotly({
    validate(
      need(input$prodnlspi_q != "", "Please select at least one entry.")
    )
    nlspi_filt <- nlspi_special_q %>% filter(`GEO`
                                   %in% input$prodnlspi_q)
    nlspi_filt$REF_DATE <- zoo::as.yearmon(nlspi_filt$REF_DATE)
    
    graphnlspi <- ggplotly( 
      ggplot(nlspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `GEO`)) + 
        xlab("Date") + 
        ylab("Index")+
        ggtitle("NLSPI Quarterly Time Series (Reference Year: 2011)")+
        scale_x_yearmon(breaks = seq(min(nlspi_filt$REF_DATE),max(nlspi_filt$REF_DATE),1/12))+
        scale_y_continuous(breaks = seq(min(nlspi_filt$VALUE),max(nlspi_filt$VALUE),6))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(nlspi_filt$REF_DATE), max(nlspi_filt$REF_DATE), thickness = 0.1)
    graphnlspi
  })
  
  output$percentnlspi_q <- renderPlotly({
    validate(
      need(input$prodnlspi_q != "", "Please select at least one entry.")
    )
    nlspi_filt <- nlspi_special_q %>% 
      filter(`GEO` %in% input$prodnlspi_q) %>% 
      group_by(`GEO`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    nlspi_filt$REF_DATE <- zoo::as.yearmon(nlspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(nlspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `GEO`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("NLSPI Quarterly Percentage Change Time Series (Reference Year: 2011)")+
        scale_x_yearmon(breaks = seq(min(nlspi_filt$REF_DATE),max(nlspi_filt$REF_DATE),1/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(nlspi_filt$REF_DATE)[2], max(nlspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentnlspi_q <- renderPlotly({
    validate(
      need(input$prodnlspi_q != "", "Please select at least one entry.")
    )
    nlspi_filt <- nlspi_special_q %>% 
      filter(`GEO` %in% input$prodnlspi_q) %>% 
      group_by(`GEO`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE, 4) - 1) * 100)
    
    nlspi_filt$REF_DATE <- zoo::as.yearmon(nlspi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(nlspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `GEO`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("NLSPI Year-to-year Percentage Change, by Quarters, Time Series (Reference Year: 2011)")+
        scale_x_yearmon(breaks = seq(min(nlspi_filt$REF_DATE),max(nlspi_filt$REF_DATE),1/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(nlspi_filt$REF_DATE)[5], max(nlspi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  
  
  # IBSPI
  
  output$plotibspi <- renderPlotly({
    validate(
      need(input$prodibspi != "", "Please select at least one entry.")
    )
    ibspi_filt <- ibspi %>% filter(`GEO`
                                   %in% input$prodibspi)
    ibspi_filt$REF_DATE = as.numeric(ibspi_filt$REF_DATE)
    
    graphibspi <- ggplotly( 
      ggplot(ibspi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `GEO`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("IBSPI Annual Time Series (Reference Year: 2010)")+
        scale_x_continuous(breaks = seq(min(ibspi_filt$REF_DATE),max(ibspi_filt$REF_DATE),1))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(ibspi_filt$REF_DATE), max(ibspi_filt$REF_DATE), thickness = 0.1)
    graphibspi
  })
  
  output$percentibspi <- renderPlotly({
    validate(
      need(input$prodibspi != "", "Please select at least one entry.")
    )
    ibspi_filt <- ibspi %>% 
      filter(`GEO` %in% input$prodibspi) %>% 
      group_by(`GEO`) %>% 
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    ggplotly( 
      ggplot(ibspi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `GEO`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("IBSPI Annual Percentage Change Time Series (Reference Year: 2010)")+
        scale_x_continuous(breaks = seq(min(ibspi_filt$REF_DATE),max(ibspi_filt$REF_DATE),1))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(ibspi_filt$REF_DATE)[2], max(ibspi_filt$REF_DATE), thickness = 0.1)
  })
  
  # NHPI
  
  output$plotnhpi <- renderPlotly({
    validate(
      need(input$prodnhpi != "", "Please select at least one entry.")
    )
    
    nhpi_filt <- nhpi_special %>% filter(`New housing price indexes`
                                         %in% input$prodnhpi)#%>% 
    #mutate(`New housing price indexes` = paste0(`New housing price indexes`,"       "))
    nhpi_filt$REF_DATE <- zoo::as.yearmon(nhpi_filt$REF_DATE)
    
    graphnhpi <- ggplotly( 
      ggplot(nhpi_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `New housing price indexes`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("NHPI Monthly Time Series (Reference Year: 2016)")+
        scale_x_yearmon(breaks = seq(min(nhpi_filt$REF_DATE),max(nhpi_filt$REF_DATE),4/12))+
        scale_y_continuous(breaks = seq(min(nhpi_filt$VALUE),max(nhpi_filt$VALUE),6))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(nhpi_filt$REF_DATE), max(nhpi_filt$REF_DATE), thickness = 0.1)
    graphnhpi
    
  })
  
  output$percentnhpi <- renderPlotly({
    validate(
      need(input$prodnhpi != "", "Please select at least one entry.")
    )
    nhpi_filt <- nhpi_special %>% 
      filter(`New housing price indexes` %in% input$prodnhpi) %>%
      group_by(`New housing price indexes`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)#%>% 
    #mutate(`New housing price indexes` = paste0(`New housing price indexes`,"       "))
    
    nhpi_filt$REF_DATE <- zoo::as.yearmon(nhpi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(nhpi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `New housing price indexes`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("NHPI Monthly Percentage Change Time Series (Reference Year: 2016)")+
        scale_x_yearmon(breaks = seq(min(nhpi_filt$REF_DATE),max(nhpi_filt$REF_DATE),4/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(nhpi_filt$REF_DATE)[2], max(nhpi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  output$yearpercentnhpi <- renderPlotly({
    validate(
      need(input$prodnhpi != "", "Please select at least one entry.")
    )
    nhpi_filt <- nhpi_special %>% 
      filter(`New housing price indexes` %in% input$prodnhpi) %>%
      group_by(`New housing price indexes`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)#%>% 
    #mutate(`New housing price indexes` = paste0(`New housing price indexes`,"       "))
    
    nhpi_filt$REF_DATE <- zoo::as.yearmon(nhpi_filt$REF_DATE)
    
    ggplotly( 
      ggplot(nhpi_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `New housing price indexes`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("NHPI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2016)")+
        scale_x_yearmon(breaks = seq(min(nhpi_filt$REF_DATE),max(nhpi_filt$REF_DATE),4/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(nhpi_filt$REF_DATE)[13], max(nhpi_filt$REF_DATE), thickness = 0.1)
  })
  
  
  
  # CUWRI
  
  output$plotcuwri <- renderPlotly({
    validate(
      need(input$prodcuwri != "", "Please select at least one entry.")
    )
    
    cuwri_filt <- cuwri_special%>% #filter(`Survey` %in% input$prodeditorsurvey) %>% 
      filter(`Construction trades` %in% input$prodcuwri)
    
    cuwri_filt$REF_DATE <- zoo::as.yearmon(cuwri_filt$REF_DATE)
    
    graphcuwri <- ggplotly( 
      ggplot(cuwri_filt, aes( y=VALUE, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Construction trades`)) + 
        xlab("Date") + 
        ylab("Index")+ 
        ggtitle("CUWRI Monthly Time Series (Reference Year: 2015)")+
        scale_x_yearmon(breaks = seq(min(cuwri_filt$REF_DATE),max(cuwri_filt$REF_DATE),4/12))+
        scale_y_continuous(breaks = seq(min(cuwri_filt$VALUE),max(cuwri_filt$VALUE),4))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(min(cuwri_filt$REF_DATE), max(cuwri_filt$REF_DATE), thickness = 0.1)
    graphcuwri
  })
  
  output$percentcuwri <- renderPlotly({
    validate(
      need(input$prodcuwri != "", "Please select at least one entry.")
    )
    
    cuwri_filt <- cuwri_special %>% 
      filter(`Construction trades` %in% input$prodcuwri) %>% 
      group_by(`Construction trades`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    cuwri_filt$REF_DATE <- zoo::as.yearmon(cuwri_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cuwri_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Construction trades`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CUWRI Monthly Percentage Change Time Series (Reference Year: 2015)")+
        scale_x_yearmon(breaks = seq(min(cuwri_filt$REF_DATE),max(cuwri_filt$REF_DATE),4/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cuwri_filt$REF_DATE)[2], max(cuwri_filt$REF_DATE), thickness = 0.1)
  })
  
  output$yearpercentcuwri <- renderPlotly({
    validate(
      need(input$prodcuwri != "", "Please select at least one entry.")
    )
    
    cuwri_filt <- cuwri_special %>% 
      filter(`Construction trades` %in% input$prodcuwri) %>% 
      group_by(`Construction trades`) %>%
      mutate(pct_change = (VALUE/lag(VALUE, 12) - 1) * 100)
    
    cuwri_filt$REF_DATE <- zoo::as.yearmon(cuwri_filt$REF_DATE)
    
    ggplotly( 
      ggplot(cuwri_filt, aes( y=pct_change, x=REF_DATE, group=1)) + 
        geom_line(aes(colour = `Construction trades`)) + 
        xlab("Date") + 
        ylab("Percentage (%)")+
        ggtitle("CUWRI Year-to-year Percentage Change, by Months, Time Series (Reference Year: 2015)")+
        scale_x_yearmon(breaks = seq(min(cuwri_filt$REF_DATE),max(cuwri_filt$REF_DATE),4/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>% 
      rangeslider(unique(cuwri_filt$REF_DATE)[13], max(cuwri_filt$REF_DATE), thickness = 0.1)
  })
  
  
  ### Download buttons
  
  output$dlaespi <- downloadHandler(
    filename = function() {
      paste("aespi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(aespi, file, row.names = FALSE)
    }
  )
  
  output$dlaspi <- downloadHandler(
    filename = function() {
      paste("aspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(aspi, file, row.names = FALSE)
    }
  )
  
  output$dlcimerlspi <- downloadHandler(
    filename = function() {
      paste("cimerlspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cimerlspi, file, row.names = FALSE)
    }
  )
  
  output$dlcmspi <- downloadHandler(
    filename = function() {
      paste("cmspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cmspi, file, row.names = FALSE)
    }
  )
  
  output$dlcospi <- downloadHandler(
    filename = function() {
      paste("cospi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cospi, file, row.names = FALSE)
    }
  )
  
  output$dlcppi <- downloadHandler(
    filename = function() {
      paste("cppi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cppi, file, row.names = FALSE)
    }
  )
  
  output$dlcrspi <- downloadHandler(
    filename = function() {
      paste("crspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(crspi, file, row.names = FALSE)
    }
  )
  
  output$dlcspi <- downloadHandler(
    filename = function() {
      paste("cspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cspi, file, row.names = FALSE)
    }
  )
  
  output$dlepspi <- downloadHandler(
    filename = function() {
      paste("epspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(epspi, file, row.names = FALSE)
    }
  )
  
  output$dlf <- downloadHandler(
    filename = function() {
      paste("fhmcfspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(f, file, row.names = FALSE)
    }
  )
  
  output$dlfipi <- downloadHandler(
    filename = function() {
      paste("fipi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fipi, file, row.names = FALSE)
    }
  )
  
  output$dlibspi <- downloadHandler(
    filename = function() {
      paste("ibspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ibspi, file, row.names = FALSE)
    }
  )
  
  output$dlippi <- downloadHandler(
    filename = function() {
      paste("ippi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ippi, file, row.names = FALSE)
    }
  )
  
  output$dlipspi <- downloadHandler(
    filename = function() {
      paste("ipspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ipspi, file, row.names = FALSE)
    }
  )
  
  output$dlmepi <- downloadHandler(
    filename = function() {
      paste("mepi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mepi, file, row.names = FALSE)
    }
  )
  
  output$dlnhpi <- downloadHandler(
    filename = function() {
      paste("nhpi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nhpi, file, row.names = FALSE)
    }
  )
  
  output$dlnlspi <- downloadHandler(
    filename = function() {
      paste("nlspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nlspi, file, row.names = FALSE)
    }
  )
  
  output$dlpaspi <- downloadHandler(
    filename = function() {
      paste("paspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(paspi, file, row.names = FALSE)
    }
  )
  
  output$dlrmpi <- downloadHandler(
    filename = function() {
      paste("rmpi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rmpi, file, row.names = FALSE)
    }
  )
  
  output$dlrspi <- downloadHandler(
    filename = function() {
      paste("rspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rspi, file, row.names = FALSE)
    }
  )
  
  output$dltaspi <- downloadHandler(
    filename = function() {
      paste("taspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(taspi, file, row.names = FALSE)
    }
  )
  
  output$dlwspi <- downloadHandler(
    filename = function() {
      paste("wspi", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(wspi, file, row.names = FALSE)
    }
  )
  
  output$dlcuwri <- downloadHandler(
    filename = function() {
      paste("cuwri", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cuwri, file, row.names = FALSE)
    }
  )
  
  
  # MEGA DATASET

  output$ploteditor <- renderPlotly({
    validate(
      need(input$prodeditor != "", "Please select at least one entry.")
    )
    mega_dataset <- mega_dataset %>% #filter(`Survey` %in% input$prodeditorsurvey) %>% 
      filter(`Figure` %in% input$prodeditor)
    mega_dataset$REF_DATE <- zoo::as.yearmon(mega_dataset$REF_DATE)
    
    graphmega_dataset <- ggplotly(
      ggplot(mega_dataset, aes( y=VALUE, x=REF_DATE, group=1)) +
        geom_line(aes(colour = `Figure`)) +
        xlab("Date") +
        ylab("Index")+
        ggtitle("Editor Time Series")+
        scale_x_yearmon(breaks = seq(min(mega_dataset$REF_DATE),max(mega_dataset$REF_DATE),2/12))+
        scale_y_continuous(breaks = seq(min(mega_dataset$VALUE),max(mega_dataset$VALUE),3))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>%
      rangeslider(min(mega_dataset$REF_DATE), max(mega_dataset$REF_DATE), thickness = 0.1)
    graphmega_dataset
  })
  
  output$percentmega_dataset <- renderPlotly({
    validate(
      need(input$prodeditor != "", "Please select at least one entry.")
    )
    mega_dataset <- mega_dataset %>%
      filter(`Figure` %in% input$prodeditor) %>%
      group_by(`Figure`) %>%
      mutate(pct_change = (VALUE/lag(VALUE) - 1) * 100)
    
    mega_dataset$REF_DATE <- zoo::as.yearmon(mega_dataset$REF_DATE)
    
    ggplotly(
      ggplot(mega_dataset, aes( y=pct_change, x=REF_DATE, group=1)) +
        geom_line(aes(colour = `Figure`)) +
        xlab("Date") +
        ylab("Percentage (%)")+
        ggtitle("Percentage Change Editor Time Series")+
        scale_x_yearmon(breaks = seq(min(mega_dataset$REF_DATE),max(mega_dataset$REF_DATE),2/12))+
        theme(axis.text.x=element_text(angle=90, hjust=1),
              plot.title = element_text(hjust = 0.5)))%>%
      layout(paper_bgcolor='#DFE6F8') %>%
      rangeslider(unique(mega_dataset$REF_DATE)[2], max(mega_dataset$REF_DATE), thickness = 0.1)
  })
}
