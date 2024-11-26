rm(list = ls())
library(shiny)
library(shinyjs)
library(plotly)
require(sqldf)
library(dplyr)
library(plyr)
library(scales)
library(shinyWidgets)
require(quantmod)
library(shiny)
library(shinydashboard)
library(gtools)
library(sm)
library(RCurl)
library(XML)
library(xts)
library(dygraphs)
require(stringr)
library(httr)
library(rdrop2)


mind=-2400
l<<-"ch"
dgf<<-600
token <- drop_auth()
saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)




drop_download('/reports/temp/mcsy.csv', dtoken = token,overwrite = TRUE)
#drop_download('/reports/temp/accinfo_tot.csv', dtoken = token,overwrite = TRUE)
#drop_download('/reports/temp/accinfo_tot1.csv', dtoken = token,overwrite = TRUE)

#drop_download('/reports/temp/cmnn.csv', dtoken = token,overwrite = TRUE)
#drop_download('/reports/temp/dcmnn.csv', dtoken = token,overwrite = TRUE)


drop_download('/reports/temp/mest.csv', dtoken = token,overwrite = TRUE)



# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS



yoursym1=read.table(file="mcsy.csv",header=FALSE,sep=",") 
yoursym1=as.matrix(yoursym1)
yoursym1[is.na(yoursym1)] <- 0

yoursym=as.data.frame((yoursym1[1,]))

colnames(yoursym)=c("symbol")

tes=t(as.character(yoursym$symbol))




minc=read.table(file="minc.csv",header=TRUE,sep=";") 


gen = c('MACD', 'SMA20', 'EVWMA20')

len = c('RESISTANCE', 'TREND')

chop = c('3M', '5D','1M', 'YTD','6M','1Y')
ind1 = c('i1','i2', 'i3','i4', 'i5','i6','i7','i8','i9','i10')
ind2 = c('i2','i1', 'i3','i4', 'i5','i6','i7','i8','i9','i10')
ind3 = c('i3','i2', 'i1','i4', 'i5','i6','i7','i8','i9','i10')
ind4 = c('i4','i2', 'i3','i1', 'i5','i6','i7','i8','i9','i10')
ind5 = c('i5','i2', 'i3','i4', 'i1','i6','i7','i8','i9','i10')
ind6 = c('i6','i2', 'i3','i4', 'i5','i1','i7','i8','i9','i10')
ind7 = c('i7','i2', 'i3','i4', 'i5','i6','i1','i8','i9','i10')
ind8 = c('i8','i2', 'i3','i4', 'i5','i6','i7','i1','i9','i10')
ind9 = c('i9','i2', 'i3','i4', 'i5','i6','i7','i8','i1','i10')



ui <- dashboardPage(
  
  dashboardHeader(title = 'Day2trade', titleWidth = 220)
  ,
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem('Stock Pick Algorithm ', tabName = 'stats', icon = icon('bar-chart')),
      hr(), menuItem('About', tabName = 'about', icon = icon('info-circle')),
      tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})')))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    ## tags$head(tags$style(HTML(".small-box {height: 95px}"))),
    fluidRow(
      column(
        width = 12, offset = 0,
        tabItems(
          tabItem(
            tabName = 'stats',
            fluidRow(
              ## title
              column(width = 10, offset = 1, align = 'center', h1(tags$i('Data Visualization ')), tags$br()),
              column(
                width = 12, align = 'center',
                tabBox(
                  title = 'Daytrade Stock Picking Algorithem', width = NULL,
                  
                  
                 
                       
                  tabPanel(
                    title = 'Strategy Backtest',
                    fluidRow(
                      
                      column(
                        width = 4,
                        box( title = 'Input',status = 'info', solidHeader = T, width = NULL,height = 170,
                             uiOutput("allInputs1")
                          
                        )),
                      column(
                        width = 4,
                        
                        box( title = 'Data Refresh',status = 'info', solidHeader = T, width = NULL,height = 170,
                            
                             textInput("custom1", "Custom symbol", "NSE:"),
                             submitButton("Update View", icon("refresh"))
                        )) ,
                      
                      column(
                        width = 4,
                        box( title = 'Data Refresh',status = 'info', solidHeader = T, width = NULL,height = 170,
                             switchInput(inputId = "somevalue2", value = FALSE),
                             textInput("Model", "model", "i1+i2+i3+i4+i5+i6+i7+i8+i9")
                                
                        ))
                      
                    ),
                    
                     
                      
                        
                         fluidRow(
                           column(
                             width = 12, offset = 0, align = 'center',
                             box(
                               title = 'Candle Chart', status = 'success', width = NULL,
                               solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE ,
                               tabBox(
                                 title = NULL, width = NULL,
                                 
                                 
                                 tabPanel(title = 'Interactivehist',   dygraphOutput("interactiveh")),
                                 tabPanel(title = 'Interactiveday',   dygraphOutput("interactivem"))
                                 
                               ),
                               
                               column(
                                 width = 6,
                                 actionButton("run", "START"),  submitButton("Update View", icon("refresh"))
                               ),
                              
                               column(
                                 width = 6,box(title = 'Today Delay', status = 'info', solidHeader = T, width = NULL,height = 120,
                                               
                                               
                                               numericInput("Ninf", label = "Make changes", value = 0)
                                 )))))
                               ,
                               
                    fluidRow(
                      column(
                        width = 12, offset = 0, align = 'center',
                        box(
                          title = 'Option Chart', status = 'success', width = NULL,
                          solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE ,
                          tabBox(
                            title = NULL, width = NULL,
                            
                            
                            tabPanel(title = 'Option',  tableOutput("iOptionm")     )),
                            column(
                              width = 6,
                              actionButton("run3", "START"),  submitButton("Update View", icon("refresh"))
                            )
                            
                          
                        
                          
                    )))
                    ,
                    
                    
                               fluidRow(
                                 column(
                                   width = 12, offset = 0, align = 'center',
                                   box(
                                     title = 'Line Chart', status = 'success', width = NULL,
                                     solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE ,
                                     sliderInput("delaymm", "",min = 0, max = 2500,step=1,value=c(0,2500)),
                                     tabBox(
                                       title = NULL, width = NULL,
                                       
                                       
                                       tabPanel(title = 'Hist Ptrn/Suppport/Resistance',plotlyOutput("plotwrh")),
                                       tabPanel(title = 'Day Ptrn/Suppport/Resistance',plotlyOutput("plotwrm"))
                                       
                                     ),
                                     
                                     
                                     column(
                                       width =6,box(title = 'Today Delay', status = 'info', solidHeader = T, width = NULL,height = 120,
                                                    
                                                    actionButton("run2", "START"),submitButton("Update View", icon("refresh"))
                                                    
                                       )) ,
                                     
                                     column(
                                       width = 6,box(title = 'Today Delay', status = 'info', solidHeader = T, width = NULL,height = 120,
                                                     sliderInput("delaydmm", "",min = -2500, max = 0,step=1,value=c(-150,0)),
                                                     
                                                     numericInput("Ninf1", label = "Make changes", value = 0)
                                                     
                                       )))))
                                     ,
                                 fluidRow(
                                   column(
                                     width = 12, offset = 0, align = 'center',
                        box(
                          title = 'History Data Load', status = 'success', width = NULL,
                          solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE ,
                          tabBox(
                            title = NULL, width = NULL,
                            
                            
                            tabPanel(title = 'Performance Stats',   tableOutput("view3") ),
                            tabPanel(title = 'Strategy', plotlyOutput("plot1mshm"))
                          ),
                          column(
                            width = 6,
                          sliderInput("delayhm", "",min = -2500, max = 0,step=1,value=c(-365,0)),
                          submitButton("Update View", icon("refresh"))),
                       
                          column(
                            width = 6,
                          box( title = 'Data Refresh',status = 'info', solidHeader = T, width = NULL,height = 100,
                                 switchInput(inputId = "somevalue3", value = FALSE)
                            )
                          
                        )))),
                        # scatter plot of Candle Chart over time/rating
                        fluidRow(
                          column(
                            width = 12, offset = 0, align = 'center',
                            box(
                          title = 'Today Data Load', status = 'success', width = NULL,
                          solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE ,
                          tabBox(
                            title = NULL, width = NULL,
                            tabPanel(title = 'Performance Stats',   tableOutput("view2") ),
                            tabPanel(title = 'Strategy',  plotOutput("plot1mstm"))
                            
                           
                          ),
                          column(
                            width = 6,sliderInput("delaydm", "",min = -2500, max = 0,step=1,value=c(-2400,0)),
                            actionButton("run1", "START"),  submitButton("Update View", icon("refresh"))
                          ),
                          column(
                            width = 3,
                            box( title = 'Strategy Refresh',status = 'info', solidHeader = T, width = NULL,height = 130,
                                 switchInput(inputId = "somevalue4", value = FALSE)
                            )
                        )
                        ,
                        column(
                          width = 3,
                          box( title = 'Data Refresh',status = 'info', solidHeader = T, width = NULL,height = 130,
                               switchInput(inputId = "somevalue5", value = FALSE)
                          )
                        )
                        
                        ))),
                        
                        fluidRow(
                          
                          column(
                            width = 6,
                            box( title = 'No of indicators want to use',status = 'info', solidHeader = T, width = NULL,height = 120,
                                 numericInput("Noind", label = "Make changes", value = 2)
                            )),
                          
                          column(
                            width = 6,
                            box( title = 'Trade Count',status = 'info', solidHeader = T, width = NULL,height = 120,
                                 numericInput("NOTRD", label = "Trade Count", value = 3)
                            ))
                          
                          
                        ),
                        
                        fluidRow(
                          
                          column(
                            width = 6,
                            box( title = 'Trend gap',status = 'info', solidHeader = T, width = NULL,height = 120,
                                 textInput("Ngap", "sma1", "1")
                            )),
                          
                          column(
                            width = 6,
                            box( title = 'Sell Off percent',status = 'info', solidHeader = T, width = NULL,height = 120,
                                 textInput("Soff", "smaf", "0.6")
                            ))
                        ),
                    fluidRow(
                      
                      column(
                        width = 6,
                        box( title = 'Email',status = 'info', solidHeader = T, width = NULL,height = 120,
                             textInput("Ngap1", "Email", "vakati2us@gmail.com")
                        )),
                      
                      column(
                        width = 6,
                        box( title = 'Time Zone',status = 'info', solidHeader = T, width = NULL,height = 120,
                             textInput("Soff1", "Time Zone", "1")
                        ))
                    )
                        
                       
                      )
                    )
                  )
                )))
          )
        )
      )     
)


      


