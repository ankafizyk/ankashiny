#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


#library(devtools)
#install_github("nik01010/dashboardthemes")

getwd()
list.files()
#devtools::install("C:\\Users\\mszurmanska\\Desktop\\dashboardthemes-master")

library(dashboardthemes)
library(highcharter)
library(sp)
library(rgdal)
library(rgeos)
library(openxlsx)
library(shiny)
library(ggplot2)
library(shinydashboard)


library(flexdashboard)
library(haven)
library(plotly)
library(dplyr)
library(purrr)

library(htmlwidgets)
library(ggvis)
library(shinydashboardPlus)
library(kableExtra)

#setwd("//allianz.local/AllianzFile/Raporty/Raporty_TMD/RoboczyMIS/Portal")
poland.map <- readOGR(dsn="wojewodztwa", "wojewodztwa",encoding = 'UTF-8') #argumentami są nazwa folderu oraz nazwa pliku (nie podajemy rozszerzenia)

class(poland.map) #typ obiektu przestrzennego
ncol(poland.map@data) #29 kolumn, jest wiele niepotrzebnych nam danych

poland.map@data <- poland.map@data[ , c(6,16)] #weźmy tylko nazwy województw oraz ich powierzchnie
names(poland.map@data) <- c("nazwa", "powierzchnia")

poland.map@data$nazwa <- c("opolskie", "swietokrzyskie", "kujawsko-pomorskie", "mazowieckie", "pomorskie", "slaskie",
                           "warminsko-mazurskie", "zachodniopomorskie", "dolnoslaskie", "wielkopolskie", "lodzkie",
                           "podlaskie", "malopolskie", "lubuskie", "podkarpackie", "lubelskie")



MAP1 <- read.xlsx("CLAIMS_MAP.xlsx", sheet=1)

poland.map@data <- cbind(poland.map@data, 1:16)
poland.rand.dataa <-data.frame('nazwa'=unique(poland.map@data$nazwa),number=MAP1$liczba)

poland.rand.datac <-data.frame('nazwa'=unique(poland.map@data$nazwa),number.=MAP1$liczba.)



poland.map@data <- merge(poland.map@data, poland.rand.dataa, by='nazwa', all=T,sort=FALSE)

poland.map@data <- merge(poland.map@data, poland.rand.datac, by='nazwa', all=T,sort=FALSE)


poland.map@data$val1 <- poland.map@data$number
poland.map@data$val1
poland.map@data$val2 <- poland.map@data$number.
poland.map@data$val2


poland.map.gg <- ggplot2::fortify(poland.map,region="nazwa")

head(poland.map.gg, n=2)

poland.map.gg <- merge(poland.map.gg, poland.map@data, by.x="id", by.y="nazwa", sort=FALSE)

head(poland.map.gg, n=2) #mamy wszystkie dane
wykres1 <- read.xlsx("GWP_DILL.xlsx", sheet=7)
wykres11 <- read.xlsx("GWP_DILL.xlsx", sheet=8)
wykres111 <- read.xlsx("GWP_DILL.xlsx", sheet=12)
wykres1112 <- read.xlsx("GWP_DILL.xlsx", sheet=13)
q <- read_sas("dane_s1_w1.sas7bdat")
d <- q[q$MONTH==1,]
wykres2 <- read_sas("dane_s1_w2.sas7bdat")
wykres3 <- read_sas("gwp_nl.sas7bdat")
wykres4 <- read_sas("gwp_pr.sas7bdat")
data <- read.xlsx("waterfall.xlsx")
claim2 <- read.xlsx("claim.xlsx", sheet=4)



dane_daym30 <- read_sas("m30.sas7bdat")

dane_day30 <- read_sas("c30.sas7bdat")
data_hp <- read.xlsx("GWP_DILL.xlsx", sheet=15)
 data_1 <- read.xlsx("GWP_DILL.xlsx", sheet=6)
data1 <- read.xlsx("waterfall.xlsx", sheet=2)
  data_11 <- read.xlsx("GWP_DILL.xlsx", sheet=9)
data13 <- read.xlsx("waterfall.xlsx", sheet=3)
data_113 <- read.xlsx("GWP_DILL.xlsx", sheet=10)
 data132 <- read.xlsx("waterfall.xlsx", sheet=4)
data_1132 <- read.xlsx("GWP_DILL.xlsx", sheet=11)
claim1 <- read.xlsx("claim.xlsx", sheet=3)
claim2 <- read.xlsx("claim.xlsx", sheet=4)
claim3 <- read.xlsx("claim.xlsx", sheet=1)
claim4 <- read.xlsx("claim.xlsx", sheet=2)
claim8a <- read.xlsx("CLAIM_METODA.xlsx",sheet=1)
 claim7a  <- read.xlsx("CLAIM_2017_2018_PLAN.xlsx", sheet=2)
claim6a <- read.xlsx("CLAIM_METODA.xlsx",sheet=2)
claim5a  <- read.xlsx("CLAIM_2017_2018_PLAN.xlsx", sheet=1)


ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title="Allianz Poland",
                    tags$li(class="dropdown",tags$a(href="https://wiki.allianz.pl/spaces/viewspace.action?key=MIS",icon("book"),target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://www.facebook.com/AllianzPolska/",icon("facebook"),target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/showcase/allianz-poland",icon("linkedin"),target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://www.youtube.com/watch?v=iAc6Qr_sAXw",icon("youtube"),target="_blank")),
                    tags$li(class="dropdown",tags$a(href="mailto:mis@allianz.pl",icon("envelope"),target="_blank"))


    ),
    dashboardSidebar(

      # sidebarSearchForm(label = "Search", "searchText", "searchButton",icon = shiny::icon("search")),

      sidebarMenu(


        menuItem("OP Target Overview", tabName = "total_nl", icon = icon("dashboard")),

        menuItem("Finance", icon = icon("bars"),

                 menuSubItem("NonLife Overview", tabName = "finance"),
                 menuSubItem("Motor", tabName = "f_motor"),
                 menuSubItem("Retail", tabName = "f_retail"),
                 menuSubItem("Corpo", tabName = "f_claims")),

        menuItem("Claims", icon = icon("bars"),

                 menuSubItem("NonLife Overview", tabName = "claims"),
                 menuSubItem("Motor", tabName = "c_motor"),
                 menuSubItem("Retail", tabName = "c_retail"),
                 menuSubItem("Corpo", tabName = "c_corpo")
        ),

        menuItem("Sales", icon = icon("bars"),tabName="sales" ),
        menuItem("Operations", icon = icon("bars"),tabName="operations"),
        menuItem("Market Analysis", icon = icon("bars"),tabName="market")
        #dateInput("date", label = h3("Date input"), value = "2014-01-01")

      )),
    dashboardBody(
      shinyDashboardThemes(
        theme = "grey_dark"
      ),
      tabItems(
        #---------------------------Total non life

        tabItem("total_nl",



                fluidRow(  widgetUserBox(
                  title = "Management Information System",
                  subtitle = "Allianz Platform of Analytical Excelence",
                  type = NULL,
                  src = "https://scontent-ams3-1.xx.fbcdn.net/v/t1.0-9/17342983_10154374939008365_3895713359493389650_n.png?_nc_cat=0&oh=117e916756266396f9c5c91e7e02bccd&oe=5BFB9256",
                  background = TRUE,
                  #color = "aqua",
                  #backgroundUrl = "https://scontent-ams3-1.xx.fbcdn.net/v/t1.0-9/31801_392966373364_4399681_n.jpg?_nc_cat=0&oh=9c2d40b86093acabb40158a1ee24a134&oe=5BFF3DCE",

                  backgroundUrl = "Baner.png",

                  closable = TRUE,
                  h2("Operating Profit Development"),
                  #footer = "Operating Profit Development",

                  width =12, height = 250)),



                #fluidRow(shinydashboard::valueBoxOutput("gwp5",width=12)),
                #fluidRow(box(flexdashboard::gaugeOutput("hp5"), width = 3, height = 170)),



                # fluidRow(shinydashboard::valueBoxOutput("gwp5",width=4),
                #          shinydashboard::valueBoxOutput("gwp1",width = 2),
                #        shinydashboard::valueBoxOutput("gwp2",width=2),
                #       shinydashboard::valueBoxOutput("gwp3",width=2),
                #      shinydashboard::valueBoxOutput("gwp4",width=2)),

                fluidRow(box(flexdashboard::gaugeOutput("hp5"),title="Allianz Poland Group", width = 4, height = 170),
                         box(flexdashboard::gaugeOutput("hp1"),title="PC", width = 2, height = 170),
                         box(flexdashboard::gaugeOutput("hp2"),title="LH", width = 2, height = 170),
                         box(flexdashboard::gaugeOutput("hp3"),title="PF", width = 2, height = 170),
                         box(flexdashboard::gaugeOutput("hp4"),title="AM", width = 2, height = 170)
                ),

                fluidRow(box(plotlyOutput("plothp"),width=12, height = 280, title = "Operating Profit monthly"))


        ),


        #-----------------------Finance-> Retail----------
        tabItem("f_retail"),


        #-----------finance-------------------
        tabItem("finance",
                fluidRow(
                  box(flexdashboard::gaugeOutput("flx1"),width=3,title="Gross Written Premium (GWP)",height = 170),
                  box(flexdashboard::gaugeOutput("flx2"),width=3,title="Operating Profit (OP)", height = 170),
                  box(flexdashboard::gaugeOutput("flx3"),width=3,title="Combined Ratio (CoR)",height = 170),
                  box(flexdashboard::gaugeOutput("flx4"),width=3,title="Expense Ratio",height = 170
                  )
                ),
                fluidRow(

                  #tabBox(selected =  "Total",side="left",
                  #   tabPanel("Total","waterfall"),
                  #  tabPanel("NonLife", "ffffff"),
                  # tabPanel("Life", "fff")
                  #,width=8),
                  box(plotlyOutput("plot2"),width=6, height = 300, title = "Operating Profit YTD"),
                  box(highchartOutput("plot6", width = "100%",height="280"), width=3,height = 300, title="Expenses"),
                  box(plotlyOutput("plot7"), width=3,height = 300, title="Claims Costs")

                  #    box(title="OP & CoR", solidHeader = TRUE,status = "info",plotlyOutput("plot1",inline=FALSE,height="250px"),width=8),
                  #   box(title="GWP YTD",status = "info",plotlyOutput("plot3",inline=FALSE,height="250px"),width=4)
                ),
                fluidRow(
                  box(plotlyOutput("plot1"),width=8,height = 400, title = "CoR & OP (monthly)"),
                  box(plotlyOutput("plot11"),width=4,height = 400,title = "Operating Profit YTD (per Product)" )

                )
                ),


                #-----------finance-> Motor-------------------

                tabItem("f_motor",

                        tabBox(selected="Total", side="left",
                               tabPanel("Total",

                                        fluidRow(
                                          box(flexdashboard::gaugeOutput("flx_m1"),width=3,title="Gross Written Premium (GWP) ",height = 170),
                                          box(flexdashboard::gaugeOutput("flx_m2"),width=3,title="Operating Profit (OP)", height = 170),
                                          box(flexdashboard::gaugeOutput("flx_m3"),width=3,title="Combined Ratio (CoR)",height = 170),
                                          box(flexdashboard::gaugeOutput("flx_m4"),width=3,title="Expense Ratio",height = 170
                                          ), width=12),

                                        fluidRow(

                                          box(plotlyOutput("plot21"),width=8, height = 270, title = "Operating Profit YTD"),
                                          box(plotlyOutput("plot71"), width=4,height = 270, title="Claims Costs")

                                        ),
                                        fluidRow(
                                          box(plotlyOutput("plot1_1"),width=8, title = "CoR & OP (monthly)", height = 250),
                                          box(plotlyOutput("plot111"),width=4,title = "Operating Profit YTD ( per Product)" , height = 250)

                                        )),
                               tabPanel("MTPL",
                                        fluidRow(
                                          box(flexdashboard::gaugeOutput("flx_m11"),width=3,title="Gross Written Premium (GWP) ",height = 170),
                                          box(flexdashboard::gaugeOutput("flx_m21"),width=3,title="Operating Profit (OP)", height = 170),
                                          box(flexdashboard::gaugeOutput("flx_m31"),width=3,title="Combined Ratio (CoR)",height = 170),
                                          box(flexdashboard::gaugeOutput("flx_m41"),width=3,title="Expense Ratio",height = 170
                                          ), width=12),
                                        fluidRow(

                                          box(plotlyOutput("plot211"),width=8, height = 270, title = "Operating Profit YTD"),
                                          box(plotlyOutput("plot711"), width=4,height = 270, title="Claims Costs")

                                        ),
                                        fluidRow(
                                          box(plotlyOutput("plot1_11"),width=8, title = "CoR & OP (monthly)", height = 220),
                                          box(plotlyOutput("plot1113"),width=4,title = "Operating Profit YTD (per Product)" , height = 220)

                                        )
                               ),
                               tabPanel("MOD",
                                        fluidRow(
                                          box(flexdashboard::gaugeOutput("flx_m12"),width=3,title="Gross Written Premium (GWP) ",height = 170),
                                          box(flexdashboard::gaugeOutput("flx_m22"),width=3,title="Operating Profit (OP)", height = 170),
                                          box(flexdashboard::gaugeOutput("flx_m32"),width=3,title="Combined Ratio (CoR)",height = 170),
                                          box(flexdashboard::gaugeOutput("flx_m42"),width=3,title="Expense Ratio",height = 170
                                          ), width=12),
                                        fluidRow(

                                          box(plotlyOutput("plot2112"),width=8, height = 270, title = "Operating Profit YTD"),
                                          box(plotlyOutput("plot7112"), width=4,height = 270, title="Claims Costs")

                                        ),
                                        fluidRow(
                                          box(plotlyOutput("plot1_112"),width=8, title = "CoR & OP (monthly)", height = 220),
                                          box(plotlyOutput("plot11132"),width=4,title = "Operating Profit YTD (per Product)" , height = 220)

                                        )
                               )




                               ,width=12, height=900)
                ),
                #----------------------Finance-> Corpo----------------
                tabItem("f_corpo"),

                #---------------------Claims----------------
                tabItem("claims",
                        fluidRow(
                          box(flexdashboard::gaugeOutput("flx_c1"),width=3,title="Loss Ratio_Reported ",height = 170),
                          box(flexdashboard::gaugeOutput("flx_c2"),width=3,title="Exposure", height = 170),
                          box(flexdashboard::gaugeOutput("flx_c3"),width=3,title="Frequency",height = 170),
                          box(flexdashboard::gaugeOutput("flx_c4"),width=3,title="Claim Value",height = 170)
                        ),
                        fluidRow(
                          box(plotlyOutput("claim2"),title = "Number of Claims & Claim Value", width = 6, height=420),
                          box(plotlyOutput("claim1"),width =6, title = "Loss Ratio YTD", height=420)

                        ),
                        fluidRow(
                          box(tableOutput("tab1"),title="Top Claims YTD",width=6, height=200),
                          box(tableOutput("tab2"),title = "Top Car Brand Theft YTD ",width=6, height=200)

                        )
                ),

                #----------------Claims->Motor----------------------
                tabItem("c_motor",
                        tabBox(selected="MTPL", side="left",
                               tabPanel("MTPL",
                                        fluidRow(
                                          box(flexdashboard::gaugeOutput("flx_e11"),width=3,title="Loss Ratio_Reported ",height = 170),
                                          box(flexdashboard::gaugeOutput("flx_e21"),width=3,title="Exposure", height = 170),
                                          box(flexdashboard::gaugeOutput("flx_e31"),width=3,title="Frequency",height = 170),
                                          box(flexdashboard::gaugeOutput("flx_e41"),width=3,title="Claim Value",height = 170)
                                          , width=12),

                                        fluidRow(
                                          column(width =6,
                                                 box(plotlyOutput("claim5"),width =NULL, title = "Loss Ratio & avg. Claims",height = 260),
                                                 box(plotlyOutput("claim6"),width=NULL, title = "Claims settelment YTD",height = 260)
                                          ),
                                          column(width=6,
                                                 box(plotOutput("Plotmap", width='80%'),width = NULL, title = "Claims number per region",height = 520)

                                          )#,
                                          #fluidRow(
                                          # box(plotlyOutput("claim6"),width=12, title = "Claims settelment YTD",height = 200)
                                        )
                                        ,
                                        fluidRow(
                                          box(plotlyOutput("claim1_0",height = 180),width=12, title = "Claims Avg. Daily",height = 200)
                                        )
                               ),
                               tabPanel("MOD",

                                        fluidRow(
                                          box(flexdashboard::gaugeOutput("flx_d12"),width=3,title="Loss Ratio_Reported ",height = 170),
                                          box(flexdashboard::gaugeOutput("flx_d22"),width=3,title="Exposure", height = 170),
                                          box(flexdashboard::gaugeOutput("flx_d32"),width=3,title="Frequency",height = 170),
                                          box(flexdashboard::gaugeOutput("flx_d42"),width=3,title="Claim Value",height = 170)
                                          , width=12),

                                        fluidRow(
                                          column(width =6,
                                                 box(plotlyOutput("claim7"),width = NULL, title = "Loss Ratio & avg. Claims",height = 260),
                                                 box(plotlyOutput("claim8"),width = NULL, title = "Claims settelment YTD",height = 260)
                                          ),
                                          column(width=6,
                                                 box(plotOutput("Plotmap2", width='80%'),width = NULL, title = "Claims number per region",height = 520)
                                          )#,
                                          # fluidRow(
                                          # box(plotlyOutput("claim8"),width=12, title = "Claims settelment YTD",height = 200)
                                        ),
                                        fluidRow(
                                          box(plotlyOutput("claim1_1",height = 180),width=12, title = "Claims Avg. Daily",height = 200)
                                        )

                               )
                               ,width=12)


                ),

                #------------------Caims->Retail-------------------
                tabItem("c_retial"),

                #------------------Claims->Corpo----------------
                tabItem("c_corpo")


        ),
	tags$footer(
        tags$a(href = "https://www.youtube.com/watch?v=iAc6Qr_sAXw", "Management Information Development Division | 2018-07" ),align = "center")

      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  output$Plotmap <- renderPlot({

    map<- ggplot() +
      geom_polygon(data = poland.map.gg,
                   aes(long, lat, group = group,  fill = number),
                   colour = "white", lwd=0.1) +
      ggtitle("") +
      labs(x = "", y = ""
           #, fill = "Liczba szkd"
      )
    map + scale_fill_gradient(low = "lightskyblue1", high = "dodgerblue4")+       theme(
        axis.line = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
      axis.text.x = element_blank(),
   panel.background = element_blank())

    #plot(poland.map.gg)
    #plot(poland.map.gg[poland.map.gg$nazwa == "wielkopolskie", ], col = "blue", add = TRUE)
  })

output$Plotmap2 <- renderPlot({

    map<- ggplot() +
      geom_polygon(data = poland.map.gg,
                   aes(long, lat, group = group,  fill = number.),
                   colour = "white", lwd=0.1) +
      ggtitle("") +
      labs(x = "", y = ""
           #, fill = "Liczba szkd"
      )
    map + scale_fill_gradient(low = "lightskyblue1", high = "dodgerblue4")+       theme(
        axis.line = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
      axis.text.x = element_blank(),
   panel.background = element_blank())
    #plot(poland.map.gg)
    #plot(poland.map.gg[poland.map.gg$nazwa == "wielkopolskie", ], col = "blue", add = TRUE)
  })

#------------------------Home Page-------------------------
data_hp

  output$plothp <- renderPlotly({

    ay <- list(
      tickfont = list(color = "white"),
      overlaying = "y",
      side = "right",
      #title = "mln PLN",
      legend = ("left"),
      linecolor="white",
      range=c(0,30)
    )
    plot_ly(data_hp, x =~month,y=~act_pc ,name = "AY AM", color=I("darkorange"),type = 'bar') %>%
      add_trace( x=~month, y =~act_lh,name = "AY PC",color=I("royalblue1")) %>%
      add_trace( x =~month,y=~act_pf ,name = "AY LH", color=I("olivedrab4")) %>%
      add_trace( x=~month, y =~act_am,name = "AY PF",color=I("olivedrab3")) %>%
      # add_lines( x =~month,y=~plan ,name = "Plan", color=I("grey83"),fill='tonexty') %>%
      add_lines( x =~month,y=~plan ,name = "Plan", color=I("lightblue"),fill='tonexty') %>%

      layout(

        yaxis = list(title="mln PLN", range=c(0,35), showgrid = F, zeroline = F),
        xaxis=list(linecolor="lightgrey",showgrid = F),
        height=200, font=list(color='lightgrey'),
        legend=list(orientation="h"),barmode = 'stack',plot_bgcolor = 'rgba(1,1,1, 0.0)',paper_bgcolor = 'rgba(1,1,1, 0.0)'
      )

  })


output$gwp1<-shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      "PC"," ",color="aqua")
  })

  output$gwp2 <- shinydashboard::renderValueBox({


    shinydashboard::valueBox(
      "LH"," ",color="aqua")
  })

  output$gwp3 <- shinydashboard::renderValueBox({


    shinydashboard::valueBox(
      "PF"," ",color="aqua")
  })

  output$gwp4 <- shinydashboard::renderValueBox({


    shinydashboard::valueBox(
      "AM"," ",color="aqua")
  })

  output$gwp5 <- shinydashboard::renderValueBox({


    shinydashboard::valueBox(
      " Allianz Poland Group"," ",color="blue")
  })

  output$hp1 <- flexdashboard::renderGauge({
    gauge(78.3, min = 0, max = 209.2, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(91, 209), warning = c(80,90), danger = c(-30, 79)
    ))
  })

  output$hp2 <- flexdashboard::renderGauge({
    gauge(52.3, min = 0, max = 86.6, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(48, 90), warning = c(40,48), danger = c(-30, 39)
    ))
  })

  output$hp3 <- flexdashboard::renderGauge({
    gauge(21.4, min = 0, max = 23.9, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(13, 210), warning = c(10,12), danger = c(-30, 9)
    ))
  })

  output$hp4 <- flexdashboard::renderGauge({
    gauge(6.9, min = 0, max = 11.4, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(5, 210), warning = c(3,4), danger = c(-30, 2)
    ))
  })


  output$hp5 <- flexdashboard::renderGauge({
    gauge(158.9, min = 0, max = 330, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(160, 330), warning = c(150,159), danger = c(-30, 149)
    ))
  })



  #-------------------Non Life------FINANCE-----------------------
  #-----------------------FlexBoxy - Non Life-----------
  output$flx1 <- flexdashboard::renderGauge({
    gauge(1047, min = 0, max = 1947.9, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(900, 2000), warning = c(500,899), danger = c(0, 499)
    ))

  })

  output$flx2 <- renderGauge({
    gauge(76.6, min = 0, max = 209.2, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(70, 210), warning = c(59,69), danger = c(0, 48)
    ))

  })

  output$flx3 <- renderGauge({
    gauge(-94.4, min = -105, max =-90.8, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(-95, -50), warning = c(-100,-99), danger = c(-120,-102 )
    ))

  })

  output$flx4 <- renderGauge({
    gauge(-32.0, min = -100, max = -32.0, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(-32, 0), warning = c(-33,-50), danger = c(-50, -100)
    ))

  })

 #--------------Fianace -> Plot OP&CoR  ----------

  output$plot1 <- renderPlotly({


    f<-list(size=8)
    ay <- list(
      tickfont = list(color = "black",size=8),
      overlaying = "y",
      side = "right",
      title = "%",
      legend = ("left"),
      showgrid = F
    )
    plot_ly(wykres1) %>%
      add_lines(x =~MONTH, y =~COR_PREV_YEAR, name = "CoR PY", color=I("gray"),yaxis="y2") %>%
      add_lines(x =~MONTH, y =~COR_ACT_YEAR, name = "CoR AY", color=I("steelblue4"),yaxis="y2") %>%
      add_lines(x =~MONTH, y =~COR_PLAN, name = "CoR Plan", color=I("dodgerblue"),yaxis="y2") %>%
      add_bars( x=~MONTH, y =~OP_PREV_YEAR, name = "OP PY", color=I("gray")) %>%
      add_bars( x =~MONTH,y=~OP_ACT_YEAR, name = "OP AY", color=I("steelblue4")) %>%
      add_bars( x=~MONTH, y =~OP_PLAN, name = "OP Plan", color=I("steelblue3")) %>%

      layout(
        xaxis = list(title = "",tickfont = list(size = 8),zerolinecolor="lightgrey"),
        yaxis2=ay,font=list(color='lightgrey'),
        yaxis = list(title="PLN mln",tickfont = list(size = 8),titlefont=f,showgrid = F), height=350,
        legend=list(font =list(size=10),orientation='h'),plot_bgcolor = 'rgba(1,1,1, 0.0)',paper_bgcolor = 'rgba(1,1,1, 0.0)'
      )

  })

#--------Finance  - > waterfall------------------
  output$plot2 <- renderPlotly({

    data


    plot_ly(data, x =~X1, y =~base, type = 'bar', color =I('steelblue4')) %>%
      add_trace(y = ~k1, marker=  list(color='rgba(1,1,1, 0.0)')) %>%
      add_trace(y = ~k2, marker = list(color = "rgb(204,204,204)")) %>%
      add_trace(y = ~k3, marker = list(color = "steelblue")) %>%
      add_trace(y = ~k4, marker = list(color = "steelblue")) %>%
      add_trace(y = ~k5, marker = list(color = "grey")) %>%
      add_trace(y = ~k6, marker = list(color = "thistle")) %>%
      add_trace(y = ~k7, marker = list(color = "mediumblue")) %>%
      layout(
        xaxis = list(title = "",tickfont = list(size = 9),linecolor="lightgrey"),
        yaxis = list(title = "",tickfont = list(size = 8),showgrid = F),
        barmode = 'stack',
        height=260,
        paper_bgcolor = 'rgba(1,1,1, 0.0)',
        plot_bgcolor = 'rgba(1,1,1, 0.0)',
        showlegend = FALSE,
        font=list(color='lightgrey')
      ) %>%
      add_annotations(text="",
                      #x = x,
                      #y = y,
                      #xref = "x",
                      #yref = "y",
                      font = list(family = 'Arial',
                                  size = 14),
                                  #color = 'rgba(1,1,1, 0.0)'),
                      showarrow = FALSE)

  })



#----------------slupek pion per prod FINANCE---


  data_1

  output$plot11 <- renderPlotly({

    plot_ly(data_1, y = ~product, x = ~ay, type = 'bar', name = 'OP ay', marker = list(color = "steelblue3")) %>%
      add_trace(x = ~py, name = 'OP py', marker = list(color = 'rgb(204,204,204)')) %>%
      add_trace(x = ~plan, name = 'OP plan', marker = list(color = 'rgb(117, 174, 222)')) %>%
      layout(xaxis = list(title = "",tickfont = list(size = 8),showgrid = F),
             yaxis = list(title = "",tickfont = list(size = 10),showgrid = F,linecolor="lightgrey"),
             margin = list(b = 100),
             barmode = 'group',
             legend=list(font =list(size=10),orientation='h'),plot_bgcolor = 'rgba(1,1,1, 0.0)',paper_bgcolor = 'rgba(1,1,1, 0.0)',
             font=list(color='lightgrey'),
             height=350)

  })

#---------------Finance  ----> Plot Pie ---- Cost----------------

  output$plot6  <- renderHighchart({

    name = c("ULAE cy","ULAE py","Other acquisition","Admin","FA Investments")

    valu = c("18.5","15.8","42.1","25.4","0.6")
    y<-as.numeric(valu)
    drilldown <-tolower(name)
    df <- data.frame(name, y, drilldown)
    df

    hc <- highchart() %>%
      hc_chart(type = "pie") %>%
      #hc_xAxis(type = "category") %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE, distance= -30)
        )
      ) %>%
      hc_add_series(
        data = df,
        name = " "
        # colorByPoint = TRUE
      )

    hc
     dfadm <- data_frame(
      name = c("ADMIN GROSS", "OTHER", "PROVISION SAR","PROVISION RSU"),
      value = c(24.3, 0.9, 0,0.1)
    )

    hc <- hc %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = list(
          list(
            id = "admin",
            data = list.parse2(dfadm)
          )
        )
      )

    # Categorie = c("ULAE cy","ULAE py","Other acquisition","Admin","FA Investments")
    # Categorie
    # valu = c("18.5","15.8","42.1","25.4","0.6")
    # values<-as.numeric(valu)
    # df = data.frame(Categorie, values)
    # df
    #
    # colors <- c('rgb(114,94,96)', 'rgb(128,133,133)', 'rgb(204,204,204)', 'rgb(114,147,203)','rgb(144,103,167)')
    # plot_ly(df, labels = ~Categorie, values = ~values, type = 'pie',
    #         textposition = 'inside',
    #         textinfo = 'label+percent',
    #         insidetextfont = list(color = '#FFFFFF',size = 10),
    #         hoverinfo = 'text',
    #         text = ~paste( values, ' mln pln', Categorie),
    #         marker = list(colors = colors,
    #                       line = list(color = '#FFFFFF', width = 1)),
    #         #The 'pull' attribute can also be used to create space between the sectors
    #         showlegend = FALSE) %>%
    #   layout(
    #     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #     height=240)

  })

  #------------Finance---> Plot Pie Claims Cost---------------

  output$plot7 <- renderPlotly({

    Cat = c("Paid", "Reserves","IBNR","Liquidation Cost","RUN OFF")
    valu = c("205.9","139.2","183.9","2.8","14.2")
    value<-as.numeric(valu)
    df1 = data.frame(Cat, value)
    df

    colors <- c('salegray', 'rgb(204,204,204)', 'gray', 'plum', 'lightblue')
    plot_ly(df1, labels =~Cat, values =~value, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF',size = 10),
            hoverinfo = 'text',
            text = ~paste( value, ' mln pln',Cat),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        plot_bgcolor = 'rgba(1,1,1, 0.0)',paper_bgcolor = 'rgba(1,1,1, 0.0)',
        font=list(color='lightgrey'),
        height=240)

  })

  #----------------FINANCE---------MOTOR------------------------------------------

  #-----------------------FlexBoxy - Motor-----------
  output$flx_m1 <- flexdashboard::renderGauge({
    gauge(657.3, min = 0, max = 1167.1, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(580, 2000), warning = c(500,589), danger = c(0, 499)
    ))

  })

  output$flx_m2 <- renderGauge({
    gauge(70.2, min = 0, max = 142.7, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(50, 200), warning = c(45,49), danger = c(-30, 44)
    ))

  })

  output$flx_m3 <- renderGauge({
    gauge(-90.6, min = -105, max =-89.0, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(-95, -50), warning = c(-100,-99), danger = c(-120,-102 )
    ))

  })

  output$flx_m4 <- renderGauge({
    gauge(-27.2, min = -100, max = -32.0, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(-32, 0), warning = c(-33,-50), danger = c(-50, -100)
    ))

  })


  #--------Finance -> MOTOR  - > waterfall------------------
  output$plot21 <- renderPlotly({

    data1


    plot_ly(data1, x =~X1, y =~base, type = 'bar', color =I('steelblue4')) %>%
      add_trace(y = ~k1, marker=  list(color='rgba(1,1,1, 0.0)')) %>%
      add_trace(y = ~k2, marker = list(color = "rgb(204,204,204)")) %>%
      add_trace(y = ~k3, marker = list(color = "steelblue")) %>%
      add_trace(y = ~k4, marker = list(color = "steelblue")) %>%
      add_trace(y = ~k5, marker = list(color = "grey")) %>%
      add_trace(y = ~k6, marker = list(color = "thistle")) %>%
      add_trace(y = ~k7, marker = list(color = "mediumblue")) %>%
      layout(
        xaxis = list(title = "",tickfont = list(size = 8)),
        yaxis = list(title = "",tickfont = list(size = 8)),
        barmode = 'stack',
        height=260,

        paper_bgcolor = 'rgba(1,1,1, 0.0)',
        plot_bgcolor = 'rgba(1,1,1, 0.0)',
        showlegend = FALSE) %>%
      add_annotations(text="",
                      #x = x,
                      #y = y,
                      #xref = "x",
                      #yref = "y",
                      font = list(family = 'Arial',
                                  size = 14,
                                  color = 'rgba(1,1,1, 0.0)'),
                      showarrow = FALSE)

  })


  #------------Finance->MOTOR---> Plot Pie Claims Cost---------------

  output$plot71 <- renderPlotly({

    Cat1 = c("Paid", "Reserves","IBNR","Liquidation Cost","RUN OFF")
    valu1 = c("166.5","76.2","92.6","1.5","5.3")
    value1<-as.numeric(valu1)
    df11 = data.frame(Cat1, value1)
    df11

    colors <- c('salegray', 'rgb(204,204,204)', 'gray', 'plum', 'lightblue')
    plot_ly(df11, labels =~Cat1, values =~value1, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF',size = 8),
            hoverinfo = 'text',
            text = ~paste( value1, ' mln pln',Cat1),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        height=260)

  })

  #--------------Fianace  -----> MOTOR -> Plot OP&CoR  ----------

  output$plot1_1 <- renderPlotly({
    f<-list(size=8)
    ay <- list(
      tickfont = list(color = "black",size=8),
      overlaying = "y",
      side = "right",
      title = "%",
      titlefont=list(size=8),
      legend = ("left"),
      size=10
    )
    plot_ly(wykres11) %>%
      add_lines(x =~MONTH, y =~COR_PREV_YEAR, name = "CoR PY", color=I("gray"),yaxis="y2") %>%
      add_lines(x =~MONTH, y =~COR_ACT_YEAR, name = "CoR AY", color=I("royalblue4"),yaxis="y2") %>%
      add_lines(x =~MONTH, y =~COR_PLAN, name = "CoR Plan", color=I("dodgerblue"),yaxis="y2") %>%
      add_bars( x=~MONTH, y =~OP_PREV_YEAR, name = "OP PY", color=I("gray")) %>%
      add_bars( x =~MONTH,y=~OP_ACT_YEAR, name = "OP AY", color=I("royalblue4")) %>%
      add_bars( x=~MONTH, y =~OP_PLAN, name = "OP Plan", color=I("steelblue3")) %>%

      layout(
        xaxis = list(title = "",tickfont = list(size = 8)),
        yaxis2=ay,
        yaxis = list(title="PLN mln",tickfont = list(size = 8),titlefont=f), height=250,
        legend=list(font =list(size=10),orientation='h')
      )

  })

 #----------------slupek pion per prod MOTOR---


  data_11

  output$plot111 <- renderPlotly({

    plot_ly(data_11, y = ~product, x = ~ay, type = 'bar', name = 'OP ay', marker = list(color = 'rgb(0, 64, 222)')) %>%
      add_trace(x = ~py, name = 'OP py', marker = list(color = 'rgb(204,204,204)')) %>%
      add_trace(x = ~plan, name = 'OP plan', marker = list(color = 'rgb(117, 174, 222)')) %>%
      layout(xaxis = list(title = "",tickfont = list(size = 8)),
             yaxis = list(title = "",tickfont = list(size = 10)),
             margin = list(b = 100),
             barmode = 'group', height=250,
             legend=list(font =list(size=10),orientation='h'))

  })


  #---------------------------------------------------------
  #---------------MTPL----------------------------------

  #-----------------------FlexBoxy - Motor-----------
  output$flx_m11 <- flexdashboard::renderGauge({
    gauge(233.0, min = 0, max = 473.5, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(230, 2000), warning = c(200,229), danger = c(0, 199)
    ))

  })

  output$flx_m21 <- renderGauge({
    gauge(12.9, min = 0, max = 35.7, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(10, 200), warning = c(8,9), danger = c(-30,7)
    ))

  })

  output$flx_m31 <- renderGauge({
    gauge(-99.5, min = -105, max =-89.0, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(-95, -50), warning = c(-100,-99), danger = c(-120,-102 )
    ))

  })

  output$flx_m41 <- renderGauge({
    gauge(-27.6, min = -100, max = -32.0, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(-32, 0), warning = c(-33,-50), danger = c(-50, -100)
    ))

  })
  #--------Finance -> MOTOR-> MTPL  - > waterfall------------------
  output$plot211 <- renderPlotly({

    data13


    plot_ly(data13, x =~X1, y =~base, type = 'bar', color =I('steelblue4')) %>%
      add_trace(y = ~k1, marker=  list(color='rgba(1,1,1, 0.0)')) %>%
      add_trace(y = ~k2, marker = list(color = "rgb(204,204,204)")) %>%
      add_trace(y = ~k3, marker = list(color = "steelblue")) %>%
      add_trace(y = ~k4, marker = list(color = "steelblue")) %>%
      add_trace(y = ~k5, marker = list(color = "grey")) %>%
      add_trace(y = ~k6, marker = list(color = "thistle")) %>%
      add_trace(y = ~k7, marker = list(color = "mediumblue")) %>%
      layout(
        xaxis = list(title = "",tickfont = list(size = 8)),
        yaxis = list(title = "",tickfont = list(size = 8)),
        barmode = 'stack',
        height=200,

        paper_bgcolor = 'rgba(1,1,1, 0.0)',
        plot_bgcolor = 'rgba(1,1,1, 0.0)',
        showlegend = FALSE) %>%
      add_annotations(text="",
                      #x = x,
                      #y = y,
                      #xref = "x",
                      #yref = "y",
                      font = list(family = 'Arial',
                                  size = 14,
                                  color = 'rgba(1,1,1, 0.0)'),
                      showarrow = FALSE)

  })


  #------------Finance->MOTOR--->MTPL---> Plot Pie Claims Cost---------------

  output$plot711 <- renderPlotly({

    Cat11 = c("Paid", "Reserves","IBNR","Liquidation Cost","RUN OFF")
    valu11 = c("44.7","26.6","58.3","0.9","4.0")
    value11<-as.numeric(valu11)
    df111 = data.frame(Cat11, value11)
    df111

    colors <- c('salegray', 'rgb(204,204,204)', 'gray', 'plum', 'lightblue')
    plot_ly(df111, labels =~Cat11, values =~value11, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF',size = 8),
            hoverinfo = 'text',
            text = ~paste( value11, ' mln pln',Cat11),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        height=200)

  })

  #--------------Fianace  -----> MOTOR ----->MTPL-> Plot OP&CoR  ----------

  output$plot1_11 <- renderPlotly({

    ay <- list(
      tickfont = list(color = "black",size=8),
      overlaying = "y",
      side = "right",
      title = "%",
      legend = ("left"),
      size=10
    )
    plot_ly(wykres111) %>%
      add_lines(x =~MONTH, y =~COR_PREV_YEAR, name = "CoR PY", color=I("gray"),yaxis="y2") %>%
      add_lines(x =~MONTH, y =~COR_ACT_YEAR, name = "CoR AY", color=I("royalblue4"),yaxis="y2") %>%
      add_lines(x =~MONTH, y =~COR_PLAN, name = "CoR Plan", color=I("dodgerblue"),yaxis="y2") %>%
      add_bars( x=~MONTH, y =~OP_PREV_YEAR, name = "OP PY", color=I("gray")) %>%
      add_bars( x =~MONTH,y=~OP_ACT_YEAR, name = "OP AY", color=I("royalblue4")) %>%
      add_bars( x=~MONTH, y =~OP_PLAN, name = "OP Plan", color=I("steelblue3")) %>%

      layout(
        xaxis = list(title = "",tickfont = list(size = 8)),
        yaxis2=ay,
        yaxis = list(title="PLN mln",tickfont = list(size = 8)), height=250,
        legend=list(font =list(size=10), orientation='h')
      )

  })

#----------------slupek pion per prod MOTOR---


  data_113

  output$plot1113 <- renderPlotly({

    plot_ly(data_113, y = ~product, x = ~ay, type = 'bar', name = 'OP ay', marker = list(color = 'rgb(0, 64, 222)')) %>%
      add_trace(x = ~py, name = 'OP py', marker = list(color = 'rgb(204,204,204)')) %>%
      add_trace(x = ~plan, name = 'OP plan', marker = list(color = 'rgb(117, 174, 222)')) %>%
      layout(xaxis = list(title = "",tickfont = list(size = 8)),
             yaxis = list(title = "",tickfont = list(size = 10)),
             margin = list(b = 100),
             barmode = 'group', height=250,
             legend=list(font =list(size=10),orientation='h'))

  })


  #---------------------------------------------------------
  #---------------MOD----------------------------------

  #-----------------------FlexBoxy - Motor-----------
  output$flx_m12 <- flexdashboard::renderGauge({
    gauge(424.3, min = 0, max = 693.7, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(350, 2000), warning = c(300,349), danger = c(0, 299)
    ))

  })

  output$flx_m22 <- renderGauge({
    gauge(57.3, min = 0, max = 107, label = paste("mln PLN YTD"),gaugeSectors(
      success = c(43, 200), warning = c(35,42), danger = c(-30,34)
    ))

  })

  output$flx_m32 <- renderGauge({
    gauge(-85.8, min = -105, max =-89.0, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(-95, -50), warning = c(-100,-99), danger = c(-120,-102 )
    ))

  })

  output$flx_m42 <- renderGauge({
    gauge(-27.0, min = -100, max = -32.0, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(-32, 0), warning = c(-33,-50), danger = c(-50, -100)
    ))

  })

  #--------Finance -> MOTOR-> MOD  - > waterfall------------------
  output$plot2112 <- renderPlotly({

    data132


    plot_ly(data132, x =~X1, y =~base, type = 'bar', color =I('steelblue4')) %>%
      add_trace(y = ~k1, marker=  list(color='rgba(1,1,1, 0.0)')) %>%
      add_trace(y = ~k2, marker = list(color = "rgb(204,204,204)")) %>%
      add_trace(y = ~k3, marker = list(color = "steelblue")) %>%
      add_trace(y = ~k4, marker = list(color = "steelblue")) %>%
      add_trace(y = ~k5, marker = list(color = "grey")) %>%
      add_trace(y = ~k6, marker = list(color = "thistle")) %>%
      add_trace(y = ~k7, marker = list(color = "mediumblue")) %>%
      layout(
        xaxis = list(title = "",tickfont = list(size = 8)),
        yaxis = list(title = "",tickfont = list(size = 8)),
        barmode = 'stack',
        height=200,

        paper_bgcolor = 'rgba(1,1,1, 0.0)',
        plot_bgcolor = 'rgba(1,1,1, 0.0)',
        showlegend = FALSE) %>%
      add_annotations(text="",
                      #x = x,
                      #y = y,
                      #xref = "x",
                      #yref = "y",
                      font = list(family = 'Arial',
                                  size = 14,
                                  color = 'rgba(1,1,1, 0.0)'),
                      showarrow = FALSE)

  })



  #------------Finance->MOTOR--->MOD---> Plot Pie Claims Cost---------------

  output$plot7112 <- renderPlotly({

    Cat112 = c("Paid", "Reserves","IBNR","Liquidation Cost","RUN OFF")
    valu112 = c("121.7","49.6","34.3","0.6","0.4")
    value112<-as.numeric(valu112)
    df1112 = data.frame(Cat112, value112)
    df1112

    colors <- c('salegray', 'rgb(204,204,204)', 'gray', 'plum', 'lightblue')
    plot_ly(df1112, labels =~Cat112, values =~value112, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF',size = 8),
            hoverinfo = 'text',
            text = ~paste( value112, ' mln pln',Cat112),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        height=200)

  })

  #--------------Fianace  -----> MOTOR ----->MOD-> Plot OP&CoR  ----------

  output$plot1_112 <- renderPlotly({
    f<- list(size=8)
    ay <- list(
      tickfont = list(color = "black",size=6),
      overlaying = "y",
      side = "right",
      title = "%",
      titlefont=list(size=8),
      legend = ("left"),
      size=8
    )
    plot_ly(wykres1112) %>%
      add_lines(x =~MONTH, y =~COR_PREV_YEAR, name = "CoR PY", color=I("gray"),yaxis="y2") %>%
      add_lines(x =~MONTH, y =~COR_ACT_YEAR, name = "CoR AY", color=I("royalblue4"),yaxis="y2") %>%
      add_lines(x =~MONTH, y =~COR_PLAN, name = "CoR Plan", color=I("dodgerblue"),yaxis="y2") %>%
      add_bars( x=~MONTH, y =~OP_PREV_YEAR, name = "OP PY", color=I("gray")) %>%
      add_bars( x =~MONTH,y=~OP_ACT_YEAR, name = "OP AY", color=I("royalblue4")) %>%
      add_bars( x=~MONTH, y =~OP_PLAN, name = "OP Plan", color=I("steelblue3")) %>%

      layout(
        xaxis = list(title = "",tickfont = list(size = 8)),
        yaxis2=ay,
        yaxis = list(title="PLN mln",tickfont = list(size = 6),titlefont=f), height=250,
        legend=list(font =list(size=10),orientation='h')
      )

  })
  #----------------slupek pion per prod MOTOR---


  data_1132

  output$plot11132 <- renderPlotly({

    plot_ly(data_1132, y = ~product, x = ~ay, type = 'bar', name = 'OP ay', marker = list(color = 'rgb(0, 64, 222)')) %>%
      add_trace(x = ~py, name = 'OP py', marker = list(color = 'rgb(204,204,204)')) %>%
      add_trace(x = ~plan, name = 'OP plan', marker = list(color = 'rgb(117, 174, 222)')) %>%
      layout(xaxis = list(title = "",tickfont = list(size = 8)),
             yaxis = list(title = "",tickfont = list(size = 10)),
             margin = list(b = 100),
             barmode = 'group', height=250,
             legend=list(font =list(size=10),orientation='h'))

  })



  #--------------------Claim--------------------------
  #-----------------------FlexBoxy - Claim Overview-----------
  output$flx_c1 <- flexdashboard::renderGauge({
    gauge(43.2, min = 0, max =46.7, symbol="%", label = paste("YTD"),gaugeSectors(
      success = c(0, 44.2), warning = c(44.2,45), danger = c(45.1, 46.7)
    ))

  })

  output$flx_c2 <- renderGauge({
    gauge(745.7, min = 0, max = 1397.9, label = paste("k YTD "),gaugeSectors(
      success = c(811.1, 1397.9), warning = c(711.1,811.1), danger = c(0,711.1)
    ))

  })

  output$flx_c3 <- renderGauge({
    gauge(5.6, min = 0, max =5.2, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(0,5.1), warning = c(5.2,6), danger = c(6,10 )
    ))

  })

  output$flx_c4 <- renderGauge({
    gauge(274.2, min = 0, max = 496.3, label = paste("mln YTD"),gaugeSectors(
      success = c(0, 269.7), warning = c(269.8,299.7), danger = c(239.8, 369.7)
    ))

  })

  #-----------------claims-> overview->slupek per prod


  claim1

  output$claim1 <- renderPlotly({

    plot_ly(claim1, y = ~Product, x = ~ay, type = 'bar', name = 'AY', marker = list(color = 'rgb(0, 64, 222)')) %>%
      add_trace(x = ~py, name = 'PY', marker = list(color = 'rgb(204,204,204)')) %>%
      add_trace(x = ~plan, name = 'Plan', marker = list(color = 'rgb(117, 174, 222)')) %>%
      layout(xaxis = list(title = "",tickfont = list(size = 8)),
             yaxis = list(title = "",tickfont = list(size = 10)),
             margin = list(b = 100),
             barmode = 'group', height=350,
             legend=list(font =list(size=10),orientation='h'), height=200)

  })

  #--------------Claim  -----> Overwiew ----->loss ratio  ----------



  claim2

  output$claim2 <- renderPlotly({
    f<- list(size=8)
    ay <- list(
      tickfont = list(color = "black",size=8),
      overlaying = "y",
      side = "right",
      title = "mln",
      titlefont=list(size=8),
      legend = ("left")

    )
    plot_ly(claim2) %>%
      add_bars( x=~Product, y =~Number, name = "Number of Claims", color=I("navy")) %>%
      add_lines( x =~Product,y=~Value, name = "Claims Value", color=I("steelblue"),yaxis="y2",fill='tonexty') %>%

      layout(xaxis = list(title = "",tickfont = list(size = 10)),
             yaxis2=ay,
             yaxis = list(title="Number",titlefont=f,tickfont = list(size = 8)),
             legend = list(x = 0.1, y = 0.9,font =list(size=10)), height=300
      )

  })

  #-----------------claims_overview---tab1

  output$tab1 <- function(){


    claim3

    claim3 %>%
      knitr::kable("html") %>%
      kable_styling(font_size = 12)%>%
      row_spec(0, bold = TRUE, color = "black", font_size = 14)%>%
      row_spec(1:3, bold = F, color = "black")
    #%>%
    # row_spec(1, bold = F, color = "navy")


  }

  #--------------claims-overview-tab2

  output$tab2 <- function(){


    claim4

    claim4 %>%
      knitr::kable("html") %>%
      kable_styling(font_size = 12)%>%
      row_spec(0, bold = TRUE, color = "black", font_size = 14)%>%
      row_spec(1:3, bold = F, color = "black")
    #%>%
    # row_spec(1, bold = F, color = "navy")


  }


  #-----------------------FlexBoxy - Claim MTPL-----------
  output$flx_e11 <- flexdashboard::renderGauge({
    gauge(33.3, min = 0, max = 38.9, symbol="%", label = paste("YTD"),gaugeSectors(
      success = c(0, 33), warning = c(33,35), danger = c(35, 38.9)
    ))

  })

  output$flx_e21 <- renderGauge({
    gauge(315.7, min = 0, max = 614.7, label = paste("k YTD"),gaugeSectors(
      success = c(356.8, 614.731), warning = c(306.824,356.823), danger = c(0, 306.823)
    ))

  })

  output$flx_e31 <- renderGauge({
    gauge(4.6, min = 0, max =4.7, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(0, 4.5), warning = c(4.6,6.4), danger = c(6.5,10 )
    ))

  })

  output$flx_e41 <- renderGauge({
    gauge(78.2, min = 0, max = 173.3, label = paste("mln YTD"),gaugeSectors(
      success = c(0, 84.190463), warning = c(84.190464,874.190462), danger = c(87.190463, 104.190463)
    ))

  })



#--------------Claim  -----> MTPL ----->loss ratio /avg ----------



  output$claim5 <- renderPlotly({

    claim5a
    f<-list(size=8)
    #  t <- list(
    #   family = "sans serif",
    #   size = 14,
    #  color = toRGB("grey50"))

    ay <- list(
      tickfont = list(color = "black",size = 6),
      overlaying = "y",
      side = "right",
      title = "",
      legend = ("left")
    )
    plot_ly(claim5a) %>%
      add_lines(x =~month, y =~LR2017, name = "LR PY", color=I("gray"),yaxis="y2") %>%
      add_lines(x =~month, y =~LR2018, name = "LR AY", color=I("royalblue4"),yaxis="y2") %>%
      add_lines(x =~month, y =~LRPLAN, name = "LR Plan", color=I("dodgerblue"),yaxis="y2") %>%
      add_bars( x=~month, y =~AVG2017, name = "AVG CLAIMS PY", color=I("gray")) %>%
      add_bars( x =~month,y=~AVG2018, name = "AVG CLAIMS AY", color=I("royalblue4")) %>%
      add_bars( x=~month, y =~AVGPLAN, name = "AVG CLAIMS Plan", color=I("steelblue3")) %>%
      layout(
        title = "", yaxis2=ay,
        xaxis=list(tickfont = list(size = 6)),
        yaxis = list(title="PLN",
                     range=c(4000,6500),
                     tickfont = list(size = 6),titlefont=f),height=240,
        legend=list(font =list(size=10),orientation='h')
      )


  })

  #--------------Claim  -----> MTPL ----->settelment ----------


  output$claim6 <- renderPlotly({

    claim6a


    plot_ly(claim6a, x = ~X1, y = ~k11, type = 'bar',
            color =I('steelblue')) %>%
      add_trace(y = ~base, marker=  list(color='rgba(1,1,1, 0.0)')) %>%
      add_trace(y = ~k12,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k13,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k14,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k15,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k16,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k17, marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k18,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k19,  marker = list(color = "sandybrown")) %>%
      layout(title = '',
             xaxis = list(title = "", size=8,tickfont = list(size = 8)),
             yaxis = list(title = "",tickfont = list(size = 8)),
             barmode = 'stack',
             paper_bgcolor = 'rgba(1,1,1, 0.0)',
             plot_bgcolor = 'rgba(1,1,1, 0.0)',
             showlegend = FALSE, height=200) %>%
      add_annotations(text="",
                      font = list(family = 'Arial',
                                  size = 8,
                                  color = 'rgba(1,1,1, 0.0)'),
                      showarrow = FALSE)
  })

  #--------------Claim  -----> MTPL -----avg claims daily ----------



  #30dni
  output$claim1_0 <- renderPlotly({


    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = " ",
      legend = ("left")
    )
    plot_ly(dane_daym30) %>%

	#plot_ly(dane_daym30,x = ~dzien, y = ~MTPL_2018, name=2018, color=I("dodgerblue"), type='bar', width=0.8) %>%
      add_lines(x = ~dzien, y = ~MTPL_2017, name =2017,fill='tonexty', color=I('skyblue1')) %>%
     add_lines(x = ~dzien, y = ~MTPL_2018, name=2018, color=I("dodgerblue"), fill='tonexty') %>%



      layout(
        title = "",
        xaxis = list(title='Day',tickfont = list(size = 8)),
        yaxis = list(title='',
                     range=c(5000,5500),
                     tickfont = list(size = 8))
      )
  })


  #-----------------------FlexBoxy - Claim CASCO-----------
  output$flx_d12 <- flexdashboard::renderGauge({
    gauge(50.7, min = 0, max = 54.7, symbol="%", label = paste("YTD"),gaugeSectors(
      success = c(0, 54.6), warning = c(54.7,54.7), danger = c(54.7, 54.7)
    ))

  })

  output$flx_d22 <- renderGauge({
    gauge(182110, min = 0, max = 299897, label = paste(" "),gaugeSectors(
      success = c(175343, 185343), warning = c(135343,175342), danger = c(0, 135342)
    ))

  })

  output$flx_d32 <- renderGauge({
    gauge(14.8, min = 0, max =14.6, symbol = '%', label = paste("YTD"),gaugeSectors(
      success = c(0, 15), warning = c(15,18), danger = c(18,20 )
    ))

  })

  output$flx_d42 <- renderGauge({
    gauge(195904287, min = 0, max = 322737559, label = paste("mln YTD"),gaugeSectors(
      success = c(0, 185393042), warning = c(185393042,1295393041), danger = c(295393042, 385393042)
    ))

  })

  #--------------Claim  -----> CASCO ----->loss ratio /avg ----------



  output$claim7 <- renderPlotly({

    claim7a

    #  t <- list(
    #   family = "sans serif",
    #   size = 14,
    #  color = toRGB("grey50"))
    f<-list(size=8)
    ay <- list(
      tickfont = list(color = "black",size = 6),
      overlaying = "y",
      side = "right",
      title = "",
      legend = ("left")
    )
    plot_ly(claim7a) %>%
      add_lines(x =~month, y =~LR2017, name = "LR PY", color=I("gray"),yaxis="y2") %>%
      add_lines(x =~month, y =~LR2018, name = "LR AY", color=I("royalblue4"),yaxis="y2") %>%
      add_lines(x =~month, y =~LRPLAN, name = "LR Plan", color=I("dodgerblue"),yaxis="y2") %>%
      add_bars( x=~month, y =~AVG2017, name = "AVG CLAIMS PY", color=I("gray")) %>%
      add_bars( x =~month,y=~AVG2018, name = "AVG CLAIMS AY", color=I("royalblue4")) %>%
      add_bars( x=~month, y =~AVGPLAN, name = "AVG CLAIMS Plan", color=I("steelblue3")) %>%
      layout(
        title = "", yaxis2=ay,
        xaxis=list(tickfont = list(size = 6)),
        yaxis = list(title="PLN",
                     range=c(5500,8500),
                     tickfont = list(size = 8),titlefont=f),height=240,
        legend=list(font =list(size=10),orientation='h')
      )


  })

  #--------------Claim  -----> CASCO ----->settelment ----------


  output$claim8 <- renderPlotly({

    claim8a


    plot_ly(claim8a, x = ~X1, y = ~k1, type = 'bar',# text = y1a, textposition = 'auto',
            color =I('sandybrown')) %>%
      add_trace(y = ~base, marker=  list(color='rgba(1,1,1, 0.0)')) %>%
      add_trace(y = ~k2,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k3,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k4,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k5,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k6, marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k7,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k8,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k9,  marker = list(color = "sandybrown")) %>%
      add_trace(y = ~k10,  marker = list(color = "steelblue")) %>%

      layout(title = '',
             xaxis = list(title = "",size=8,tickfont = list(size = 7)),
             yaxis = list(title = "",tickfont = list(size = 8)),
             barmode = 'stack',
             paper_bgcolor = 'rgba(1,1,1, 0.0)',
             plot_bgcolor = 'rgba(1,1,1, 0.0)',
             showlegend = FALSE,height=180) %>%
      add_annotations(text="",
                      #  x = x,
                      #  y = y,
                      # xref = "x",
                      #  yref = "y",
                      font = list(family = 'Arial',
                                  size = 8,
                                  color = 'rgba(1,1,1, 0.0)'),
                      showarrow = FALSE)
  })
  #--------------Claim  -----> MOD -----avg claims daily ----------



  #30dni
  output$claim1_1 <- renderPlotly({


    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = " ",
      legend = ("left")
    )
    plot_ly(dane_day30) %>%
      add_lines(x = ~dzien, y = ~CASCO_2017, name =2017,fill='tonexty', color=I('skyblue1')) %>%
      add_lines(x = ~dzien, y = ~CASCO_2018, name=2018, color=I("dodgerblue"),  fill='tonexty') %>%


      layout(
        title = "",
        xaxis = list(title='Day',tickfont = list(size = 8)),
        yaxis = list(title='',
                     range=c(6500,7500),
                     tickfont = list(size = 8))
      )
  })





}

# Run the application
shinyApp(ui = ui, server = server)

