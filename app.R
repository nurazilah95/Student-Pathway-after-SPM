#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(repr)
library(DiagrammeR)
library(tidyr)
rank_uni <- read.csv("Malaysia_Uni.csv")

rank_uni$Institution <- as.character(rank_uni$Institution)
rank_uni$Country <- as.character(rank_uni$Country)
rank_uni$World.Rank <- as.factor(rank_uni$World.Rank)
ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "U-Beginner"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("About U-Beginner", tabName = "abt", icon = icon("book")),
            menuItem("Pathway Overview", tabName = "flowchart", icon = icon(name = "sitemap", lib = "font-awesome")),
            menuItem("Institutions Finder", tabName = "list", icon = icon("list-alt")),
            menuItem("World Ranking", tabName = "table", icon = icon("table"))
           # menuItem("Plot", tabName = "plot", icon = icon("bar-chart")),
            
            
        )
    ),
   
    ## Body content
    dashboardBody(
    tabItems(
            
            # tab content on List of Institutions in Malaysia
            tabItem(tabName = "list",
                fluidPage(
                  titlePanel((h1(strong(("Institutions Finder")),align="center"))),
                  helpText(h5(strong(
                    "What are your options after SPM?",br(), br(),
                    "Choosing the right institution and course may be the most important decision you'll have to make in the near future.",br(), br(),
                    "Have a look at the list of Institution right here!",br(), br(),
                    "You will be directed to the Instituition website by clicking the name"), style="color:black",align="center")),
                  
                  # Create a new Row in the UI for selectInputs
                  
                  fluidRow(
                    
                    
                    column(6,
                           selectInput("select1",
                                       "Field of Study",
                                       c("Diploma","Foundation","Others")))
                    ,
                    column(6,
                           selectInput("select2",
                                       "Institution",
                                       c("IPTA","IPTS", "Others")))),
                  tags$style("body{background-color: white; color:black}"),
                  
                  uiOutput("url",align="center")
                    
                    
                   
                )

            ),
        
            
            # tab content on Table World Ranking
            tabItem(tabName = "table",
                fluidPage(
                    titlePanel(h1(strong("World Ranking"), align = "center")),
                    helpText(h5(strong(
                      "University rankings are of the resources to help students find the right colleges for you.",br(), br(),
                      "Rankings acts as a quick reputation checker.",br(), br(),
                      "Here is the list of Malaysia University ranking for your guidance!"), style="color:black",align="center")),
                ),
                    
                    
                fluidRow(
                    # 1st filter for Institution
                    column(width = 4, 
                           selectInput("Institution",
                                                  "Institution: ",
                                                  c("All",
                                                    as.character(rank_uni$Institution)))
                    ),
                    # 2nd filter for Country
                    column(width = 4, 
                           selectInput("Country",
                                       "Country:",
                                       c("All",
                                         as.character(rank_uni$Country)))
                               
                    ),
                    # 3rd filter for Years
                    column(width = 4, 
                           selectInput("Year",
                                       "Year: ",
                                       c("All","2020", "2019","2018", "2017", "2016", "2015", "2014", "2013", "2012",
                                         "2011", "2010", "2009", "2008", "2007", "2006", "2005", "2004",
                                         as.integer(rank_uni$Year)))
                           
                    )
                ),                                    
                
                # Create a new row for the table
                DT::dataTableOutput("table"),                                    
            
            ),
                    
            # tab content on Flow Chart
            tabItem(tabName = "flowchart",
            fluidPage(
              titlePanel(h1(strong("Pathway Overview"), align = "center")),
              helpText(h5(strong(
                "You might have people around you telling you what you should study or which",br(), br(),
                "intitution in Malaysia you should choose, or done some research yourself.",br(), br(),
                "But you are still not sure where to start?",br(), br(),
                "Feel free to refer to our flow chart for a head start!"), style="color:black",align="center")),
              grVizOutput("dg", width = "100%", height = "700px"),
              verbatimTextOutput("print")
            ),
  

                    ),
            
            # tab content on About
            tabItem(tabName = "abt",
            fluidPage(
              titlePanel(h1(strong("About U-Beginner"), align = "center")),
              div(includeMarkdown("about.Rmd"), align = "justify")
            )
                    
                    ) 
            
            

            
                ) # Tab items end bracket
    ) # dashboard body end bracket
) # dashboard page end bracket

library(ggplot2)          

server <- function(input, output) {
  



  # List of Institutions
  output$url<-renderUI({
    
    schoolType <- input$select1
    sectorType <- input$select2
    
    ##Foundation IPTA
    
    if(schoolType=="Foundation" && sectorType=="IPTA")
      
      
      body <- dashboardBody(
        fluidRow(
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://puspa.unisza.edu.my/index.php?option=com_content&view=article&id=228&Itemid=723&lang=en", h4(strong("UniSZA"))),
                   br(),
                   img(src = "UniSZA.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://asasi.uitm.edu.my/web/", h4(strong("UiTM"))),
                   br(),
                   img(src = "UiTM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.umt.edu.my/", h4(strong("UMT"))),
                   br(),
                   img(src = "UMT.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.ums.edu.my/ppstv2/", h4(strong("UMS"))),
                   br(),
                   img(src = "UMS.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.ukm.my/geniuspintar/asasipintar-pre-univ/", h4(strong("UKM"))),
                   br(),
                   img(src = "UKM.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.um.edu.my/foundation", h4(strong("UM"))),
                   br(),
                   img(src = "UM.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://esmp.upm.edu.my/promosi/", h4(strong("UPM"))),
                   br(),
                   img(src = "UPM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.usim.edu.my/admission/tamhidi/", h4(strong("USIM"))),
                   br(),
                   img(src = "USIM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://space.utm.my/foundation/", h4(strong("UTM"))),
                   br(),
                   img(src = "UTM.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://asasi.uum.edu.my/index.php", h4(strong("UUM"))),
                   br(),
                   img(src = "UUM.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.unimas.my/pre-u-programmes-a-z", h4(strong("UNIMAS"))),
                   br(),
                   img(src = "UNIMAS.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.iium.edu.my/programme/index/foundation/6", h4(strong("UIAM"))),
                   br(),
                   img(src = "UIAM.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.upnm.edu.my/en/index.php?req=26", h4(strong("UPNM"))),
                   br(),
                   img(src = "UPNM.png",width="150px",height="150px") 
                 )
                 
                 
          )
        )
      )
    
    ##Foundation IPTS
    
    else if (schoolType=="Foundation" && sectorType=="IPTS")
      
      
      body <- dashboardBody(
        fluidRow(
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.atc2u.edu.my/our-courses/", h4(strong("ATC College"))),
                   br(),
                   img(src = "ATC.PNG",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.aimst.edu.my/program/pre-university/", h4(strong("AIMST University"))),
                   br(),
                   img(src = "AIMST.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.amu.edu.my/#", h4(strong("AMU"))),
                   br(),
                   img(src = "AMU.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.apu.edu.my/our-courses/pre-university-studies/foundation-programme", h4(strong("APU"))),
                   br(),
                   img(src = "APU.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.berjaya.edu.my/university/academic-programmes/", h4(strong("BERJAYA"))),
                   br(),
                   img(src = "Berjaya.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://courses.curtin.edu.my/courses-and-study/foundation-studies/", h4(strong("Curtin University"))),
                   br(),
                   img(src = "Curtin.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://cyberjaya.edu.my/", h4(strong("CUCMS"))),
                   br(),
                   img(src = "CUCMS.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.disted.edu.my/programmes/", h4(strong("DISTED College"))),
                   br(),
                   img(src = "DISTED.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://firstcity.edu.my/category/programmes/", h4(strong("FIRST CITY"))),
                   br(),
                   img(src = "FCU.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.gmi.edu.my/pre-university-programme/german-a-level-preparatory-programme-gapp/", h4(strong("GMI"))),
                   br(),
                   img(src = "GMI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://hcu.edu.my/course-detail/foundation-mass-communication", h4(strong("Han Chiang"))),
                   br(),
                   img(src = "Han Chiang.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://college.help.edu.my/all-courses/?wpv-wpcf-course-area-of-study=Foundation+Studies&wpv_aux_current_post_id=528&wpv_view_count=511-TCPID528", h4(strong("HELP CAT"))),
                   br(),
                   img(src = "HELP_CAT.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://search.hw.ac.uk/s/search.html?collection=malaysia-courses&f.Level%7Ccourselevel=Degree+Entry+%28Foundation%29", h4(strong("Heriot-Watt Uni"))),
                   br(),
                   img(src = "Heriot-Watt.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://iact.edu.my/foundation-in-media-studies/", h4(strong("IACT College"))),
                   br(),
                   img(src = "IACT.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://imperia.edu.my/", h4(strong("Imperia College"))),
                   br(),
                   img(src = "Imperia.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://intec.edu.my/home#", h4(strong("INTEC Edu College"))),
                   br(),
                   img(src = "intec.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.icom.edu.my/program_foundation_in_music.php", h4(strong("ICOM"))),
                   br(),
                   img(src = "ICOM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.imu.edu.my/imu/programmes/pre-university/foundation-in-science/", h4(strong("IMU"))),
                   br(),
                   img(src = "IMU.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://newinti.edu.my/academic-programmes/undergraduate/foundation/", h4(strong("INTI"))),
                   br(),
                   img(src = "INTI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://iukl.edu.my/programmes/foundation/", h4(strong("IUKL"))),
                   br(),
                   img(src = "IUKL.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://iumw.edu.my/arts/foundation_in_arts/", h4(strong("IUMW"))),
                   br(),
                   img(src = "IUMW.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://klmuc.edu.my/apply-now/", h4(strong("KLMU"))),
                   br(),
                   img(src = "KLMU.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.kysnz.edu.my/home/foundation-in-commerce-draft/", h4(strong("KYS School"))),
                   br(),
                   img(src = "KYS.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.limkokwing.net/cambodia/academic/courses/", h4(strong("Limkokwing Uni"))),
                   br(),
                   img(src = "LKW.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://mahsa.edu.my/faculties/Pre-University/foundation-science.php", h4(strong("MAHSA University"))),
                   br(),
                   img(src = "MAHSA.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.msu.edu.my/programme", h4(strong("MSU"))),
                   br(),
                   img(src = "MSU.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://miu.edu.my/course-listing?field_course_level_tid=9", h4(strong("Manipal Uni"))),
                   br(),
                   img(src = "Manipal.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.manipal.edu.my/manipal-mmmc/admissions/fis-programme.html", h4(strong("MMMC"))),
                   br(),
                   img(src = "MMMC.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.mckl.edu.my/course/13/Australian-Matriculation-(AUSMAT)/", h4(strong("Methodist College"))),
                   br(),
                   img(src = "MMU.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.mmu.edu.my/programmes/", h4(strong("Multimedia Uni"))),
                   br(),
                   img(src = "MMU.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.ncl.ac.uk/numed/study/undergraduate/foundation/#courseoverview", h4(strong("Newcastle Uni"))),
                   br(),
                   img(src = "Newcastle.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.olympia.edu.my/index.php/what-to-study/business/foundation-in-business/", h4(strong("Olympia College"))),
                   br(),
                   img(src = "Olympia.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.perdanauniversity.edu.my/programmes/#1495782292446-4b685137-e4ab", h4(strong("Perdana Uni"))),
                   br(),
                   img(src = "PERDANA.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.point.edu.my/index.php/courses/foundation-in-design", h4(strong("Point College"))),
                   br(),
                   img(src = "Point.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://raffles-university.edu.my/#", h4(strong("Raffles University"))),
                   br(),
                   img(src = "Raffles.png",width="150px",height="150px")
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.rcsiucd.edu.my/programme/foundation-in-science/", h4(strong("RCSI & UCD"))),
                   br(),
                   img(src = "RCSI_UCD.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.reliance.edu.my/courses/", h4(strong("Reliance College"))),
                   br(),
                   img(src = "Reliance.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.segi.edu.my/visit-segi/segi-college-penang", h4(strong("SEGI Penang"))),
                   br(),
                   img(src = "SEGI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://segi.edu.my/sarawak/#", h4(strong("SEGI Sarawak"))),
                   br(),
                   img(src = "SEGI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.segi.edu.my/programmes/pre-u-foundation/", h4(strong("SEGI Uni"))),
                   br(),
                   img(src = "SEGI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://college.sunway.edu.my/programmes/pre-u", h4(strong("Sunway College"))),
                   br(),
                   img(src = "UNITAR.jpeg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://sunway.edu.my/ipoh/Foundation-in-Arts", h4(strong("Sunway Ipoh"))),
                   br(),
                   img(src = "Sun_ipoh.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.sunway.edu.my/jb/programmes/pre-university", h4(strong("Sunway College JB"))),
                   br(),
                   img(src = "SUNWAY_JB.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://college.taylors.edu.my/en/study/foundation-programmes.html", h4(strong("Taylor's College"))),
                   br(),
                   img(src = "Taylor_s.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://toa.edu.my/programmes/foundation/index.php?menuheaders=2", h4(strong("The One Academy"))),
                   br(),
                   img(src = "TOA.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.tmc.edu.my/foundation-in-science/", h4(strong("TMC College"))),
                   br(),
                   img(src = "TMC.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.tarc.edu.my/admissions/programmes/programme-offered-a-z/pre-university-programme/", h4(strong("TAR UC"))),
                   br(),
                   img(src = "TARC.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.ucsiuniversity.edu.my/application-guidelines", h4(strong("UCSI College"))),
                   br(),
                   img(src = "UCSI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.unikl.edu.my/programmes/foundation/", h4(strong("UniKL"))),
                   br(),
                   img(src = "UniKL.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://unitar.my/undergraduate-foundation", h4(strong("UNITAR"))),
                   br(),
                   img(src = "UNITAR.jpeg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.nottingham.edu.my/Study/Foundation-courses/UG-Foundation-courses.aspx", h4(strong("Nottingham Uni"))),
                   br(),
                   img(src = "NOTTINGHAM.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://admission.utar.edu.my/How_To_Apply.php", h4(strong("UTAR"))),
                   br(),
                   img(src = "UTAR.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.uniten.edu.my/programmes/foundation-programmes/", h4(strong("UNITEN"))),
                   br(),
                   img(src = "UNITEN.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.unirazak.edu.my/?page_id=614", h4(strong("UNIRAZAK"))),
                   br(),
                   img(src = "UNIRAZAK.png",width="150px",height="150px")
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.southampton.ac.uk/my/foundation_programme/index.page", h4(strong("University of Southampton"))),
                   br(),
                   img(src = "SOUTHAMPTON.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.uowmkdu.edu.my/programmes/pre-university-studies/", h4(strong("UOWM KDU University"))),
                   br(),
                   img(src = "UOW.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.westminster.edu.my/courses/degree-foundation", h4(strong("Westminster Intl College"))),
                   br(),
                   img(src = "WIC.jpg",width="150px",height="150px") 
                 )
                 
                 
          )
        )
      )
    
    
    ##Diploma IPTA
    
    else if (schoolType=="Diploma" && sectorType=="IPTA")
      
      
      body <- dashboardBody(
        fluidRow(
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.upsi.edu.my/prospective-student-ii/", h4(strong("UPSI"))),
                   br(),
                   img(src = "UPSI.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.uthm.edu.my/en/programs/diploma", h4(strong("UTHM"))),
                   br(),
                   img(src = "UTHM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.utem.edu.my/admission/list-of-programmes.html", h4(strong("UTeM"))),
                   br(),
                   img(src = "UTeM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.unisza.edu.my/index.php?option=com_content&view=article&id=898:diploma&catid=42&Itemid=807&lang=ms", h4(strong("UniSZA"))),
                   br(),
                   img(src = "UniSZA.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://online.uitm.edu.my/diploma.cfm", h4(strong("UiTM"))),
                   br(),
                   img(src = "UiTM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.umt.edu.my/index.php?go=ak_pra", h4(strong("UMT"))),
                   br(),
                   img(src = "UMT.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.ums.edu.my/fpsk/en/academic/academic/undergraduate/nursing-program/nursing", h4(strong("UMS"))),
                   br(),
                   img(src = "UMS.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://umpadvanced.ump.edu.my/diploma_must.php", h4(strong("UMP"))),
                   br(),
                   img(src = "UMP.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://umcced.edu.my/programme/diploma-full-time", h4(strong("UM"))),
                   br(),
                   img(src = "UM.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://upm.edu.my/akademik/prasiswazah/programmes_courses-8252?L=en", h4(strong("UPM"))),
                   br(),
                   img(src = "UPM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.unimap.edu.my/index.php/ms/senarai-program-pengajian-ditawarkan-sidang-akademik-2019-2020", h4(strong("UniMAP"))),
                   br(),
                   img(src = "UniMAP.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.admissions.usm.my/index.php/undergraduate/program-pengajian/program-diploma", h4(strong("USM"))),
                   br(),
                   img(src = "USM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://space.utm.my/diploma/", h4(strong("SPACE UTM"))),
                   br(),
                   img(src = "UTM_SPACE.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.upnm.edu.my/mobile/index.php?req=26", h4(strong("UPNM"))),
                   br(),
                   img(src = "UPNM.png",width="150px",height="150px") 
                 )
                 
                 
          )
        )
      )
    
    ##Diploma IPTS
    
    
    else if (schoolType=="Diploma" && sectorType=="IPTS")
      
      
      body <- dashboardBody(
        fluidRow(
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.atc2u.edu.my/our-courses/", h4(strong("ATC College"))),
                   br(),
                   img(src = "ATC.PNG",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.aimst.edu.my/program/pre-university/", h4(strong("AIMST University"))),
                   br(),
                   img(src = "AIMST.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.alfa.edu.my/our-programmes/", h4(strong("ALFA Int"))),
                   br(),
                   img(src = "ALFA.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.amu.edu.my/#", h4(strong("AMU"))),
                   br(),
                   img(src = "AMU.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.apu.edu.my/our-courses/pre-university-studies/diploma-programmes", h4(strong("APU"))),
                   br(),
                   img(src = "APU.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.berjaya.edu.my/college/our-programmes/", h4(strong("BERJAYA TVET"))),
                   br(),
                   img(src = "BERJAYA TVET.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.berjaya.edu.my/university/academic-programmes/", h4(strong("BERJAYA"))),
                   br(),
                   img(src = "Berjaya.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.crescendo.edu.my/site/pages/111/Diploma_in_Business", h4(strong("Crescendo College"))),
                   br(),
                   img(src = "CRESCENDO.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://cyberjaya.edu.my/", h4(strong("CUCMS"))),
                   br(),
                   img(src = "CUCMS.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://cyberlynx.edu.my/admissions/", h4(strong("Cyberlynx Int"))),
                   br(),
                   img(src = "Cyberlynx.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.disted.edu.my/programmes/", h4(strong("DISTED College"))),
                   br(),
                   img(src = "DISTED.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.dhuautomotive.edu.my/foe-programme-offered/", h4(strong("DRB-HICOM University"))),
                   br(),
                   img(src = "DRB-HICOM.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.edufly.edu.my/our-courses/", h4(strong("Edufly"))),
                   br(),
                   img(src = "Edufly.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.equator.edu.my/courses/school-of-built-environment/", h4(strong("Equator Academy"))),
                   br(),
                   img(src = "EQUATOR.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://college.erican.edu.my/programmes-overview/", h4(strong("Erican College"))),
                   br(),
                   img(src = "ERICAN.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://ftms.edu.my/v2/school/accounting-business-management/", h4(strong("FTMS"))),
                   br(),
                   img(src = "FTMS.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.gmi.edu.my/diploma-programmes/", h4(strong("GMI"))),
                   br(),
                   img(src = "GMI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://hcu.edu.my/programs.php", h4(strong("Han Chiang College"))),
                   br(),
                   img(src = "Han Chiang.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://academy.help.edu.my/all-courses/?wpv-wpcf-course-level-of-study=Diploma&wpv_view_count=511", h4(strong("Help Academy"))),
                   br(),
                   img(src = "HELPUniversity.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://college.help.edu.my/all-courses/?wpv-wpcf-course-level-of-study=Diploma&wpv_view_count=511", h4(strong("HELP CAT"))),
                   br(),
                   img(src = "HELP_CAT.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://iact.edu.my/programmes/", h4(strong("IACT College"))),
                   br(),
                   img(src = "IACT.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://imperia.edu.my/", h4(strong("Imperia College"))),
                   br(),
                   img(src = "Imperia.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://innovative.edu.my/diploma-programme-2/", h4(strong("Innovative Intl College"))),
                   br(),
                   img(src = "INNOVATIVE.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.inpens.edu.my/kursus.php", h4(strong("INPENS Intl College"))),
                   br(),
                   img(src = "INPENS.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://intec.edu.my/programmes#", h4(strong("INTEC Edu College"))),
                   br(),
                   img(src = "intec.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.icom.edu.my/dis-production.php", h4(strong("ICOM"))),
                   br(),
                   img(src = "ICOM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.icym.edu.my/v14f/", h4(strong("ICYM"))),
                   br(),
                   img(src = "ICYM.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.imc.edu.my/diploma-in-nursing/", h4(strong("IMC"))),
                   br(),
                   img(src = "IMC.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://newinti.edu.my/academic-programmes/undergraduate/diploma", h4(strong("INTI College"))),
                   br(),
                   img(src = "INTI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.dika.edu.my/category/courses/diploma/", h4(strong("Kolej DIKA"))),
                   br(),
                   img(src = "DIKA.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.kpjuc.edu.my/programme/", h4(strong("KPJUC"))),
                   br(),
                   img(src = "KPJHUniversity.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.limkokwing.net/malaysia/academic/courses/", h4(strong("LKW"))),
                   br(),
                   img(src = "LKW.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.linton.edu.my/Programs/Diploma", h4(strong("Linton"))),
                   br(),
                   img(src = "Linton.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://mahsa.edu.my/", h4(strong("MAHSA University"))),
                   br(),
                   img(src = "MAHSA.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://must.edu.my/core-programmes/", h4(strong("MUST"))),
                   br(),
                   img(src = "MUST.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://mia.edu.my/programmes/", h4(strong("MIA"))),
                   br(),
                   img(src = "MIA.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.mib.edu.my/our-programs/", h4(strong("MIB"))),
                   br(),
                   img(src = "MIB.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.msu.edu.my/programme", h4(strong("MSU"))),
                   br(),
                   img(src = "MSU.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.mckl.edu.my/course/17/Diploma-in-Early-Childhood-Education/", h4(strong("Methodist College"))),
                   br(),
                   img(src = "Methodist.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.nmit.edu.my/academic/our-programmes/", h4(strong("NMIT"))),
                   br(),
                   img(src = "NMIT.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.newera.edu.my/course_details.php?acalevel=3", h4(strong("New Era"))),
                   br(),
                   img(src = "NEW_ERA.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.nilai.edu.my/diploma", h4(strong("Nilai University"))),
                   br(),
                   img(src = "Nilai.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.olympia.edu.my/index.php/what-to-study/", h4(strong("Olympia College"))),
                   br(),
                   img(src = "Olympia.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.pia.edu.my/courses/", h4(strong("Pia College"))),
                   br(),
                   img(src = "PIA.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.point.edu.my/index.php/courses/diploma-in-film-tv-production", h4(strong("Point College"))),
                   br(),
                   img(src = "Point.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://putraintelek.edu.my/en/", h4(strong("Putra Intelek"))),
                   br(),
                   img(src = "PutraIntelek.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.qiup.edu.my/academics/programmes/", h4(strong("QIUP"))),
                   br(),
                   img(src = "QIUP.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://raffles.edu.my/department/diploma/", h4(strong("Raffles College"))),
                   br(),
                   img(src = "Raffles.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://raffles-university.edu.my/", h4(strong("Raffles University"))),
                   br(),
                   img(src = "Raffles.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.ramsaysimedarbycollege.edu.my/programmes/", h4(strong("Ramsay College"))),
                   br(),
                   img(src = "RAMSAY.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.reliance.edu.my/course/diploma-in-business-management/", h4(strong("Reliance"))),
                   br(),
                   img(src = "Reliance.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.reliance.edu.my/courses/diploma/", h4(strong("Reliance (Penang)"))),
                   br(),
                   img(src = "Reliance.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://saito.edu.my/schools/", h4(strong("Saito"))),
                   br(),
                   img(src = "SAITO.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.segi.edu.my/programmes/", h4(strong("SEGI College"))),
                   br(),
                   img(src = "SEGI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.segi.edu.my/study-with-us/", h4(strong("SEGI Uni&Col"))),
                   br(),
                   img(src = "SEGI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://mktg41.wixsite.com/seristamfordcollege/school-of", h4(strong("Seri Stamford"))),
                   br(),
                   img(src = "SeriStamford.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.southern.edu.my/sra/diploma.html", h4(strong("Southern Uni"))),
                   br(),
                   img(src = "SOUTHERN.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://mktg41.wixsite.com/seristamfordcollege/school-of", h4(strong("Stamford College"))),
                   br(),
                   img(src = "Stamford.jpg",width="150px",height="150px") 
                 )
          ),         
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.kuipsas.edu.my/program-tawaran-kuipsas/", h4(strong("KUIPSAS"))),
                   br(),
                   img(src = "KUIPSAS.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://college.sunway.edu.my/programmes/diploma", h4(strong("Sunway College"))),
                   br(),
                   img(src = "Sunway.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://sunway.edu.my/ipoh/Programmes", h4(strong("Sunway Ipoh"))),
                   br(),
                   img(src = "Sun_ipoh.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.sunway.edu.my/jb/programmes/diploma", h4(strong("Sunway College JB"))),
                   br(),
                   img(src = "SUNWAY_JB.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://swck.edu.my/courses/#", h4(strong("Sunway Kuching"))),
                   br(),
                   img(src = "Sunway-Kuch.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.cordonbleu.edu/malaysia/programmes/en", h4(strong("Le Cordon Bleu"))),
                   br(),
                   img(src = "SunwayLeCordon.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://university.sunway.edu.my/programmes", h4(strong("Sunway University"))),
                   br(),
                   img(src = "SUNWAY_UNI.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://college.taylors.edu.my/en/admissions/malaysian-students.html#entry-requirements", h4(strong("Taylor's College"))),
                   br(),
                   img(src = "Taylor_s.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://studymalaysia.com/where/profile.php?code=terra", h4(strong("Terra College"))),
                   br(),
                   img(src = "TERRA.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.theoneacademy.edu.my/programmes/diploma/index.php", h4(strong("The One Academy"))),
                   br(),
                   img(src = "TOA.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.tmc.edu.my/course-programmes/", h4(strong("TMC College"))),
                   br(),
                   img(src = "TMC.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://courses.toc.edu.my/?gclid=EAIaIQobChMI2qj_opDR6QIVBzdgCh0AsQYYEAAYASAAEgLwgfD_BwE", h4(strong("TOC"))),
                   br(),
                   img(src = "TOC.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.tarc.edu.my/admissions/programmes/programme-offered-a-z/undergraduate-programme/", h4(strong("TAR UC"))),
                   br(),
                   img(src = "TARC.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.ucsicollege.edu.my/diploma-programmes", h4(strong("UCSI College"))),
                   br(),
                   img(src = "UCSI.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.unikl.edu.my/programs/by-levels-of-study/", h4(strong("UniKL"))),
                   br(),
                   img(src = "UniKL.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.uniten.edu.my/programmes/diploma-programmes/", h4(strong("UNITEN"))),
                   br(),
                   img(src = "UNITEN.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.unirazak.edu.my/?page_id=1148", h4(strong("UNIRAZAK"))),
                   br(),
                   img(src = "UNIRAZAK.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.unisel.edu.my/admission/programmes-offered/#diploma", h4(strong("UNISEL"))),
                   br(),
                   img(src = "UNISEL.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.uowmkdu.edu.my/programmes/find-your-program/", h4(strong("UOWM KDU University"))),
                   br(),
                   img(src = "UOW.png",width="150px",height="150px") 
                 )
                 
                 
          )
        )
      )
    ##Others
    else if (schoolType=="Diploma" && sectorType=="Others")
      
      
      body <-
      
      helpText(h5(strong(
        "Sorry, no available result. Please chooce another option", style="color:red",align="center")))
    
    else if (schoolType=="Foundation" && sectorType=="Others")
      
      
      body <-
      
      helpText(h5(strong(
        "Sorry, no available result. Please chooce another option", style="color:red",align="center")))
    
    else if (schoolType=="Others" && sectorType=="IPTA")
      
      
      body <-
      
      helpText(h5(strong(
        "Sorry, no available result. Please chooce another option", style="color:red",align="center")))
   
    else if (schoolType=="Others" && sectorType=="IPTS")
      
      
      body <-
      
      helpText(h5(strong(
        "Sorry, no available result. Please chooce another option", style="color:red",align="center")))
    
    else
      body <- dashboardBody(
        fluidRow(
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.malaysia.gov.my/portal/subcategory/135", h4(strong("Matriculation"))),
                   br(),
                   img(src = "Matrik.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://portal.mpm.edu.my/kalendar-peperiksaan#stpm20", h4(strong("STPM"))),
                   br(),
                   img(src = "STPM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="https://www.moe.gov.my/pendidikan/lepas-menengah/tingkatan-6", h4(strong("STAM"))),
                   br(),
                   img(src = "STAM.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://ambilan.mypolycc.edu.my/portalbpp2/index.asp", h4(strong("POLITEKNIK"))),
                   br(),
                   img(src = "POLITEKNIK.jpg",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://www.jtm.gov.my/2015v3/index.php/ms/kursus-ditawarkan", h4(strong("ILJTM"))),
                   br(),
                   img(src = "ILJTM.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://kemahiran.kbs.gov.my/ms/", h4(strong("ILKBS"))),
                   br(),
                   img(src = "ILKBS.png",width="150px",height="150px") 
                 )
          ),
          
          column(width = 2,
                 box(
                   status = "warning", width = NULL,
                   tags$a(href="http://ipgm.moe.edu.my/", h4(strong("IPGM"))),
                   br(),
                   img(src = "IPGM.jpg",width="150px",height="150px") 
                 )
                 
          )
        )
      )
    #	
  })
    
    
    
    
    # Filter table data 
    output$table <- DT::renderDataTable(DT::datatable({
        
        data <- rank_uni
        
        if(input$Institution != "All"){
            data <- data[data$Institution == input$Institution,]
        }
        if (input$Country != "All") {
            data <- data[data$Country == input$Country,]
        }
        if (input$Year != "All") {
            data <- data[data$Year == input$Year,]
        }
        data
    }))
   

    # Flow Chart tab
    output$dg <- renderGrViz({
      grViz("
      
digraph a_nice_graph {
# node definitions with substituted label text
      node [fontname = Helvetica, shape = hexagon,color = darkorange,fontsize=20]      
      tab1 [label = '@@1']
      node [fontname = Helvetica, shape = oval,color = olivedrab]
      tab2 [label = '@@2']
      node [fontname = Helvetica, shape = rectangle,color = orchid]
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      node [fontname = Helvetica, shape = hexagon,color = turquoise] 
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      node [fontname = Helvetica, shape = oval,color = darkmagenta]
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      node [fontname = Helvetica, shape = rectangle,color = mediumspringgreen]
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11']
      tab12 [label = '@@12']
      node [fontname = Helvetica, shape = rectangle,color = gold]
      tab13 [label = '@@13']
      tab14 [label = '@@14']
      tab15 [label = '@@15']
      tab16 [label = '@@16']
      tab15 [label = '@@15']
      tab17 [label = '@@17']
      tab18 [label = '@@18']
      tab19 [label = '@@19']
      tab20 [label = '@@20']
      tab21 [label = '@@21']
      tab22 [label = '@@22']
      tab23 [label = '@@23']
      tab24 [label = '@@24']
      tab25 [label = '@@25']

      
      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3 -> tab6 -> tab7 -> tab9
      tab7 -> tab10
      tab10 -> tab14[arrowhead = box]
      tab2 -> tab4 -> tab5 -> tab8-> tab11
      tab11 -> tab15[arrowhead = box]
      tab8 -> tab12
      tab12 -> tab16[arrowhead = box]
      tab9 -> tab13[arrowhead = box]
      tab13 ->tab17[arrowhead = box]
      tab17 ->tab18[arrowhead = box]
      tab18 ->tab19[arrowhead = box]
      tab19 ->tab20[arrowhead = box]
      tab20 ->tab21[arrowhead = box]
      tab14 -> tab22[arrowhead = box]
      tab22 -> tab23[arrowhead = box]
      tab23 -> tab24[arrowhead = box]
      tab24 -> tab25[arrowhead = box]
      }

      [1]: 'Where to study?'
      [2]: 'Costwise'
      [3]: 'Affordable'
      [4]: 'Expensive'
      [5]: 'IPTS'
      [6]: 'IPTA'
      [7]: 'Year(s) Spend'
      [8]: 'Year(s) Spend'
      [9]: '2 Years and less'
      [10]: 'More than 2 Years'
      [11]: '2 Years and less'
      [12]: 'More than 2 Years'
      [13]: 'Foundation'
      [14]: 'Diploma'
      [15]: 'Foundation'
      [16]: 'Diploma'
      [17]: 'Matriculation'
      [18]: 'STPM'
      [19]: 'STAM'
      [20]: 'ILKBS'
      [21]: 'ILJTM'
      [22]: 'Polytechnic'
      [23]: 'IPGM'
      [24]: 'ILKBS'
      [25]: 'ILJTM'

")
    })
    txt <- reactive({
      req(input$dg_click)
      nodeval <- input$dg_click$nodeValues[[1]]
      return(paste(nodeval, " is clicked"))
    })
    output$print <- renderPrint({
      txt()
    })

    
 
} # end bracket for server

shinyApp(ui, server)
