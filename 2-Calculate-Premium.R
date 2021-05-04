## function ##
library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(formattable)
library(ggplot2)
library(ggmap)
library(sp)
library(maptools)
library(viridis)
library(gridExtra)

# Life Insurance
year2010<-read.csv("2010생명표.csv",header=TRUE, skip=1)

premium<-function(data,gender,age,rate,n, m, b){
  
  r<-log(1+rate)
  age<-age+1
  data<-data[1:100,]
  if(gender==0){
    lx<-data[,9]
  }else{
    lx<-data[,10]
  }
  
  lx_plus_n<-lx[(age+1):(age+(n-1))] 
  r_n<-r*seq(1:(n-1))
  ax_n<-0.5+ sum(lx_plus_n*exp(-r_n)/lx[age])+ 0.5*lx[(age+n)]*exp(-r*n)/lx[age]
  
  lx_plus_m<-lx[(age+1):(age+m)]
  r_m<-r*seq(1:m)
  Ax_m<-1-r*(0.5+sum(lx_plus_m*exp(-r_m))/lx[age]+(lx[age+m]/lx[age])*exp(-r*(m+1))/(1-exp(-r)))
  
  px_annual<-b*Ax_m/ax_n
  px<-px_annual/12
  result=list("px"=px_annual/12, "data"=data)
  return(result)
}  


# parameter: 데이터/성별/나이/이자율/납입만기/지급개시시접/월지급연급액
calculate.px <- function(data, g, x, i, n, a, b){ 
  
  # data 
  if (g == 'f'){ # female
    Tx <- as.integer(data[,4][-1]) # start from age 1
    l <- lx <- data[,10][-1]
  } else { # male
    Tx <- as.integer(data[,3][-1])
    l <- lx <- data[,9][-1]
  }
  
  r <- log(1+i)
  b <- b*12  # year
  
  # f(t): trapezoidal rule
  f <- function(t) {
    if (x+t > 100) { # over 100 years old
      return(l[100]/l[x] * exp(-r*t))
    } else {
      return(l[x+t]/l[x] * exp(-r*t))
    }
  }
  
  # sum of f(t)
  s <- function(start, end) {
    s <- 0
    for (t in start:end) {
      s <- s + f(t)
    }
    return(s)
  }
  
  # integration: trapezoidal rule
  integration <- function(start, end) {
    (1/2)*f(start) + s(start, end) + (1/2)*f(end)
  }
  
  # expenditure: px * integration(x, x+n) 
  annuity <- b * integration(a-x, Tx[x]) 
  px <- annuity/integration(0, n)

  return(px/12) # monthly premium
}



# Car Insurance
car<-read.csv('premium.csv', stringsAsFactors = F)


car$Kilometers <- as.factor(car$Kilometers)
car$Zone <- as.factor(car$Zone)
car$Make <- as.factor(car$Make)
car$Bonus<-as.factor(car$Bonus)
car$mu <- car$Payment/car$Claims


carprem<-function(data, deduct, lim, bon, kilo, zz, make){
  
  glm1<-glm(Claims~ offset(log(Insured + 1)) + Kilometers + Zone + Bonus + 
              Make + Kilometers:Zone + Kilometers:Bonus + Kilometers:Make + 
              Zone:Bonus + Zone:Make + Bonus:Make, data=car, family="poisson"(link="log"))
  
  lm <- glm(Payment/Claims~Kilometers+Zone+Make+Bonus+Kilometers:Zone+Kilometers:Make+Kilometers:Bonus+Zone:Make+Zone:Bonus+Make:Bonus,
            data=car, family=Gamma(link='log'), weight=car$Claims)
  
  data$poisson <- predict(glm1, data, type='response')
  data$gamma <- predict(lm, data, type='response')
  
  A<-deduct
  B<-lim
  
  poisson.values <-data[data$Bonus==bon & data$Kilometers==kilo & data$Zone==zz & data$Make==make,]$poisson
  m <- data[data$Bonus==bon & data$Kilometers==kilo & data$Zone==zz & data$Make==make,]$Insured
  lnMu <- data[data$Bonus==bon & data$Kilometers==kilo & data$Zone==zz & data$Make==make,]$gamma
  
  lambda <- poisson.values/(1 + m)
  Mu <- min(max(lnMu-A, 0),B)
  
  prem <- lambda*Mu
  return(as.numeric(prem*127.4))
}

# map
korea_map_shp = rgdal::readOGR("2013_si_do.shp")
korea_map <- fortify(korea_map_shp)
korea_map <- as.data.frame(korea_map)
korea_map2 <- type_convert(korea_map)

#````````````````````````````````````````````````````````````````````````

## ui.R ##
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

header <- dashboardHeader(title = "Insurance")
sidebar <- dashboardSidebar(
  sidebarMenu(
    
    convertMenuItem(menuItem("Life Insurance", tabName = "LifeInsurance", icon = icon("heartbeat"),
             fileInput('file1', label=h6('Choose CSV File'),
                       accept=c('text/csv',  
                                'text/comma-separated-values,text/plain', 
                                '.csv'))), tabName = "LifeInsurance"),
    
    convertMenuItem(menuItem("Pension", tabName = "Pension", icon = icon("money"),
             fileInput('file2', label=h6('Choose CSV File'),
                       accept=c('text/csv',  
                                'text/comma-separated-values,text/plain', 
                                '.csv'))), tabName = "Pension"),
    
    convertMenuItem(menuItem("Car Insurance", tabName = "CarInsurance", icon = icon("car"),
             fileInput('file3', label=h6('Choose CSV File'),
                       accept=c('text/csv',  
                                'text/comma-separated-values,text/plain', 
                                '.csv'))), tabName = "CarInsurance")
    
  )
)


## Body
body <- dashboardBody(
  tabItems(
    tabItem("LifeInsurance",  
            fluidRow(valueBoxOutput("p1",width=12)),
            tabBox(width = 12,
                   tabPanel("Information",
                          fluidRow(
                            box(title="Personal Information", status="primary", width=4,solidHeader=TRUE,
                              radioButtons("Gender1", "Gender" ,choices = list("male" = 0, "female" = 1), selected = 0),
                              sliderInput("Age1","Age", 0, 100, 30)),
                          box(title="Premium Information", status="warning", width=8,solidHeader=TRUE,
                              column(6,
                                numericInput("n1", "Payment term(year)", 20, 0, 100, 1),
                                numericInput("m1", "Maturity term(year)", 20, 0, 100, 1)
                              ),
                              column(6,
                                sliderInput("rate1", "Annual interest rate(%)", 0, 20, 2),
                                numericInput("b1", "Benefit(10,000 won)", 10000, 0, 20000, 1)
                              )
                        ))),
                   tabPanel("Graph",
                            fluidRow(box(title = 'Life Table', status = 'primary',width = 12, plotOutput("Plot2"))))
            )),
                            
    
    
    tabItem("Pension",
            fluidRow(valueBoxOutput("Bp",width=12)),
            tabBox(width = 12,
                   tabPanel("Information",
            fluidRow(
              box(title ='Personal Information',width = 12, height = 150, solidHeader = TRUE, status = "primary",
                  column(3, radioButtons('Bgender',"Gender", choices = list('Female'=0,'Male'=1),selected=0)),
                  column(3, numericInput("Bage","Age",40,0,100,1)),
                  column(3, numericInput("Br","Retirement Age",60,40,100,1)),
                  column(3, numericInput('Bm',"Payment Start Age",60,40,100,1))
                  
                  )),
            fluidRow(
              box(title = 'Premium Information',width = 12, height = 150, solidHeader = TRUE, status = "warning",
                  column(4, sliderInput("Bi","Interest Rate",0,30,2)),
                  column(4, sliderInput('Bn',"Payment Expiration(year)",0,50,30)),
                  column(4, sliderInput('Ba',"Monthly Receipt(10,000 won)",0,500,2))
                  ))))),
    tabItem("CarInsurance",
            fluidRow(valueBoxOutput("p3", width=12)),
            tabBox(width = 12,
              tabPanel("Information",
                fluidRow(
                  box(width=4, height=200, title = 'Bonus', solidHeader=TRUE, status="primary",
                      numericInput('bonus', '', value=2, min=1, max=7, step=1)),
                 
                  box(width=8, height=200, title = 'Limit and Deductible', solidHeader=TRUE, status="warning",
                      checkboxInput('no', 'No LIMIT and No Deductible', value = FALSE ),
                      fluidRow(box(width=6, sliderInput('deductible',label = "Deductible",min = 0, max = 1000,value = 10)),
                               box(width=6, sliderInput('limit', label = "Limit", min = 1000, max = 100000, value = 10))))),
                
                fluidRow(
                  box(title="Kilometers", status="primary", width=4, height=300, solidHeader=TRUE,
                      radioButtons("km", "" ,
                                   choices = list("1: less than 1000" = 1,
                                                  "2: from 1000 to 15000" = 2,
                                                  "3: 15000 to 20000" = 3,
                                                  "4: 20000 to 25000" = 4,
                                                  "5: more than 25000" = 5), selected = 1)),
            
                  box(title="Zone", status="primary", width=4, height=300, solidHeader=TRUE,
                      radioButtons("zone", "" ,
                                    choices = list("Jeju, Jeollabuk-do, Daejeon" = 1, 
                                                   "Gyeonggi-do, Incheon, Seoul" = 2,
                                                   "Ulsan, Busan" = 3,
                                                   "Daegu, Gyeongsangnam-do" = 4,
                                                   "Chungcheongnam-do, Gwangju, Gyeongsangbuk-do" = 5,
                                                   "Chungcheongbuk-do, Sejong" = 6,
                                                   "Gangwon-do, Jeollanam-do" = 7), selected = 1)),
            
                  box(title="Type of Cars", status="primary", width=4, height=300, solidHeader=TRUE,
                      radioButtons("car", "" ,
                                   choices = list("KIA-K7" = 1, 
                                                  "KIA-K9" = 2,
                                                  "KIA-Morning" = 3,
                                                  "Hyundai-Granger" = 4,
                                                  "Hyndai-Genesis" = 5,
                                                  "Hyndai-Sonata" = 6,
                                                  "Hyundai-Santafe" = 7,
                                                  "Chevrolet-Spark" = 8,
                                                  "Benz-Sedan" = 9), selected = 1)))
                ),
              tabPanel("Graph",
                       fluidRow(box(title = 'Premium by Zone', status = 'primary',width = 12, plotOutput("Plot1"))))
              ))
    )
)


ui <- dashboardPage(header, sidebar, body, skin='black')

#````````````````````````````````````````````````````````````````````````

## server.R ##
server <- function(input, output, session) {
  ## Life ##
  fileupload1<- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else {
      otheryear <- read.csv(inFile$datapath, header = TRUE, skip=1, stringsAsFactors=FALSE)
      return(otheryear)
    }
  })
  
  # data selection
  lifedata <- reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      year2010
    }else{
      fileupload1()
      }
  })
  
  
  p1<-reactive({
    
    data<-lifedata()
    
    if(input$Gender1 == 0){ 
      gender<-0
    }else{
      gender<-1
    }
    
    age <- input$Age1
    n <- input$n1  
    m <- input$m1
    rate <- input$rate1 *0.01
    b <- input$b1 * 10000
    
    p1 <- comma(round(premium(data, gender, age, rate, n, m, b)$px,0),format='d')
    return(p1)
  })
  
  output$p1 <- renderValueBox({
    valueBox(
      p1(), "Premium Price", icon = icon("won"),
      color = "aqua", width=12
    )})
  
  output$Plot2 <-renderPlot({
    
    data<-lifedata()
    surv_m<-data[,9]; surv_f<-data[,10]
    d_m<-data[0:100,18]; d_f<-data[0:100,19]
    data1<-data.frame(age=rep(0:100,2), gender=rep(c('male','female'),c(101,101)), survival=c(surv_m, surv_f))
    data2<-data.frame(age=rep(0:99,2), gender=rep(c('male','female'),c(100,100)), death=c(d_m, d_f))
    
    g1<-ggplot(data2, aes(age, death, color=gender))+geom_line()+geom_point()+ggtitle('Death Plot')
    g2<-ggplot(data1, aes(age, survival, color=gender))+geom_line()+geom_point()+ggtitle('Survival Plot')
    grid.arrange(g1,g2, nrow=1, ncol=2)
  })
  
  # data selection
  pensiondata <- reactive({
    inFile <- input$file2
    if(is.null(inFile)){
      year2010
    }else{
      otheryear <- read.csv(inFile$datapath, header = TRUE, skip=1, stringsAsFactors=FALSE)
      return(otheryear)
    }
  })
  
  Bp<-reactive({
    
    data <- pensiondata()
    
    if (input$Bgender == 0){
      Bp <- comma(round(calculate.px(data, 'f',input$Bage, input$Bi/100, input$Bn, input$Bm, input$Ba)*10000,0), format='d')
      
    }else if(input$Bgender == 1){
      Bp <- comma(round(calculate.px(data, 'm',input$Bage, input$Bi/100, input$Bn, input$Bm, input$Ba)*10000,0), format='d')
    }
    return(Bp)
  })
  
  output$Bp <- renderValueBox({
    valueBox(
      Bp(), "Premium Price", icon = icon("won"),
      color = "aqua", width=12
    )})
  
  
  ## Car ##
  fileupload3<- reactive({
    inFile <- input$file3
    if (is.null(inFile)){
      car
    }else {
      new.data <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = F)
      return(new.data)
    }
  })
  
  p3<-reactive({
    
    data <- fileupload3()
    
    data$Insured <- as.numeric(data$Insured)
    data$Claims <- as.numeric(data$Claims)
    data$Payment <- as.numeric(data$Payment)
    
    
    data$Kilometers <- as.factor(as.character(data$Kilometers))
    data$Zone <- as.factor(as.character(data$Zone))
    data$Make <- as.factor(as.character(data$Make))
    data$Bonus<-as.factor(as.character(data$Bonus))
    
    deduct <- as.numeric(input$deductible)
    lim <- as.numeric(input$limit)
    
    bon <- factor(input$bonus, levels=c(1:7))
    kilo <- factor(input$km, levels=c(1:5))
    zz <- factor(input$zone, levels=c(1:7))
    make <- factor(input$car, levels=c(1:9))
    
    if(input$no==FALSE){
      p3<-comma(round(carprem(data, deduct, lim, bon, kilo, zz, make), 0), format='d')
      return(p3)
    }else{
      data1 <- data[data$Bonus==bon & data$Kilometers==kilo & data$Zone==zz & data$Make==make,]
      p4<-comma(round(data1$Payment/data1$Claims,0), format='d')
      return(p4)
    }
  })
  
  output$p3 <- renderValueBox({
    valueBox(
      p3(), "Premium Price", icon = icon("won"),
      color = "aqua", width=12
    )})
  
  # Premium by Zone
  plotdata<-reactive({
    
    data <- fileupload3()
    
    data$Insured <- as.numeric(data$Insured)
    data$Claims <- as.numeric(data$Claims)
    data$Payment <- as.numeric(data$Payment)
    
    
    data$Kilometers <- as.factor(as.character(data$Kilometers))
    data$Zone <- as.factor(as.character(data$Zone))
    data$Make <- as.factor(as.character(data$Make))
    data$Bonus<-as.factor(as.character(data$Bonus))
    
    deduct <- as.numeric(input$deductible)
    lim <- as.numeric(input$limit)
    
    bon <- factor(input$bonus, levels=c(1:7))
    kilo <- factor(input$km, levels=c(1:5))
    make <- factor(input$car, levels=c(1:9))
    
    z1 <- factor(1, levels=c(1:7))
    z2 <- factor(2, levels=c(1:7))
    z3 <- factor(3, levels=c(1:7))
    z4 <- factor(4, levels=c(1:7))
    z5 <- factor(5, levels=c(1:7))
    z6 <- factor(6, levels=c(1:7))
    z7 <- factor(7, levels=c(1:7))
    
    
    p1<-carprem(data, deduct, lim, bon, kilo, z1, make)
    p2<-carprem(data, deduct, lim, bon, kilo, z2, make)
    p3<-carprem(data, deduct, lim, bon, kilo, z3, make)
    p4<-carprem(data, deduct, lim, bon, kilo, z4, make)
    p5<-carprem(data, deduct, lim, bon, kilo, z5, make)
    p6<-carprem(data, deduct, lim, bon, kilo, z6, make)
    p7<-carprem(data, deduct, lim, bon, kilo, z7, make)
    
    data<-data.frame(zone=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
                     premium=c(p2,p3,p4,p2,p5,p1,p3,p6,p2,p7,p6,p5,p1,p7,p5,p4,p1))
    return(data)
  })
  
  output$Plot1 <-renderPlot({
    
    data <- plotdata()
    
    data1<-data%>%group_by(zone)%>%summarize(premium=mean(premium))
    data2<-inner_join(data1, korea_map2, by=c('zone'='id'))
    ggplot(data2, aes(long, lat, group=group))+geom_polygon(aes(fill = premium)) + coord_quickmap()+ scale_fill_viridis(option = "inferno", direction = -1)
    
  })
  
}

## app.R ##
shinyApp(ui, server)

