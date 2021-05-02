## function ##
library(shiny)
library(shinydashboard)
library(formattable)
library(rsconnect)
library(MASS)
library(lubridate)
library(ggplot2)
library(gridExtra)

# data
exit<-read.csv('exit.csv', sep=",", header=F, skip=9, stringsAsFactor = FALSE)
aladdin<-read.csv('aladdin.csv', sep=",", header=F, skip=8, stringsAsFactor = FALSE)


# data preprocessing
prepro<-function(data,weekend){
  raw <- data
  raw2<-raw[,c(1,11,14)]                    # variable selection
  colnames(raw2) <- c('date','n','cum_n')   # variable name
  raw2$t<-seq(1,nrow(raw2),1)               # date from opening date
  raw2$predict<-'0'                         # train/ predict
  raw2$n <- as.numeric(gsub(",","",raw2$n)) # remove comma
  raw2$cum_n <- as.numeric(gsub(",","",raw2$cum_n))   # remove comma
  raw2$day <- wday(as.Date(raw2$date), label = TRUE, locale = "English")
  
  t <- 1:(nrow(raw2))  
  date <- raw2$date  # date
  day <- raw2$day    # day
  st <- raw2$n       # number or audience
  data_f <- data.frame(date,day,t,st)
  
  # weekend effect(ad_st)
  for (i in 1:nrow(data_f)){
    if (data_f$day[i]=='Sat'){
      data_f$ad_st[i] <- data_f$st[i]/weekend
    }else if (data_f$day[i]=='Sun'){
      data_f$ad_st[i] <- data_f$st[i]/weekend
    }else if (data_f$date[i] == '2019-08-15'){
      data_f$ad_st[i] <- data_f$st[i]/weekend
    }else if (data_f$date[i] == '2019-06-06'){
      data_f$ad_st[i] <- data_f$st[i]/weekend
    }else if (data_f$date[i] == '2019-05-06'){
      data_f$ad_st[i] <- data_f$st[i]/weekend
    }else if (data_f$date[i] == '2019-09-12'){
      data_f$ad_st[i] <- data_f$st[i]/weekend
    }else if (data_f$date[i] == '2019-09-13'){
      data_f$ad_st[i] <- data_f$st[i]/weekend
    }else{
      data_f$ad_st[i] <- data_f$st[i]
    }
  }
  
  data_f$yt<-cumsum(data_f$ad_st[1:nrow(data_f)]) # cummulative audience
  data_f$yt_1<-c(1,data_f$yt[-nrow(data_f)])      
  data_f$lnyt_1<-log(data_f$yt_1)   # log
  data_f$predict <- '0'
  
  return(data_f)
}

# real m
error<-function(data){
  raw<-data
  raw2<-raw[,c(1,11,14)]                   # variable selection
  colnames(raw2) <- c('date','n','cum_n')  # variable name
  raw2$cum_n <- as.numeric(gsub(",","",raw2$cum_n))  # remove comma
  m_real<-raw2$cum_n[nrow(raw2)]
  return(m_real)
}


# bass model
bass_lm<-function(lm_data,train){
  lm<-lm(ad_st~yt_1+I(yt_1^2),data=lm_data[1:train,])
  coef<-summary(lm)$coef[,1]
  
  math<-function(a,b,c){
    m1=(-b+sqrt(b^2-4*a*c))/(2*c)
    m2=(-b-sqrt(b^2-4*a*c))/(2*c)
    max(m1,m2)
  }
  m<- round(as.numeric(math(coef[1],coef[2],coef[3])),0)
  q<- round(as.numeric((-1)*coef[3]*m),3)
  p<- round(as.numeric(coef[1]/m),3)
  a<- p*m
  b<- q-p
  c<- -q/m
  return(c(m,p,q,a,b,c))
}

# logis model
logis_lm<-function(lm_data,train){
  lm<-lm(ad_st~yt_1+I(yt_1^2)-1,data=lm_data[1:train,])
  coef<-summary(lm)$coef[,1]
  
  q<- round(as.numeric(coef[1]),3)
  m<- round(as.numeric((-1)*coef[1]/coef[2]),0)
  a<- q
  b<- -q/m
  return(c(m,q,a,b))
}
# gumbel model
gumbel_lm<-function(lm_data,train){
  lm<-lm(ad_st~yt_1+I(yt_1*log(yt_1))-1,data=lm_data[1:train,])
  coef<-summary(lm)$coef[,1]
  
  q<- round(as.numeric(-coef[2]),3)
  m<- round(as.numeric(exp(coef[1]/q)),0)
  a<- q*log(m)
  b<- -q
  return(c(m,q,a,b))
}
# exponential model
exp_lm<-function(lm_data,train){
  lm<-lm(ad_st~yt_1,data=lm_data[1:train,])
  coef<-summary(lm)$coef[,1]
  
  p<- round(as.numeric(-coef[2]),3)
  m<- round(as.numeric(coef[1]/p),0)
  a<- p*m
  b<- -p
  return(c(m,p,a,b))
}

# graph
time<-function(data,train){
  ad_st<-data[train+1:length(data)]
  t<- (train+1:length(ad_st))
  p<-data.frame(t,ad_st)
  #p$yt<-cumsum(p$st[1:nrow(p)])
  predict<-'1'
  pp<-cbind(p,predict)
  ppp<-pp[,c('t','ad_st','predict')]
  return(pp)
}

# Q-Q plot 
qq_bass<-function(lm_data,train,c){
  opt1<-function(par){
    m<-par[1]
    summary(lm(t~log(1+c*(yt/(m+1))/(1-(yt/(m+1))))-1,data=lm_data[1:train,]))$r.squared
  }
  m_init<-lm_data$yt[nrow(lm_data)]
  optim_r<-optim(par=c(m_init), opt1, control=list(fnscale=-1))
  m<-optim_r$par
  summary<-summary(lm(t~log(1+c*(yt/(m+1))/(1-(yt/(m+1))))-1,data=lm_data[1:train,]))
  k<- 1/(summary$coef[1])
  p<- k/(1+c)
  q<- c*k/(1+c)
  return(c(m,p,q,k))
}

qq_logis<-function(lm_data,train){
  opt1<-function(par){
    m<-par[1]
    summary(lm(t~log(1+c*(yt/(m+1))/(1-(yt/(m+1)))),data=lm_data[1:train,]))$r.squared
  }
  m_init<-lm_data$yt[nrow(lm_data)]
  optim_r<-optim(par=c(m_init), opt1, control=list(fnscale=-1))
  m<-optim_r$par
  q<- 1/(summary(lm(t~log(1+c*(yt/(m+1))/(1-(yt/(m+1)))),data=lm_data[1:train,]))$coef[2])
  p<-0
  mu<-summary(lm(t~log(1+c*(yt/(m+1))/(1-(yt/(m+1)))),data=lm_data[1:train,]))$coef[1]
  sigma<-summary(lm(t~log(1+c*(yt/(m+1))/(1-(yt/(m+1)))),data=lm_data[1:train,]))$coef[2]
  return(c(m,p,q,mu,sigma))
}
qq_gumbel<-function(lm_data,train){
  opt1<-function(par){
    m<-par[1]
    summary(lm(t~(-log(-log(yt/(m+1)))),data=lm_data[1:train,]))$r.squared
  }
  m_init<-lm_data$yt[nrow(lm_data)]
  optim_r<-optim(par=c(m_init), opt1, control=list(fnscale=-1))
  m<-optim_r$par
  summary<-summary(lm(t~(-log(-log(yt/(m+1)))),data=lm_data[1:train,]))
  q<- 1/summary$coef[2]
  p<-0
  mu<-summary$coef[1]
  sigma<-summary$coef[2]
  return(c(m,p,q,mu,sigma))
}
qq_exp<-function(lm_data,train){
  opt1<-function(par){
    m<-par[1]
    summary(lm(t~(-log(1-(yt/(m+1)))),data=lm_data[1:train,]))$r.squared
  }
  m_init<-lm_data$yt[nrow(lm_data)]
  optim_r<-optim(par=c(m_init), opt1, control=list(fnscale=-1))
  m<-optim_r$par
  summary<-summary(lm(t~(-log(1-(yt/(m+1)))),data=lm_data[1:train,]))
  p<- 1/summary$coef[2]
  q<-0
  mu<-summary$coef[1]
  sigma<-summary$coef[2]
  return(c(m,p,q,mu,sigma))
}

## ui.R ##

header <- dashboardHeader(title = "Movie")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data",
             fileInput('file1', label=h6('Choose CSV File'),
                       accept=c('text/csv', 
                                'text/comma-separated-values,text/plain', 
                                '.csv')),
             #tags$hr(),
             # movie
             selectInput("Data", label = h6("Movie"),
                         choices = list("EXIT" = 1, "Aladdin" = 2), selected = 1)),
    menuItem("Forecast",
             # Diffusion model
             radioButtons("Diffusionmodel", label = h6("Diffusion model"),
                          choices = list("Bass" = 'bass', "Gumbel" = 'gumbel',
                                         "Logistic" = 'logis', "Exponential" = 'exp'),selected = 'bass'),
             # Estimation method
             radioButtons("Estimationmethod", label = h6("Estimation model"),
                          choices = list("OLS" = 'ols', "Q-Q plot" = 'qqplot',
                                         "MLE" = 'mle'), selected = 'ols'),
             # Prediction date
             sliderInput("bins",
                         label = h6("Prediction horizon:"),
                         min = 1,
                         max = 30,
                         value = 5),
             # train date
             numericInput("train",
                          label = h6("Train data"),
                          value = 28),
             # weekend effect
             numericInput("weekend",
                          label = h6("Weekend adjustment"),
                          value = 2)
    )))

body <- dashboardBody(
  fluidRow(
    tabBox(width = 12,
           tabPanel(title="Data", 
                    fluidRow(
                      box(title = 'Poster', status = "primary", imageOutput('poster')),
                      box(title = 'Preview', status = "primary", uiOutput("video")))),
           tabPanel("Predict",
                    valueBoxOutput("m"),
                    valueBoxOutput("p"),
                    valueBoxOutput("q"),
                    fluidRow(
                      box(title = "Data table", status = "primary", tableOutput('head')),
                      box(title = "Predict values", status = "primary", tableOutput('predict'))
                    ),
                    fluidRow(
                      box(title = 'Time-Series plot', status = 'primary',width = 12, plotOutput("Plot1"))
                    ))
    )))


ui <- dashboardPage(header, sidebar, body)

## server.R ##
server <- function(input, output, session) {
  
  
  
  # data selection
  Moviedata <- reactive({
    if(input$Data==1){
      exit
    }else if(input$Data==2){
      aladdin
    }
  })    
  
  # market potential
  modeling_m<-reactive({
    # estimation method 1
    if (input$Estimationmethod == 'ols'){  
      if (input$Diffusionmodel == 'bass'){ 
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        m<-comma(round(bass_lm(lm_data,train_num)[1],0), format='d')
        
      }else if (input$Diffusionmodel == 'logis'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        m<-comma(round(logis_lm(lm_data,train_num)[1],0), format='d')
      }else if (input$Diffusionmodel == 'gumbel'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        m<-comma(round(gumbel_lm(lm_data,train_num)[1],0), format='d')
      }else if (input$Diffusionmodel == 'exp'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        m<-comma(round(exp_lm(lm_data,train_num)[1],0), format='d')
      }
      # estimation method 2
    }else if (input$Estimationmethod == 'qqplot'){
      if (input$Diffusionmodel == 'bass'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        p1<-bass_lm(lm_data,train_num)[2]
        q1<-bass_lm(lm_data,train_num)[3]
        c<-q1/p1
        m<-comma(round(qq_bass(lm_data,train_num,c)[1],0), format='d')
      }else if (input$Diffusionmodel == 'logis'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        m<-comma(round(qq_logis(lm_data,train_num)[1],0), format='d')
      }else if (input$Diffusionmodel == 'gumbel'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        m<-comma(round(qq_gumbel(lm_data,train_num)[1],0), format='d')
      }else if (input$Diffusionmodel == 'exp'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        m<-comma(round(qq_exp(lm_data,train_num)[1],0), format='d')
      }
    }
  })
  
  modeling_p<-reactive({
    if (input$Estimationmethod == 'ols'){
      if (input$Diffusionmodel == 'bass'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        p<-round(bass_lm(lm_data,train_num)[2],3)
        
      }else if (input$Diffusionmodel == 'logis'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        p<-''
      }else if (input$Diffusionmodel == 'gumbel'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        p<-''           
      }else if (input$Diffusionmodel == 'exp'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        p<-round(exp_lm(lm_data,train_num)[2],3)
      }
      
    }else if (input$Estimationmethod == 'qqplot'){
      if (input$Diffusionmodel == 'bass'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        p1<-bass_lm(lm_data,train_num)[2]
        q1<-bass_lm(lm_data,train_num)[3]
        c<-q1/p1
        p<-round(qq_bass(lm_data,train_num,c)[2],3)
      }else if (input$Diffusionmodel == 'logis'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        p<-''
      }else if (input$Diffusionmodel == 'gumbel'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        p<-''           
      }else if (input$Diffusionmodel == 'exp'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        p<-round(qq_exp(lm_data,train_num)[2],3)           
      } 
    }
  })  
  
  modeling_q<-reactive({
    if (input$Estimationmethod == 'ols'){
      if (input$Diffusionmodel == 'bass'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        q<-round(bass_lm(lm_data,train_num)[3],3)
        
      }else if (input$Diffusionmodel == 'logis'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        q<-round(logis_lm(lm_data,train_num)[2],3)
      }else if (input$Diffusionmodel == 'gumbel'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        q<-round(gumbel_lm(lm_data,train_num)[2],3)
      }else if (input$Diffusionmodel == 'exp'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        q<-''
      }
      
    }else if (input$Estimationmethod == 'qqplot'){
      if (input$Diffusionmodel == 'bass'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        p1<-bass_lm(lm_data,train_num)[2]
        q1<-bass_lm(lm_data,train_num)[3]
        c<-q1/p1
        q<-round(qq_bass(lm_data,train_num,c)[3],3)
      }else if (input$Diffusionmodel == 'logis'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        q<-round(qq_logis(lm_data,train_num)[3],3)
      }else if (input$Diffusionmodel == 'gumbel'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        q<-round(qq_gumbel(lm_data,train_num)[3],3)
      }else if (input$Diffusionmodel == 'exp'){
        lm_data<-prepro(Moviedata(),input$weekend)
        train_num<-input$train
        q<-''
      }
    }
  }) 
  
  # parameter m
  output$m <- renderValueBox({
    valueBox(
      modeling_m(), "Market potential", icon = icon("user-friends"),
      color = "blue"
    )})
  # parameter p
  output$p <- renderValueBox({
    valueBox(
      modeling_p(), "Coefficient of Innovation", icon = icon("signal"),
      color = "purple"
    )})
  # parameter q
  output$q <- renderValueBox({
    valueBox(
      modeling_q(), "Coefficient of Immitation", icon = icon("signal"),
      color = "purple"
    )})
  # train head
  output$head <- renderTable({
    raw<-Moviedata()
    raw1<-raw[,c(1,11,14)]
    colnames(raw1) <- c('Date','Audience','Cummulative Audience')
    return(head(raw1)) 
  })
  
  # Predective data head
  predict_y <- reactive({
    if (input$Estimationmethod == 'ols'){
      if (input$Diffusionmodel == 'bass'){
        lm_data <- prepro(Moviedata(),input$weekend)
        train_num <- input$train
        a <- bass_lm(lm_data,train_num)[4]
        b <- bass_lm(lm_data,train_num)[5]
        c <- bass_lm(lm_data,train_num)[6]
        pred <- a + b*lm_data$yt_1 + c*lm_data$yt_1^2
      }else if (input$Diffusionmodel == 'logis'){
        lm_data <- prepro(Moviedata(),input$weekend)
        train_num <- input$train
        a <- logis_lm(lm_data,train_num)[3]
        b <- logis_lm(lm_data,train_num)[4]
        pred <- a*lm_data$yt_1 + b*lm_data$yt_1^2  
      }else if (input$Diffusionmodel == 'gumbel'){
        lm_data <- prepro(Moviedata(),input$weekend)
        train_num <- input$train
        a <- gumbel_lm(lm_data,train_num)[3]
        b <- gumbel_lm(lm_data,train_num)[4]
        pred <- a*lm_data$yt_1 + b*lm_data$yt_1*lm_data$lnyt_1            
      }else if (input$Diffusionmodel == 'exp'){
        lm_data <- prepro(Moviedata(),input$weekend)
        train_num <- input$train
        a <- exp_lm(lm_data,train_num)[3]
        b <- exp_lm(lm_data,train_num)[4]
        pred <- a + b*lm_data$yt_1
      }
      
    }else if (input$Estimationmethod == 'qqplot'){
      if (input$Diffusionmodel == 'bass'){
        lm_data <- prepro(Moviedata(),input$weekend)
        train_num <- input$train
        p1<-bass_lm(lm_data,train_num)[2]
        q1<-bass_lm(lm_data,train_num)[3]
        c<-q1/p1  
        m <- qq_bass(lm_data,train_num,c)[1]
        p <- qq_bass(lm_data,train_num,c)[2]
        q <- qq_bass(lm_data,train_num,c)[3]
        k <- qq_bass(lm_data,train_num,c)[4]
        Yt <- m*(1-exp(-k*lm_data$t))/(1+(q/p)*exp(-k*lm_data$t))
        Yt_1 <- m*(1-exp(-k*(lm_data$t-1)))/(1+(q/p)*exp(-k*(lm_data$t-1)))
        pred <- Yt - Yt_1
      }else if (input$Diffusionmodel == 'logis'){
        lm_data <- prepro(Moviedata(),input$weekend)
        train_num <- input$train
        m<- qq_logis(lm_data,train_num)[1]
        p<- qq_logis(lm_data,train_num)[2]
        q<- qq_logis(lm_data,train_num)[3]
        mu <- qq_logis(lm_data,train_num)[4]
        sigma <- qq_logis(lm_data,train_num)[5]
        z<-(lm_data$t-mu)/sigma
        pred <- m/sigma*exp(-z)/(1+exp(-z))^2
      }else if (input$Diffusionmodel == 'gumbel'){
        lm_data <- prepro(Moviedata(),input$weekend)
        train_num <- input$train
        m<- qq_gumbel(lm_data,train_num)[1]
        p<- qq_gumbel(lm_data,train_num)[2]
        q<- qq_gumbel(lm_data,train_num)[3]
        mu <- qq_gumbel(lm_data,train_num)[4]
        sigma <- qq_gumbel(lm_data,train_num)[5]
        z<-(lm_data$t-mu)/sigma
        pred <- m*exp(-z)*exp(-exp(-z))/sigma
      }else if (input$Diffusionmodel == 'exp'){
        lm_data <- prepro(Moviedata(),input$weekend)
        train_num <- input$train
        m<- qq_exp(lm_data,train_num)[1]
        p<- qq_exp(lm_data,train_num)[2]
        q<- qq_exp(lm_data,train_num)[3]
        mu <- qq_exp(lm_data,train_num)[4]
        sigma <- qq_exp(lm_data,train_num)[5]
        pred <- m*p*exp(-p*(lm_data$t-mu))
      }
    }
    return(pred)
  })
  
  output$predict <- renderTable({
    raw<-Moviedata()
    raw2<-raw[,c(1,11)]
    p<-round(as.numeric(predict_y()),0)
    raw3<-cbind(raw2,p)
    colnames(raw3) <- c('Date','Audience','Predictive audience')
    raw4<-raw3[input$train-1:input$train+4,]
    return(head(raw4))
  })
  # Time series
  output$Plot1 <-renderPlot({
    # label 
    label_ko_num = function(num) {
      ko_num = function(x) {
        new_num = x %/% 1000
        return(paste(new_num, 'K', sep = ''))
      }
      return(sapply(num, ko_num))
    }
    raw1<-prepro(Moviedata(),input$weekend)
    raw2<-raw1[1:(input$train),][,c('t','ad_st','predict')]
    p<-time(predict_y(),input$train)[1:input$bins,]
    raw3<-rbind(raw2,p)
    raw3$yt<-cumsum(raw3$ad_st[1:nrow(raw3)])
    
    g1<-ggplot(raw3,aes(x=t,y=ad_st,fill=predict))+geom_bar(stat='identity')+xlab('t')+ylab('Audience')+scale_y_continuous(labels = label_ko_num)+scale_fill_manual(values=c("#00798c","darkred"))
    g2<-ggplot(raw3,aes(x=t,y=yt,fill=predict))+geom_bar(stat='identity')+xlab('t')+ylab('Cummulative Audience')+scale_y_continuous(labels = label_ko_num)+scale_fill_manual(values=c("#00798c","darkred"))
    
    
    grid.arrange(g1,g2, nrow=1, ncol=2)
  })
  
  output$poster<-renderImage({
    if(input$Data==1){
      list(src = "exit.jpeg", height="100%")
    }else if(input$Data==2){
      list(src = "aladdin.jpg", height="100%")}}, deleteFile = FALSE)
  
  output$video <- renderUI({
    tags <- c("li4jOV5j7SI", "KrM3vS5sy2w")
    
    if(input$Data==1){
      tag <- tags[1]
    }else if(input$Data==2){
      tag <- tags[2]}
    
    HTML(paste0(
      '<html>
        <body>
          <iframe id="existing-iframe"
              width="100%" height="100%"
              src="https://www.youtube.com/embed/',
      tag,
      '?enablejsapi=1"
              frameborder="0"
          ></iframe>

          <script type="text/javascript">
            var tag = document.createElement(\'script\');
            tag.src = \'https://www.youtube.com/iframe_api\';
            var firstScriptTag = document.getElementsByTagName(\'script\')[0];
            firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);

            var player;
            function onYouTubeIframeAPIReady() {
              player = new YT.Player(\'existing-iframe\');
            }
          </script>
        </body>
      </html>' ))
    
    
  })
}

## app.R ##
shinyApp(ui, server)
#rsconnect::deployApp()
