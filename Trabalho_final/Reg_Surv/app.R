library(shiny)
library(ggplot2)
library(tidyr)
library(shinyjs)
library(survival)

inter=2.8429
# No, Yes
Partner_lab=c('No','Yes')
Partner=c(0,0.5432)
# Bank transfer, Credit card, Electronic check, Mailed check 
PayMet_lab=c('Bank transfer (automatic)','Credit card (automatic)','Electronic check','Mailed check')
PayMet=c(0,0.0522,-0.5729,-0.6517)
#M2M,1Y,2Y
Contr_lab=c('Month-to-month','One year','Two year')
Contr=c(0,1.6203,2.4372)
#No phone service, No, Yes
MulLine_lab=c('No phone service','No','Yes')
MulLine=c(0.1283,0,0.5308)
#No, DSL, Fiber optic
Net_lab=c('No','DSL','Fiber optic')
Net=c(1.0513,0,-0.3135)
#No, Yes
OnSec_lab=c('No','Yes')
OnSec=c(0,0.6395)
#No, Yes
OnBac_lab=c('No','Yes')
OnBac=c(0,0.6038)
#No, Yes
DevProt_lab=c('No','Yes')
DevProt=c(0,0.3578)
#No, Yes
TecSup_lab=c('No','Yes')
TecSup=c(0,0.4001)

scale=1.4

dados=read.csv('Telco-Customer-Churn.csv',stringsAsFactors = T)
dados=dados[order(dados$gender,
                  dados$SeniorCitizen,
                  dados$Partner,
                  dados$Dependents,
                  dados$PhoneService,
                  dados$MultipleLines,
                  dados$InternetService,
                  dados$OnlineSecurity,
                  dados$OnlineBackup,
                  dados$DeviceProtection,
                  dados$TechSupport,
                  dados$StreamingTV,
                  dados$StreamingMovies,
                  dados$Contract,
                  dados$PaperlessBilling,
                  dados$PaymentMethod),]

dados$tenure=dados$tenure+0.5
dados[dados=='No internet service']='No'

ui <- fluidPage(
    titlePanel("Ajuste do modelo paramétrico Log-Normal"),
    useShinyjs(),
    fluidRow(
        column(3,
               column(6,
                      radioButtons(
                          inputId='Partner',
                          label='Partner',
                          choiceNames = c('No','Yes'),
                          choiceValues = c(1:2)
                      ),
                      radioButtons(
                          inputId='PayMet',
                          label='PayMet',
                          choiceNames = c('Bank transfer','Credit card','Electronic check','Mailed check'),
                          choiceValues = c(1:4),
                          selected=3
                      ),
                      radioButtons(
                          inputId='Contr',
                          label='Contr',
                          choiceNames = c('Month-to-month','One year','Two year'),
                          choiceValues = c(1:3)
                      ),
                      radioButtons(
                          inputId='MulLine',
                          label='MulLine',
                          choiceNames = c('No phone service','No','Yes'),
                          choiceValues = c(1:3),
                          selected = 3
                      ),
                      radioButtons(
                          inputId='Net',
                          label='Net',
                          choiceNames = c('No','DSL','Fiber optic'),
                          choiceValues = c(1:3),
                          selected= 3
                      )),
               column(6,
                      radioButtons(
                          inputId='OnSec',
                          label='OnSec',
                          choiceNames = c('No','Yes'),
                          choiceValues = c(1:2)
                      ),
                      radioButtons(
                          inputId='OnBac',
                          label='OnBac',
                          choiceNames = c('No','Yes'),
                          choiceValues = c(1:2)
                      ),
                      radioButtons(
                          inputId='TecSup',
                          label='TecSup',
                          choiceNames = c('No','Yes'),
                          choiceValues = c(1:2)
                      ),
                      radioButtons(
                          inputId='DevProt',
                          label='DevProt',
                          choiceNames = c('No','Yes'),
                          choiceValues = c(1:2)
                      )
                      )),
               column(6,
                      mainPanel(
                          plotOutput("distPlot",width='800px',height='600px')
                      ))
    )
)
server <- function(input, output) {
    
    observeEvent(input$Net,{
        if(input$Net==1){
            updateRadioButtons(
                inputId = "OnSec",
                selected = 1
            )
            
            disable(id="OnSec")
            
            updateRadioButtons(
                inputId = "OnBac",
                selected = 1
            )
            
            disable(id="OnBac")
            
            updateRadioButtons(
                inputId = "TecSup",
                selected = 1
            )
            
            disable(id="TecSup")
            
            updateRadioButtons(
                inputId = "DevProt",
                selected = 1
            )
            
            disable(id="DevProt")
        }else{
            enable(id="OnSec")
            enable(id="OnBac")
            enable(id="TecSup")
            enable(id="DevProt")
            }
        })

    output$distPlot <- renderPlot({
        
        ref_data=dados[dados$Partner==Partner_lab[input$Partner %>% as.numeric] &
                       dados$PaymentMethod== PayMet_lab[input$PayMet %>% as.numeric] &
                       dados$Contract== Contr_lab[input$Contr %>% as.numeric] &
                       dados$MultipleLines== MulLine_lab[input$MulLine %>% as.numeric] &
                       dados$InternetService== Net_lab[input$Net %>% as.numeric] &
                       dados$OnlineSecurity== OnSec_lab[input$OnSec %>% as.numeric] &
                       dados$OnlineBackup== OnBac_lab[input$OnBac %>% as.numeric] &
                       dados$TechSupport== TecSup_lab[input$TecSup %>% as.numeric] &
                       dados$DeviceProtection== DevProt_lab[input$DevProt %>% as.numeric],]

        lin_disc=inter+
                Partner[input$Partner %>% as.numeric]+
                PayMet[input$PayMet %>% as.numeric]+
                Contr[input$Contr %>% as.numeric]+
                MulLine[input$MulLine %>% as.numeric]+
                Net[input$Net %>% as.numeric]+
                OnSec[input$OnSec %>% as.numeric]+
                OnBac[input$OnBac %>% as.numeric]+
                TecSup[input$TecSup %>% as.numeric]+
                DevProt[input$DevProt %>% as.numeric]
        x <- c(1:80)
        y <- 1-plnorm(x,lin_disc,scale)
        
        
        plot=ggplot()+
            geom_line(aes(x=x,y=y))+
            scale_y_continuous('Probabilidade de sobrevivência',limits=c(0,1))+
            scale_x_continuous('Tempo')+
            theme_bw()
        
        print(length(ref_data$tenure))
        if(length(ref_data$tenure)>10){
            kp_est=survfit(Surv(ref_data$tenure,ifelse(ref_data$Churn=='Yes',1,0))~1)
            
            time=kp_est$time
            upper=kp_est$upper
            lower=kp_est$lower
            surv=kp_est$surv
            
            ribbon_time=sort(c(time-0.001,time+0.001),decreasing=F)[-c(1)]
            ribbon_lower=sort(c(lower,lower),decreasing=T,na.last=T)[-c(length(time)*2)]
            ribbon_upper=sort(c(upper,upper),decreasing=T,na.last=T)[-c(length(time)*2)]
            
            plot=plot+
                geom_step(aes(x=time,y=surv,color='Kaplan-Meier'))+
                geom_ribbon(aes(x=ribbon_time,
                                ymin=ribbon_lower,
                                ymax=ribbon_upper,
                                fill='I.C. Kaplan-Meier'),
                            alpha=0.25)
        }
    })
        
}
shinyApp(ui = ui, server = server)
