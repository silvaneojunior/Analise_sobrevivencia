library(shiny)
library(ggplot2)
library(tidyr)
library(shinyjs)

inter=2.8997
# No, Yes
Partner=c(0,0.5533)
# Automatic, Electronic check, Mailed check 
PayMet=c(0,-0.6240,-0.6774)
#M2M,1Y,2Y
Contr=c(0,1.6420,2.4778)
#No phone service, No, Yes
MulLine=c(0.1132,0,0.5237)
#No, DSL, Fiber optic
Net=c(1.0145,0,-0.3458)
#No, Yes
OnSec=c(0,0.6471)
#No, Yes
OnBac=c(0,0.6173)
#No, Yes
TecSup=c(0,0.4125)
#No, Yes
StrTV=c(0,0.1207)
#No, Yes
StrMov=c(0,0.1381)

scale=1.41 

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
                          choiceNames = c('Automatic','Electronic check','Mailed check'),
                          choiceValues = c(1:3)
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
                          choiceValues = c(1:3)
                      ),
                      radioButtons(
                          inputId='Net',
                          label='Net',
                          choiceNames = c('No','DSL','Fiber optic'),
                          choiceValues = c(1:3)
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
                          inputId='StrTV',
                          label='StrTV',
                          choiceNames = c('No','Yes'),
                          choiceValues = c(1:2)
                      ),
                      radioButtons(
                          inputId='StrMov',
                          label='StrMov',
                          choiceNames = c('No','Yes'),
                          choiceValues = c(1:2)
                      ))),
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
                inputId = "StrTV",
                selected = 1
            )
            
            disable(id="StrTV")
            
            updateRadioButtons(
                inputId = "StrMov",
                selected = 1
            )
            
            disable(id="StrMov")
        }else{
            enable(id="OnSec")
            enable(id="OnBac")
            enable(id="TecSup")
            enable(id="StrTV")
            enable(id="StrMov")
            }
        })

    output$distPlot <- renderPlot({
        
        lin_disc=inter+
                Partner[input$Partner %>% as.numeric]+
                PayMet[input$PayMet %>% as.numeric]+
                Contr[input$Contr %>% as.numeric]+
                MulLine[input$MulLine %>% as.numeric]+
                Net[input$Net %>% as.numeric]+
                OnSec[input$OnSec %>% as.numeric]+
                OnBac[input$OnBac %>% as.numeric]+
                TecSup[input$TecSup %>% as.numeric]+
                StrTV[input$StrTV %>% as.numeric]+
                StrMov[input$StrMov %>% as.numeric]
        x <- c(1:500)
        y <- 1-plnorm(x,lin_disc,scale)
        
        plot=ggplot()+
            geom_line(aes(x=x,y=y))+
            scale_y_continuous('Probabilidade de sobrevivência',limits=c(0,1))+
            scale_x_continuous('Tempo')+
            theme_bw()
        plot
        
    })
}
shinyApp(ui = ui, server = server)
