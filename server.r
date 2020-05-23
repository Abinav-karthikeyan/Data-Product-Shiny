#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

        library(leaflet)
        library(geosphere)
        library(class)
        setwd("C:\\Users\\ADMIN\\Desktop\\acads")
        t=read.csv("trav.csv")
        cf=read.csv("co.csv")
        head(t)
        
        t$GENDER=sample (c('M','F'), size=101, replace=T, prob=c(.65,.35))
        
        t$GENDER=as.factor(t$GENDER)
       
        gw=c()
        for (row in 1:nrow(t)) {
            l <- t[row,4]
            d <- t[row,5]
            gw=c(gw,norm(l,d))
            
        }
        gw
        t$coord=gw
       
        
        
        
        mum=c(19.076,72.877)
        kol=c(22.5726,88.3639)
        delh=c(28.7041,77.1025)
        che=c(13.0827,80.2702)
        pun=c(18.5204,73.8567)
        ahm=c(23.0225,72.571)
        ind=c(22.7196,75.8577)
        
       
        
        norm=function(u,v)
        {
            x=c(u,v)
            a=distHaversine(x,mum)
            b=distHaversine(x,kol)
            c=distHaversine(x,che)
            d=distHaversine(x,delh)
            e=distHaversine(x,ind)
            f=distHaversine(x,ahm)
            
            return(min(a,b,c,d,e,f))
            
            
            }
       
        #1-herbal
        #2-beverage
        #3-iced
        
        drin=c(1,2,2,2,3,2,2,1,2,3,3,1,1,2,3,3,2,2,1,2,2,1,3,1,3,2,2,1,1,2,1,3,1,2,1
               , 2,2,1,1,2,1,3,2,1,2,3,3,3,2,2,1,2,2,1,1,1,1,2,1,3,2,3,2,3,3,2,2,2,2,1,1,2,2,1,1,1,1,1,2,3,3,1,2,1,2,3,2,1,1,2
                  ,1,1,2,2,1,3,3,2,2,1,2)
        t$drink=drin
        
        
       
        ttrain=t[1:60,]
        
        o=ttrain[-c(1,4:5)]
        
        o=o[-c(2)]
        
        
        
        o=ttrain[-1]
        o=ttrain[-2]
        o=data.frame(AGE=ttrain$AGE,BODY.TEMPERATURE=ttrain$BODY.TEMPERATURE,
                     coord=ttrain$coord)
        
        normalize <- function(x) {
            return ((x - mean(x)) /sd(x) )}
        ttrain_n=as.data.frame(lapply(o[1:3],normalize))
        
        
        head(ttrain_n)
        
        predlab=ttrain$drink
       
        length(predlab)
        
        
        
        ttest=t[61:101,]
        
        head(ttest)
        ttest_n=ttest[-c(1,4:5)]
        ttest_n=ttest_n[-c(2,5)]
        actlab=ttest$drink
        predlabn=as.data.frame(predlab)
        
        s = reactive(
            {
                
            
            e=input$s
            
            
            prediction <- knn(train=ttrain_n,test=ttest_n,predlab,k=e,prob = TRUE)
            xtab=table(prediction,actlab)
            xtab
            
            
                
                
                accuracy = sum(prediction == actlab) / length(actlab)
                
                accuracy
        
    
            }
    )
        
        
        y=reactive(
        if(input$map)
        {
            
            mp=leaflet()%>% addTiles()%>% addMarkers(lat=cf$latitude,lng=cf$longitude,
                                                 popup="passengers",clusterOptions = TRUE) %>% addCircleMarkers(lat=c(mum[1],kol[1],che[1],delh[1],ahm[1],pun[1],ind[1]),
                                                                                                                lng=c(mum[2],kol[2],che[2],delh[2],ahm[2],pun[2],ind[2]) 
                                                                                                                ,radius = 50,color = "red",popup = "COVID CLUSTERS")
        
        }
          
        )
        
        
        
        
        
        
        
        
        
        
        
        
        output$kn=renderText(
            {
            s()
            }
            
            
        )
        output$p=renderLeaflet(
            
            y()
        )
        
       
      
        
        
    
         

    

})
