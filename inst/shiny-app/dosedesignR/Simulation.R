performSimulation <- function(input, tab, optValue, UserValue){
  
  sim <- input[[paste("sim", tab, sep="")]]
  model <- input[[paste("model", tab, sep="")]]
  
  if (sim == "Optimal 1") {
    simdesign <- data.frame(doses=optValue$doses1, pats=optValue$pats1)
  } else if (sim == "Optimal 2") {
    simdesign <- data.frame(doses=optValue$doses2, pats=optValue$pats2)
  } else if (sim == "Optimal 3") {
    simdesign <- data.frame(doses=optValue$doses3, pats=optValue$pats3)
  } else if (sim == "User Defined 1") {
    if(length(UserValue$doses1) != length(UserValue$pats1)){ stop("Please define equal number of doses and patients.")}
    simdesign <- data.frame(doses=UserValue$doses1, pats=UserValue$pats1)
  } else if (sim == "User Defined 2") {
    if(length(UserValue$doses2) != length(UserValue$pats2)){ stop("Please define equal number of doses and patients.")}
    
    simdesign <- data.frame(doses=UserValue$doses2, pats=UserValue$pats2)
  } else if (sim == "User Defined 3") {
    if(length(UserValue$doses3) != length(UserValue$pats3)){ stop("Please define equal number of doses and patients.")}
    
    simdesign <- data.frame(doses=UserValue$doses3, pats=UserValue$pats3)
  }
if(length(simdesign$doses)== 0){ stop("No user defined model found.")}
  #if(length(simdesign$doses) != length(simdesign$pats)){ stop("Please define equal number of doses and patients.")}
  if (model == "Linear") {
    Int <- input[[paste("linInt", tab, sep="")]]
    Slp <- input[[paste("linSlp", tab, sep="")]]
    doses <- simdesign$doses
    pats <- floor(simdesign$pats)
    x <- rep(simdesign$doses, pats)
    y <- linear(x, Int, Slp) + rnorm(sum(pats), 0, input$sdsim)
    datasim <- data.frame(x,y)
    model <- fitMod(x,y, model="linear")
    level <- input$clevel/100
    doseSeq <- seq(0, input$dmax, length = 201)
    truem <- linear(doseSeq, Int, Slp)
    
    pred <- predict(model,predType = "ls-means",doseSeq = doseSeq, se.fit=TRUE)
    quant <- qt(1 - (1 - level)/2, df = model$df)
    lbnd <- pred$fit - quant * pred$se.fit
    ubnd <- pred$fit + quant * pred$se.fit
    predval <- pred$fit
    plotdata <- data.frame(doseSeq,predval,lbnd,ubnd,truem)
    ylimlow <- min(c(truem[length(truem)]-3*input$sdsim,truem[1]-3*input$sdsim))   
    ylimhigh <- max(c(truem[1]+3*input$sdsim,truem[length(truem)]+3*input$sdsim))
    
    p <- ggplot(plotdata, aes(x=doseSeq)) +  geom_ribbon(aes(ymin = lbnd, ymax = ubnd), fill = "grey70") +
      geom_line(aes(y = predval),colour="#FF3162")+geom_line(aes(x=doseSeq, y=truem))+xlim(input$dmin,input$dmax)+
      ylim(ylimlow,ylimhigh)+
      theme(axis.text = element_text(size=rel(1.1)),axis.title = element_text(size=rel(1.1)))
    
    t<- c("true curve", "estimated curve")
    datleg <-  data.frame(t, test=1:2, y=1:2)
    
   pmodel  <- p + geom_point(data=datasim, aes(x,y))  +labs(x="Dose",y="Response")+
     geom_line(data = datleg, aes(colour = t, x = test, y = y,group=1), alpha = 0) + 
     scale_colour_manual(values = c("black","#FF3162"),labels=c("true curve", "estimated curve"))+    
     theme(legend.text=element_text(size=rel(1.1)), legend.key =  element_rect(fill = "white"),
           legend.margin=margin(l=-10),legend.position = "bottom",legend.title =element_blank())    +   
     guides(colour=guide_legend(override.aes=list(size=1.05,alpha=1)))
  } 
  else if (model == "Emax") {
    E0 <- input[[paste("emxE0", tab, sep="")]]
    ED50 <- input[[paste("emxED50", tab, sep="")]]
    Emax <- input[[paste("emxEmax", tab, sep="")]]
    doses <- simdesign$doses
    pats <- floor(simdesign$pats)
    x <- rep(simdesign$doses, pats)
    y <- emax(x, E0, Emax, ED50) + rnorm(sum(pats), 0, input$sdsim)
    datasim <- data.frame(x,y)
    model <- fitMod(x,y, model="emax", bnds=defBnds(max(doses))$emax)
    level <- input$clevel/100
    model <- fitMod(x,y, model="emax", bnds=defBnds(max(simdesign$doses))$emax)
        doseSeq <- seq(0,  input$dmax, length = 201)
    truem <- emax(doseSeq, E0, Emax, ED50) 
    pred <- predict(model,predType = "ls-means",doseSeq = doseSeq, se.fit=TRUE)
    quant <- qt(1 - (1 - level)/2, df = model$df)
    lbnd <- pred$fit - quant * pred$se.fit
    ubnd <- pred$fit + quant * pred$se.fit
    predval <- pred$fit
    plotdata <- data.frame(doseSeq,predval,lbnd,ubnd,truem)
  
    ylimlow <- min(c(truem[length(truem)]-3*input$sdsim,truem[1]-3*input$sdsim))   
    ylimhigh <- max(c(truem[1]+3*input$sdsim,truem[length(truem)]+3*input$sdsim))
    
    p <- ggplot(plotdata, aes(x=doseSeq)) +  geom_ribbon(aes(ymin = lbnd, ymax = ubnd), fill = "grey70") +
      geom_line(aes(y = predval), colour = "#FF3162")+geom_line(aes(x=doseSeq, y=truem))+xlim(input$dmin,input$dmax)+
      ylim(ylimlow,ylimhigh)+         
      theme(axis.text = element_text(size=rel(1.1)),axis.title = element_text(size=rel(1.1)))
       t<- c("true curve", "estimated curve")
    datleg <-  data.frame(t, test=1:2, y=1:2)
    
    pmodel  <- p + geom_point(data=datasim, aes(x,y))  +labs(x="Dose",y="Response")+
      geom_line(data = datleg, aes(colour = t, x = test, y = y,group=1), alpha = 0) + 
      scale_colour_manual(values = c("black","#FF3162"),labels=c("true curve", "estimated curve"))+    
      theme(legend.text=element_text(size=rel(1.1)), legend.key =  element_rect(fill = "white"),
            legend.margin=margin(l=-10),legend.position = "bottom",legend.title =element_blank())    +   
      guides(colour=guide_legend(override.aes=list(size=1.05, alpha=1)))
    
    
  }
  else if (model == "Sigmoidal Emax") {
    E0 <- input[[paste("semxE0", tab, sep="")]]
    ED50 <- input[[paste("semxED50", tab, sep="")]]
    Emax <- input[[paste("semxEmax", tab, sep="")]]
    Hill <- input[[paste("semxHill", tab, sep="")]]
    doses <- simdesign$doses
    pats <- floor(simdesign$pats)
    x <- rep(simdesign$doses, pats)
    y <- sigEmax(x, E0, Emax, ED50, Hill)  + rnorm(sum(pats), 0, input$sdsim)
    datasim <- data.frame(x,y)
    model <- fitMod(x,y, model="sigEmax", bnds=defBnds(max(simdesign$doses))$sigEmax)
    level <- input$clevel/100
    doseSeq <- seq(0,  input$dmax, length = 201)
    truem <- sigEmax(doseSeq, E0, Emax, ED50,Hill) 
    pred <- predict(model,predType = "ls-means",doseSeq = doseSeq, se.fit=TRUE)
    quant <- qt(1 - (1 - level)/2, df = model$df)
    lbnd <- pred$fit - quant * pred$se.fit
    ubnd <- pred$fit + quant * pred$se.fit
    predval <- pred$fit
    plotdata <- data.frame(doseSeq,predval,lbnd,ubnd,truem)
    ylimlow <- min(c(truem[length(truem)]-3*input$sdsim,truem[1]-3*input$sdsim))   
    ylimhigh <- max(c(truem[1]+3*input$sdsim,truem[length(truem)]+3*input$sdsim))
    p <- ggplot(plotdata, aes(x=doseSeq)) +  geom_ribbon(aes(ymin = lbnd, ymax = ubnd), fill = "grey70") +
    geom_line(aes(y = predval), colour="#FF3162")+geom_line(aes(x=doseSeq, y=truem))+xlim(input$dmin,input$dmax)+
      ylim(ylimlow,ylimhigh)+
      theme(axis.text = element_text(size=rel(1.1)),axis.title = element_text(size=rel(1.1)))
    
    t<- c("true curve", "estimated curve")
    datleg <-  data.frame(t, test=1:2, y=1:2)
    
    pmodel  <- p + geom_point(data=datasim, aes(x,y))  +labs(x="Dose",y="Response")+
      geom_line(data = datleg, aes(colour = t, x = test, y = y,group=1), alpha = 0) + 
      scale_colour_manual(values = c("black","#FF3162"),labels=c("true curve", "estimated curve"))+    
      theme(legend.text=element_text(size=rel(1.1)), 
            legend.margin=margin(l=-10),legend.position = "bottom",legend.key =  element_rect(fill = "white"),legend.title =element_blank())    +   
      guides(colour=guide_legend(override.aes=list(size=1.05,alpha=1)))
   
  }
  
  return(pmodel)
}