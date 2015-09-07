# server.R

shinyServer(
  function(input, output,session) {
    
    isFileLoaded<-reactive({
      if (is.null(input$datafile))
        return (FALSE)
    })
    
    isFileRDAfunc <- reactive({
        tempValues$nameAsString<- basename(input$datafile$name)
        tempValues$isRDA <-grepl(".rda",tempValues$nameAsString)
      if (tempValues$isRDA==TRUE){
        paste("Uploaded file: ",tempValues$nameAsString," with ",dim(lodadedData())[1]," entries")
        }
      # for future implementations use else if to customize other file extentions  messages
      else paste("Uploaded file: ",tempValues$nameAsString," with ",dim(lodadedData())[1]," entries")
    })

    
    output$fileStatusInfo <- renderText({
      if (is.null(input$datafile)==FALSE) filetypeVal<-isFileRDAfunc()
      else filetypeVal="FILE is not YET LOADED"
      filetypeVal})


    updateDisplayedArguments<-reactive({
      if (!tempValues$nameAsString==''){
        tempValues$columnNames<-names(lodadedData())
        argChoices <- unlist(strsplit(tempValues$columnNames, split=","))
        updateSelectizeInput(session, 'survArguments', choices = argChoices,server = TRUE)
        updateSelectizeInput(session, 'usedArguments', choices = argChoices,server = TRUE)
        updateSelectizeInput(session, 'terminalValue', choices = argChoices,server = TRUE)
        updateSelectizeInput(session, 'clusterColumn', choices = argChoices,server = TRUE)
        updateSelectizeInput(session,   'slopeValue' , choices = argChoices,server = TRUE)
        updateSelectizeInput(session, 'termEvArguments', choices = argChoices,server = TRUE)
        updateSelectInput(session,  'clusteredData')
        updateNumericInput(session,'clustersNb')
        updateSelectInput(session,'alphaValue')
        updateSelectInput(session,'recurrentAGValue')
        updateTextInput(session, 'kappaValues')
        updateSelectInput(session, 'crossValidation')
        updateSelectInput(session,'correlation')
      }
      else argChoices <- 'empty_field'
    })
    
    lodadedData<-reactive({
      inFile <- input$datafile    
      if (is.null(inFile)){
        return(NULL)}
    
      frailtyData <- do.call(rbind, lapply(inFile$datapath, function(M){
          if (tempValues$isRDA==TRUE)  ws<-get(load(M))
          else ws <-read.csv(M, header=TRUE)
        }))
    })
    
    output$contents <- renderDataTable({
      updateDisplayedArguments() # the order of called functions is VERY important for display output !
      lodadedData()
      #head(frailtyData, n = input$obs)
    }, options = list(lengthMenu = c(5,10,50,200,500,1000), pageLength = 5)
    )
  
    resultsBlock<-reactive({
      switchCurrentState()
      if(is.null(tempValues$transformedData)==FALSE) readmissionTransformed <-tempValues$transformedData
      customFunct <-  eval(parse(text=toString(tempValues$totalStringToPrint)))
      })
    
    switchCurrentState<-reactive({
      if (tempValues$currentState==TRUE) tempValues$currentState=FALSE
      else tempValues$currentState==TRUE
    })
    
    
    output$functionAsText <- renderText({
      tempValues$dataFileName=unlist(strsplit(tempValues$nameAsString,".", fixed=TRUE))
      eval(parse(text = c("data(",tempValues$dataFileName[1],")")))
      if ((is.null(lodadedData())==FALSE) & input$JointGselectedformula =='Joint General default options' & input$modelType =='Joint General' ) {
        tempValues$resultsFileName=input$JointGselectedformula
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(time,event) ~ cluster(id) + dukes + charlson + sex  + chemo + terminal(death),
                                                      formula.terminalEvent = ~ dukes + charlson + sex + chemo, data =",tempValues$dataFileName[1],", jointGeneral = TRUE,
                                                      n.knots = 8, kappa = c(2.11e+08, 9.53e+11))")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Jointselectedformula =='JOINT frailty model with gap times' & input$modelType =='Joint' ) {
        tempValues$resultsFileName=input$Jointselectedformula
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(time, event) ~ cluster(id) + dukes + charlson + sex + chemo + terminal(death),
                                                      formula.terminalEvent = ~ dukes + charlson + sex + chemo, data =",tempValues$dataFileName[1],",
                                                      n.knots = 8 , kappa = c(2.11e+08,9.53e+11))")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Jointselectedformula =='Stratified JOINT frailty model with gap times' & input$modelType =='Joint' ) {
        tempValues$resultsFileName=input$Jointselectedformula
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(time, event) ~ cluster(id) + dukes + charlson + strata(sex) + chemo + terminal(death),
                                                      formula.terminalEvent = ~ dukes + charlson + sex + chemo, data =",tempValues$dataFileName[1],",
                                                      n.knots = 8, kappa = c(2.11e+08,2.11e+08,9.53e+11))")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Jointselectedformula =='JOINT frailty model without alpha parameter' & input$modelType =='Joint' ) {
        tempValues$resultsFileName=input$Jointselectedformula
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(time, event) ~ cluster(id) + dukes + charlson + sex + chemo + terminal(death),
                                                      formula.terminalEvent = ~ dukes + charlson + sex + chemo, data =",tempValues$dataFileName[1],",
                                                      n.knots = 8, kappa = c(2.11e+08,9.53e+11), Alpha = 'none')")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Jointselectedformula =='JOINT frailty model for clustered data' & input$modelType =='Joint' ) {
        tempValues$resultsFileName=input$Jointselectedformula
        tempValues$transformedData <- transform(readmission,group=id%%31+1)
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(t.start, t.stop, event) ~ cluster(group) + num.id(id) + dukes + charlson + sex + chemo + terminal(death),
                                                      formula.terminalEvent = ~ dukes + charlson + sex + chemo, data = readmissionTransformed,
                                                      recurrentAG = TRUE,  n.knots = 10, kappa = c(2.11e+08,9.53e+11))")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Multivariateselectedformula =='MULTIVARIATE frailty model with gap times' & input$modelType =='Multivariate' ) {
        tempValues$totalStringToPrint <- paste("multivPenal(Surv(TIMEGAP,INDICREC)~ cluster(PATIENT) + v1 + v2 + event2(INDICMETA) + terminal(INDICDEATH),
                                                      formula.Event2 =~ v1 + v2 + v3, formula.terminalEvent =~ v1, data = dataMultiv,
                                                      hazard = 'Weibull')")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Coxselectedformula=='COX proportionnal hazard model with gap times' & input$modelType =='Cox') {
        tempValues$resultsFileName=input$Jointselectedformula
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(time, event) ~ dukes + charlson + sex + chemo, n.knots = 10, kappa = 1, data =",tempValues$dataFileName[1],",
                                                      cross.validation = TRUE)")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Additiveselectedformula =='ADDITIVE frailty model with no correlation between random effects' & input$modelType =='Additive'){
        tempValues$resultsFileName=input$Additiveselectedformula
        tempValues$totalStringToPrint <- paste("additivePenal(Surv(t1,t2,event) ~ cluster(group) + var1 + var2 + slope(var1), cross.validation = TRUE, data =",tempValues$dataFileName[1],",
                                                      correlation = FALSE, n.knots = 10, kappa = 1)")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Additiveselectedformula =='ADDITIVE frailty model with a correlation between random effects' & input$modelType =='Additive'){
        tempValues$resultsFileName=input$Additiveselectedformula
        tempValues$totalStringToPrint <- paste("additivePenal(Surv(t1,t2,event) ~ cluster(group) + var1 + var2 + slope(var1), cross.validation = TRUE, data =",tempValues$dataFileName[1],",
                                                      correlation = TRUE, n.knots = 10, kappa = 1)")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Nestedselectedformula =='NESTED frailty model' & input$modelType =='Nested'){
        tempValues$resultsFileName=input$input$Nestedselectedformula
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(t1, t2, event) ~ cluster(group) + subcluster(subgroup) + cov1 + cov2, data =",tempValues$dataFileName[1],",
                                                      n.knots = 8, kappa = 50000, cross.validation = TRUE)")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Nestedselectedformula =='Stratified NESTED frailty model' & input$modelType =='Nested'){
        tempValues$resultsFileName=input$Nestedselectedformula
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(t1, t2, event) ~ cluster(group) + subcluster(subgroup) + cov1 + strata(cov2), data =",tempValues$dataFileName[1],",
                                                      n.knots = 8, kappa = c(50000,50000))")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Sharedselectedformula =='Shared frailty model with gap times' & input$modelType =='Shared'){
        tempValues$resultsFileName=input$Sharedselectedformula
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(time, event) ~ cluster(id) + dukes + charlson + sex + chemo, n.knots = 10, data =",tempValues$dataFileName[1],",
                                                      kappa = 1, cross.validation = TRUE)")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Sharedselectedformula =='Stratified shared frailty model with gap times' & input$modelType =='Shared'){
        tempValues$resultsFileName=input$Sharedselectedformula
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(time, event) ~ cluster(id) + charlson + dukes + chemo + strata(sex), n.knots = 10, data =",tempValues$dataFileName[1],",
                                                      kappa = c(2.11e+08,2.11e+08))")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Sharedselectedformula =='Shared frailty model with time-varying effect of covariates' & input$modelType =='Shared'){
        tempValues$resultsFileName=input$Sharedselectedformula
        tempValues$totalStringToPrint <- paste("frailtyPenal(Surv(time, event) ~ cluster(id) + dukes + charlson + timedep(sex) + chemo, n.knots = 8, data =",tempValues$dataFileName[1],",
                                                      kappa = 1, betaknots = 3, betaorder = 1)")}
      
      else if ((is.null(lodadedData())==FALSE) & input$Sharedselectedformula =='Shared frailty model with interval-censored data' & input$modelType =='Shared'){
        tempValues$resultsFileName=input$Sharedselectedformula
        bcos$event <- ifelse(bcos$left!=bcos$right,1,0)
        bcos$group <- c(rep(1:20,4),1:14)
        
        tempValues$totalStringToPrint <- paste("frailtyPenal(SurvIC(left, right, event) ~ cluster(group) + treatment, n.knots = 8, data =",tempValues$dataFileName[1],", kappa = 10000)")}
      
      else if ((is.null(lodadedData())==FALSE) & ((input$JointGselectedformula== 'custom options' & input$modelType =='Joint General') |
                                                  (input$Sharedselectedformula == 'custom options' & input$modelType =='Shared') |
                                                  (input$Additiveselectedformula== 'custom options' & input$modelType =='Additive') |
                                                  (input$Coxselectedformula== 'custom options' & input$modelType =='Cox') |
                                                  (input$Jointselectedformula== 'custom options' & input$modelType =='Joint') |
                                                  (input$Multivariateselectedformula== 'custom options' & input$modelType =='Multivariate') |
                                                  (input$Nestedselectedformula== 'custom options' & input$modelType =='Nested'))){
        tempValues$resultsFileName=paste(input$modelType," custom Results")
        tempValues$totalStringToPrint <-customFunctionBuilder()
      }
      else return (NULL)
      })
    
    
    customFunctionBuilder<-reactive({
      if (!tempValues$nameAsString==''){
        funcToCall=''
        cluster=c("cluster(",input$clusterColumn,") + ")
        num.id= ''
        formula.terminalEvent =''
        terminal=''
        slope=''
        recurrentAGString=''
        correlation=''
        n.knotsString =input$knotsNb
        stringToPrint=''
        terminalEventString=''
        fileNameAsString = tempValues$dataFileName[1]
        alphaString =''
        jointGeneralValue =''
        kappaString=c(", kappa = c(",input$kappaValues,")")
        cross.validation= "FALSE"
        
        if(input$modelType=='Joint' | input$modelType=='Joint General'){
          funcToCall <-'frailtyPenal'
          formula.terminalEvent =',formula.terminalEvent = ~'
          selectedTerEvSize<-length(input$termEvArguments)
          if (is.null(input$numID)==FALSE) {
            num.id = paste("+ num.id(",input$numID,") + ")
            tempValues$transformedData <- transform(readmission,group=id%%31+1)
          }
          if (selectedTerEvSize>=1){
            for (p in 1:selectedTerEvSize) {
              if (p==selectedTerEvSize) terminalEventString<-c(terminalEventString,input$termEvArguments[p])
              else terminalEventString<-c(terminalEventString,input$termEvArguments[p],' + ')
            }
          }
          alphaString = c(", Alpha = '",input$alphaValue,"'")
          terminal =c(" + terminal(",input$terminalValue,")")
          recurrentAGString=c(", recurrentAG = ",input$recurrentAGValue)
          if (input$modelType=='Joint General') jointGeneralValue = ", jointGeneral = TRUE"
        }
        
        else if (input$modelType =='Cox'){
          funcToCall <-'frailtyPenal'
          cluster=''
          cross.validation=input$crossValidation
        }
        else if (input$modelType =='Shared' | input$modelType =='Nested')                             funcToCall <-'frailtyPenal'
        
        else if (input$modelType=='Additive'){
          funcToCall <-'additivePenal'
          slope =c(" + slope(",input$slopeValue,")")
          cross.validation=input$crossValidation
          correlation=paste(", correlation = ",input$correlation)
        }
        
        else if (input$modelType=='Multivariate')                                                     funcToCall <-'multivPenal'
        else funcToCall <- ''
        
        survString=input$survArguments[1]
        tempValues$columnNames<-names(lodadedData())
        selectedSurvSize<-length(input$survArguments)
        selectedArgSize<-length(input$usedArguments)
        
        #Part of string common for all the models
        if (selectedSurvSize>=2){
          for (j in 2:selectedSurvSize) {
            survString<-c(survString,',',input$survArguments[j])
          }
        } 
        if (selectedArgSize>=1){
          for (i in 1:selectedArgSize) {
            if (i==selectedArgSize) stringToPrint<-c(stringToPrint,input$usedArguments[i])
            else stringToPrint<-c(stringToPrint,input$usedArguments[i],' + ')
          }
        }
        tempValues$customKnotsNb<- input$knotsNb[1]
        tempStringVect <-c(funcToCall, "(Surv(", survString, ") ~",cluster, stringToPrint, terminal, slope, formula.terminalEvent, terminalEventString,
                          correlation, recurrentAGString," , n.knots = ",n.knotsString, kappaString, " , data = ", fileNameAsString , alphaString,
                          jointGeneralValue, " , cross.validation =", cross.validation ,")")
        tempValues$totalStringToPrint<- paste(tempStringVect,sep='', collapse = '')
      }
    })
    
    #output for program status object
    output$out2 <- renderPrint({
      input$initiateFit   # detects when "EXECUTE" button is pressed
      isolate({           # prevents code re-execution, unless "EXECUTE" button is pressed
        if (is.null(resultsBlock())==TRUE) {
          cat(sprintf("Press EXECUTE to procces function"))
        }
        else cat(sprintf("Process finished. Please check results tab"))
      })
    })
    
    # output RESULTS string object
    output$fitResults <- renderPrint({
      input$initiateFit   # detects when "EXECUTE" button is pressed
      isolate({           # prevents code re-execution, unless "EXECUTE" button is pressed
        if (is.null(resultsBlock())==FALSE) {
          print(resultsBlock(), digits = 4)
        }
        else cat(sprintf("NO data file selected yet"))
      })
    })
    
    # save as file output
    output$downloadResultsData <- downloadHandler(
      filename = function() { paste(tempValues$resultsFileName, '.txt', sep='') },
      content = function(filename) {
        sink(filename)
        print(resultsBlock(), digits = 4)
        # Stop writing to the file
        sink()
      }
    )
    
    # outplut Plot with display parameters
    output$plot2display <- renderPlot({
      if (is.null(resultsBlock())==TRUE){
        paste ("No plot can be desplayed at this moment")}
      else plotInput()},
      width = "auto", height = "auto", res = 72, quoted = FALSE
    )
    
    # download version for outplut Plot
    output$downloadPlot <- downloadHandler(
      filename = "Shinyplot.png",
      content = function(file) {
        png(file)
        plotInput()
        dev.off()
      })  
    
    # function that draws the Plot
    plotInput <- function(){
      p <-plot(resultsBlock(),type.plot="haz",event="recurrent",conf.bands=TRUE)}
    
    # current chosen model message
    output$text1 <- renderText({ 
      cat(sprintf("You have selected", input$modelType, "model"))
    })
#######################################################################################################################
#######################################################################################################################
  }
)
