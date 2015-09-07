library(gtools)
library(shiny)
library(knitr)
library(frailtypack)

tempValues <- reactiveValues(dataFileName=list(), isRDA = 0,nameAsString='', columnNames='', currentState=FALSE, totalStringToPrint='', 
                             transformedDat = data.frame(), customKnotsNb=10, resultsFileName='')
isolate(tempValues$isRDA)
isolate(tempValues$nameAsString)