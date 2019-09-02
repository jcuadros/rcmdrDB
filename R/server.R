options(shiny.maxRequestSize=500*1024^2)
shinyServer(function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  #Data selection
  dfActions <- NULL
  dfMilestonesDef <- NULL 
  dfSessions <- NULL
  
  #Import
  dfActions <- reactive({
    if(is.null(input$logsImport)) return(NULL)
    return(importStudentData(input$logsImport$datapath,input$logsImport$name))
  })
  
  #No Milestones
  dfUsers <- reactive({generatedfUsers(dfActions())})
  dfFiles <- reactive ({generatedfFiles(dfActions())})
  dfStudents <- reactive({if(input$checkbox) dfUsers() else dfFiles()})
  
  #Yes Milestones
  dfMilestonesDef<-reactive({
    inFile <- input$obsMilestonesImport
    if (is.null(inFile)) return(NULL) else
      return(read.table(inFile$datapath, header=TRUE, sep="\t", quote=""))
  })
  
  dfUsersMilestones <- reactive ({generatedfUsersMilestones(dfActions(), dfMilestonesDef())})
  dfFilesMilestones <- reactive ({generatedfFilesMilestones(dfActions(), dfMilestonesDef())})
  dfStudentsMilestones <- reactive({
    inFile <- input$obsMilestonesImport
    if (is.null(inFile)) return(NULL) else
      if(input$checkbox) dfUsersMilestones() else dfFilesMilestones()
  })
  
  dfMilestonesEvDef<-reactive({
    inFile <- input$evMilestonesImport
    if (is.null(inFile)) return(NULL) else
      return(read.table(inFile$datapath, header=TRUE, sep="\t", quote=""))
  })
  dfStudentsMilestonesEv <- reactive({
    generatedStudentsMilestonesEv(dfStudentsMilestones(),dfMilestonesEvDef())
  })
  
  #Data input.
  output$studentData <- renderText({
    if(is.null(dfActions())) {
      "Please select some logs files to start the analysis!"
    } else {
      paste("Loaded log data:",length(unique(dfActions()$filename)),
            "files,",nrow(dfActions()),"actions.")
    }
  })
  output$milestonesData <- renderText({
    if(is.null(dfMilestonesDef())) {
      "Observation milestones definition not yet loaded!"
    } else {
      paste("Read observation milestones: ",nrow(dfMilestonesDef()),
            " milestones (", paste(dfMilestonesDef()[,1],collapse = ", ") ,")", sep="")
    }
  })
  output$evmilestonesData <- renderText({
    if(is.null(dfMilestonesEvDef())) {
      "Evaluation milestones not loaded. Observation milestones will be used for evaluation."
    } else {
      paste("Read evaluation milestones: ",nrow(dfMilestonesEvDef()),
            " milestones (", paste(dfMilestonesEvDef()[,1],collapse = ", ") ,")", sep="")
    }
  })
  
  
  #GRAPHS
  
  #Global Results
  #Time on task distribution
  output$toT_dist <- renderPlot({
    if(is.null(dfStudents())) return(NULL)
    plotDistribution(dfStudents()$time_on_task, xlabel="Time on Task (in minutes)")
  })
  output$toT_text1 <- renderText({
    if(is.null(dfStudents())) return("")
    str1 <- paste("Distribution of the time on task spent by student. 
                  A histogram and an estimate of the density curve are presented.")
    str2 <- paste("The mean of this distribution is", 
                  round(mean(dfStudents()$time_on_task), 2),
                  "minutes.")
    lowbound <- floor(mean(dfStudents()$time_on_task) - sd(dfStudents()$time_on_task))
    upbound <- ceiling(mean(dfStudents()$time_on_task) + sd(dfStudents()$time_on_task))
    prop <- mean(dfStudents()$time_on_task >= lowbound &
                   dfStudents()$time_on_task <= upbound) * 100
    str3 <- paste(round(prop,0), "% of students have worked between", lowbound, 
                  "and", upbound, "minutes.")
    bimod <- diptest::dip.test(dfStudents()$time_on_task)
    str4 <- if(bimod$p.value < 0.05) {
      paste("There is clearly more than one mode for the time on task.")
    } else if(bimod$p.value < 0.10) {
      paste("There is some sign of presence of more than one mode for time on task.")
    } else(paste("There is no sign of more than one mode for time on task."))
    HTML(paste(str1, str2, str3, str4, sep = '<br />'))
  })
  
  #Num of actions distribution
  output$nActions <- renderPlot({
    if(is.null(dfStudents())) return(NULL)
    plotDistribution(dfStudents()$n_actions, xlabel="Number of Actions")
  })
  output$nActions_text1 <- renderText({
    if(is.null(dfStudents())) return("")
    str1 <- paste("Distribution of the number of actions done by student. 
                  A histogram and an estimate of the density curve are presented.")
    str2 <- paste("The mean of this distribution is", 
                  round(mean(dfStudents()$n_actions), 2),
                  "actions.")
    lowbound <- floor(mean(dfStudents()$n_actions) - sd(dfStudents()$n_actions))
    upbound <- ceiling(mean(dfStudents()$n_actions) + sd(dfStudents()$n_actions))
    prop <- mean(dfStudents()$n_actions >= lowbound &
                   dfStudents()$n_actions <= upbound) * 100
    str3 <- paste(round(prop,0), "% of students have done between", lowbound, 
                  "and", upbound, "actions.")
    bimod <- diptest::dip.test(dfStudents()$n_actions)
    str4 <- if(bimod$p.value < 0.05) {
      paste("There is clearly more than one mode for the number of actions.")
    } else if(bimod$p.value < 0.10) {
      paste("There is some sign of presence of more than one mode for the number of actions.")
    } else(paste("There is no sign of more than one mode for the number of actions."))
    HTML(paste(str1, str2, str3, str4, sep = '<br />'))
  })
  
  #Num of actions vs total time
  output$nActionsVStoT <- renderPlot({
    if(is.null(dfStudents())) return(NULL)
    ggplot(dfStudents(), aes(x=time_on_task, y=n_actions)) +  geom_point(size = I(3), alpha = I(0.2)) +
      labs(x = "Time on Task", y = "Number of Actions") + theme_bw()
  })
  
  output$plotuinAct <- renderUI({
    if(is.null(dfStudents())) return(NULL)
    plotOutput("nActionsVStoT", height=300,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
  })

  output$plot_pointsnAct <- renderText({
    if(is.null(dfStudents())) return("")
    
    if(input$checkbox)  {dat <- data.frame(ids=dfStudents()$user)} 
    else {dat <- data.frame(ids=dfStudents()$filename)}

    dat$toT <- dfStudents()$time_on_task
    dat$nAc <- dfStudents()$n_actions
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    
    if(length(response)==0)
      return ("Place your mouse over a data point to identify the student.") 
    else
      return (paste("Student Id:",response[1]))
  })
  
  #Action time by student
  output$tei_graph<- renderPlot({
    if(is.null(dfActions())) return(NULL)
    l <- rcmdrtrTimeEventsPerId(time = dfActions()$time, ids = 
        (if(input$checkbox) dfActions()$user else dfActions()$filename),
        sessions = dfActions()$session)
    
    punts <- l[[1]]
    linea <- l[[2]]
    maxTime <- l[[3]]
    
    drawPlotActionPoints(punts, linea, maxTime)
  })
  
  output$plotui <- renderUI({
    if(is.null(dfActions())) return(NULL)
    plotOutput("tei_graph", height=300,
               hover = hoverOpts(
                 id = "plot_hover",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$plot_points <- renderText({
    if(is.null(dfActions())) return("")
    dat <- rcmdrtrTimeEventsPerId(time = dfActions()$time, ids = 
                                    (if(input$checkbox) dfActions()$user else dfActions()$filename),
                                    sessions = dfActions()$session)
    res <- nearPoints(dat[[1]], input$plot_hover,
                      threshold = 5, maxpoints = 100,
                      addDist = TRUE)
    res$dist_ <- round(res$dist_, 1)
    response<-unique(as.character(res$ids))
    if(length(response)==0)
      return ("Place your mouse over a data point to identify the student.") 
    else
      return (paste("Student Id:",response))
  })
  
  #Wordcloud
  output$wcloud <- renderPlot({
    if(is.null(dfActions())) return(NULL)
    drawCommandsWordCloud(as.character(dfActions()$xml))
  })
  
  
  #Observation Milestones
  #Proportion bar diagram
  output$proportionbars <- renderPlot ({
    if(is.null(dfStudentsMilestones())) return(NULL)
    rcmdrtrMilestonesDifficulty(dfStudentsMilestones())
  })
  
  #Heatmap
  output$heatmap <- renderPlot({
    if(is.null(dfStudentsMilestones())) return(NULL)
    rcmdrtrHeatMapAchievedMilestonePerId(dfStudentsMilestones(),labels=NULL)
  })
  
  
  #Evaluation Milestones
  #Proportion bar diagram
  output$evproportionbars <- renderPlot ({
    if(is.null(dfStudentsMilestonesEv())) return(NULL)
    rcmdrtrMilestonesDifficulty(dfStudentsMilestonesEv())
  })
  
  #Heatmap
  output$evheatmap <- renderPlot({
    if(is.null(dfStudentsMilestonesEv())) return(NULL)
    rcmdrtrHeatMapAchievedMilestonePerId(dfStudentsMilestonesEv()[,1:(ncol(dfStudentsMilestonesEv())-1)],labels=NULL)
  })
  
  #Num times milestone is done
  output$gradedistr <- renderPlot({
    
    gradeDistribution(dfStudentsMilestonesEv()$grades)
    
  })
  output$gradedistr_text1 <- renderText({
    if(is.null(dfStudentsMilestonesEv())) return("")
    
    str1 <- paste("The mean of this distribution is", 
                  round(mean(dfStudentsMilestonesEv()$grades), 2),
                  "%.")
    
    HTML(paste(str1, sep = '<br />'))
  })
  
  #Num Act vs Total Time vs Grade
  output$nActionsVStoTVSgrade <- renderPlot({
    if(is.null(dfStudentsMilestonesEv())|is.null(dfStudents())) return(NULL)
    
    dfMil <- dfStudentsMilestonesEv()
    dfSt<- dfStudents()
    if(input$checkbox) {ids = "user"} else {ids = "filename"}
    
    nActVStoTVSGrade(dfStudentsMilestones = dfMil, dfStudents = dfSt, id = ids)
  })
  
  output$plotuinActionsVStoTVSgrade <- renderUI({
    if(is.null(dfStudentsMilestonesEv())|is.null(dfStudents())) return(NULL)
    
    plotOutput("nActionsVStoTVSgrade", height=300,
               hover = hoverOpts(
                 id = "plot_hover_nActVStoTVSGrade",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$plot_pointsnActionsVStoTVSgrade <- renderText({
    if(is.null(dfStudentsMilestonesEv())|is.null(dfStudents())) return("")
    
    if(input$checkbox) {ids = "user"} else {ids = "filename"}
    dat <- merge(x = dfStudents(), y = dfStudentsMilestonesEv(), by.x = ids, by.y = "student")
    
    res <- nearPoints(dat, input$plot_hover_nActVStoTVSGrade,
                      xvar="time_on_task", yvar="n_actions", maxpoints = 1,
                      addDist = TRUE)
    response <- unique(as.character(res[,ids]))
    
    if(length(response)==0)
      return ("Place your mouse over a data point to identify the student.") 
    else
      return (paste("Student Id:",response[1]))
  })
  
  #Student-specific
  output$selectStudent <- renderUI({
    if(is.null(dfStudents())) 
      return(selectInput("student", "Select a student..", c("No data loaded")))
    else {
      students<-if(input$checkbox) unique(as.character(dfStudents()$user)) else 
        unique(as.character(dfStudents()$filename))
      students<-students[order(students)]
      return(selectInput("student", "Select a student..", students))
    }})
  
  output$studentCmdHistory <- renderDataTable({
    if(is.null(dfStudents())) return(NULL)
    df<- dfActions()
    
    df <- if(input$checkbox) df[df$user==input$student,] else df[df$filename==input$student,]
    xmls <- as.character(df$xml)
    regs<-regexec("name='Command' value='([^']*?)[']",xmls)
    start<-sapply(regs,function(x){x[2]})
    length<-sapply(regs,function(x){attr(x,"match.length")[2]})
    cmd<-sapply(substr(xmls,start,start+length-1),function(x) {URLdecode(x)})
    
    df <- data.frame(Time=df$time,TimeOnTask=round(df$diff_time_cum_real/60,2),Command=cmd,
                     stringsAsFactors = FALSE)
    df <- df[!df$Command=="NA",]
    df <- df[order(df$Time),]
    return(datatable(df,
                     colnames = c('Time' = 2,
                                  'Time On Task' = 3, 
                                  'Command' = 4), 
                     style ='bootstrap',
                     options = list(pageLength = 50)))
  })
  
  output$studentanalysis_text <- renderText({
    if(is.null(dfStudentsMilestonesEv())&is.null(dfStudents())) return("")
    
    if(is.null(dfStudentsMilestonesEv())) {
      df2 <- dfStudents()
      df2 <- if(input$checkbox) df2[df2$user == input$student,] else df2[df2$filename == input$student,]
      
      text2 <- paste("Time on task:", round(df2$time_on_task,2), "minutes.")
      text3 <- paste("Number of actions:", df2$n_actions, ".")
      
      HTML(paste(text2, text3, sep = '<br />'))
    } else {
      df <- dfStudentsMilestonesEv()
      df2 <- dfStudents()
      
      df <- df[df$student == input$student,]
      df2 <- if(input$checkbox) df2[df2$user == input$student,] else df2[df2$filename == input$student,]
      
      text1 <- paste("Student grade:", round(df$grade,2),"%.")
      text2 <- paste("Time on task:", round(df2$time_on_task,2), "minutes.")
      text3 <- paste("Number of actions:", df2$n_actions, ".")
      
      HTML(paste(text1, text2, text3, sep = '<br />'))
    }
  })
  
  output$heatmapStudent <- renderPlot({
    if(is.null(dfStudentsMilestonesEv())) return(NULL)
    
    df <- dfStudentsMilestonesEv()
    df <- df[df$student == input$student,]
    
    rcmdrtrHeatMapAchievedMilestonePerId(bMilestones = df)
    
  })
})


