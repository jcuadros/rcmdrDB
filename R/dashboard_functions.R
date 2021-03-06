timeLimit <- 300 #5-minutes and longer stops are discounted

#From https://gist.github.com/jimhester/6355622#file-read-file-cpp
#Suggested from https://gist.github.com/hadley/6353939
sourceCpp("readFile.cpp")

#Import actions
importStudentData <- function(filesImport,names = NULL) {
  
  # Importar els fitxers
  vecXMLActions<-NULL
  vecFileNames<-NULL
  
  for(i in 1:length(filesImport)) {
    # lines<-readLines(filesImport[i])
    lines<-read_file_cpp2(filesImport[i])
    lines<-gsub("\r","\n",lines)
    lines<-gsub("\n\n","\n",lines)
    lines<-unlist(strsplit(lines,"\n"))
    vecFileNames<-c(vecFileNames,rep(
      if(is.null(names)) filesImport[i] else names[i],length(lines)))
    vecXMLActions<-c(vecXMLActions,lines)
  }
  
  numActions<-length(vecXMLActions)
  
  # Muntatge del dfAccions
  vecApplication<-rep(NULL,numActions)
  vecAction<-rep(NULL,numActions)
  vecUser<-rep(NULL,numActions)
  vecSession<-rep(NULL,numActions)
  vecNumber<-rep(NULL,numActions)
  vecTime<-rep(NULL,numActions)
  vecType<-rep(NULL,numActions)
  vecParamsName<-rep(NULL,numActions)
  vecParamsValue<-rep(NULL,numActions)
  
  for (i in 1:numActions) {
    
    objXMLDoc<-xmlTreeParse(vecXMLActions[i],asText=TRUE,trim=TRUE)
    lisXMLDoc<-xmlToList(objXMLDoc)
    vecApplication[i]<-as.character(lisXMLDoc$.attrs["application"])
    vecAction[i]<-as.character(lisXMLDoc$.attrs["action"])
    vecUser[i]<-as.character(lisXMLDoc$.attrs["user"])
    vecSession[i]<-as.character(lisXMLDoc$.attrs["session"])
    vecNumber[i]<-as.numeric(as.character(lisXMLDoc$.attrs["number"]))
    vecTime[i]<-as.character(lisXMLDoc$.attrs["time"])
    vecType[i]<-as.character(lisXMLDoc$.attrs["type"])
    vecParamsName[i]<-as.character(lisXMLDoc[[1]])
    vecParamsValue[i]<-as.character(lisXMLDoc[[2]])
  }
  
  
  dfActions<-data.frame(gsub(".txt","",vecFileNames),vecXMLActions,vecApplication,
                        vecAction,vecUser,vecSession,vecNumber,vecTime,vecType,
                        vecParamsName,vecParamsValue)
  
  colnames(dfActions)<-c("filename","xml","application","action","user","session","number",
                         "time","type","param_name","param_value")
  
  # Taula amb les accions ordenades
  dfActionsSorted<-dfActions[order(dfActions[,"filename"],dfActions[,"user"],dfActions[,"session"],
                                   as.character(dfActions[,"time"])),]
  
  #Calcul del temps per accio
  vecDiffTime<-rep(NA,numActions)
  vecDiffTimeCum<-rep(0, numActions)
  vecDiffTimeCumReal<-rep(0, numActions)
  
  # Do not accumulate XML
  # vecXMLCum<-rep(NA, numActions)
  # vecXMLCum[1]<-as.character(dfActionsSorted[1,"xml"])
  vecXMLCum<-as.character(dfActionsSorted[,"xml"])
  
  for(i in 2:numActions) {
    
    if(dfActionsSorted[i,"filename"]==dfActionsSorted[i-1,"filename"] && 
       dfActionsSorted[i,"user"]==dfActionsSorted[i-1,"user"] && 
       dfActionsSorted[i,"session"]==dfActionsSorted[i-1,"session"]) {
      thisTime<-as.numeric(as.POSIXct(as.character(dfActionsSorted[i,"time"]),
                                      format="%Y%m%d%H%M%OS"))
      previousTime<-as.numeric(as.POSIXct(as.character(dfActionsSorted[i-1,"time"]),
                                          format="%Y%m%d%H%M%OS"))
      vecDiffTime[i]<-thisTime-previousTime
      
      vecDiffTimeCum[i]<-vecDiffTime[i]+vecDiffTimeCum[i-1]
      vecDiffTimeCumReal[i]<-ifelse(vecDiffTime[i]>timeLimit,0,vecDiffTime[i])+vecDiffTimeCumReal[i-1]
      # vecXMLCum[i]<-paste(vecXMLCum[i-1],as.character(dfActionsSorted[i,"xml"]),sep="")
    } else {
      vecDiffTime[i]<-NA
      vecDiffTimeCum[i]<-0
      vecDiffTimeCumReal[i]<-0
      # vecXMLCum[i]<-as.character(dfActionsSorted[i,"xml"])
    }
  }
  
  dfActionsSorted<-cbind(dfActionsSorted,vecDiffTime,vecDiffTimeCum,vecDiffTimeCumReal,vecXMLCum)
  colnames(dfActionsSorted)<-c("filename","xml","application","action","user","session","number",
                               "time","type","param_name","param_value","diff_time","diff_time_cum",
                               "diff_time_cum_real","xml_cum")
  return(dfActionsSorted)
}

#Without Milestones
generatedfFiles <- function (actions) {
  if(is.null(actions)) return(NULL)
  
  # Muntatge de dfSessions
  vecSessions<-unique(as.character(actions[,"session"]))
  
  vecMinId<-character(length(vecSessions))
  vecMaxId<-character(length(vecSessions))
  vecMinTime<-character(length(vecSessions))
  vecMaxTime<-character(length(vecSessions))
  vecNActions<-numeric(length(vecSessions))
  vecUser<-character(length(vecSessions))
  vecFile<-character(length(vecSessions))
  vecNSeq<-numeric(length(vecSessions))
  vecTimeOnTask<-numeric(length(vecSessions))
  
  for (i in 1:length(vecSessions)) {
    dfActionsPerSession<-actions[as.character(actions[,"session"])==vecSessions[i],]
    dfActionsPerSessionSorted<-dfActionsPerSession[order(dfActionsPerSession[,"number"]),]
    primer<-head(dfActionsPerSessionSorted,1)
    darrer<-tail(dfActionsPerSessionSorted,1)
    vecMinId[i]<-primer[,"number"]
    vecMaxId[i]<-darrer[,"number"]
    vecMinTime[i]<-as.character(primer[,"time"])
    vecMaxTime[i]<-as.character(darrer[,"time"])
    vecNActions[i]<-nrow(dfActionsPerSessionSorted)
    vecFile[i]<-as.character(primer[,"filename"])
    vecUser[i]<-as.character(primer[,"user"])
    vecNSeq[i]<-max(table(dfActionsPerSessionSorted[,"number"]))
    vecTimeOnTask[i]<-darrer[,"diff_time_cum_real"]/60
  }
  
  vecSessionStart<-strptime(vecMinTime, "%Y%m%d%H%M%OS")
  vecSessionEnd<-strptime(vecMaxTime, "%Y%m%d%H%M%OS")
  vecDurationSession<-as.numeric(difftime(vecSessionEnd,vecSessionStart,units="mins"))
  
  dfSessions<-data.frame(vecSessions,vecFile,vecUser,vecNActions,vecNSeq,vecMinId,vecMaxId,vecMinTime,
                         vecMaxTime,vecDurationSession,vecTimeOnTask)
  colnames(dfSessions)<-c("session","filename","user","n_actions","n_seq","min_number","max_number",
                          "min_time","max_time","duration","time_on_task")
  
  # Muntatge de dfFiles
  vecFiles<-unique(as.character(actions[,"filename"]))
  
  vecMinTime<-character(length(vecFiles))
  vecMaxTime<-character(length(vecFiles))
  vecNActions<-numeric(length(vecFiles))
  vecNSessions<-numeric(length(vecFiles))
  vecNUsers<-numeric(length(vecFiles))
  vecNSeq<-numeric(length(vecFiles))
  vecDuration<-numeric(length(vecFiles))
  vecTimeOnTask<-numeric(length(vecFiles))
  
  for (i in 1:length(vecFiles)) {
    dfActionsPerFile<-actions[as.character(actions[,"filename"])==vecFiles[i],]
    dfSessionsPerFile<-dfSessions[as.character(dfSessions[,"filename"])==vecFiles[i],]
    
    dfActionsPerFileSorted<-dfActionsPerFile[order(dfActionsPerFile[,"session"],dfActionsPerFile[,"number"]),]
    dfSessionsPerFileSorted<-dfSessionsPerFile[order(dfSessionsPerFile[,"session"]),]
    
    dfSessionsPerFileSorted$duration_previous<-rep(0,nrow(dfSessionsPerFileSorted))
    dfSessionsPerFileSorted$time_on_task_previous<-rep(0,nrow(dfSessionsPerFileSorted))
    if (nrow(dfSessionsPerFileSorted)>1) {
      for(s in 2:nrow(dfSessionsPerFileSorted)) {
        dfSessionsPerFileSorted$duration_previous[s]<-sum(dfSessionsPerFileSorted[which(as.character(dfSessionsPerFileSorted[,"session"])
                                                                                        < as.character(dfSessionsPerFileSorted[s,"session"])),"duration"])
        dfSessionsPerFileSorted$time_on_task_previous[s]<-sum(dfSessionsPerFileSorted[which(as.character(dfSessionsPerFileSorted[,"session"])
                                                                                            < as.character(dfSessionsPerFileSorted[s,"session"])),"time_on_task"])	
      }
    }
    
    primer<-head(dfActionsPerFileSorted,1)
    darrer<-tail(dfActionsPerFileSorted,1)
    vecMinTime[i]<-as.character(primer[,"time"])
    vecMaxTime[i]<-as.character(darrer[,"time"])
    vecNActions[i]<-nrow(dfActionsPerFileSorted)
    vecNSessions[i]<-nrow(dfSessionsPerFileSorted)
    vecNUsers[i]<-length(unique(dfActionsPerFileSorted[,"user"]))
    
    # com a suma de sessions, en minuts
    vecDuration[i]<-sum(dfSessionsPerFileSorted[,"duration"])
    vecTimeOnTask[i]<-sum(dfSessionsPerFileSorted[,"time_on_task"])
  }
  dfFiles<-data.frame(vecFiles,vecNSessions,vecNUsers,vecNActions,vecMinTime,
                      vecMaxTime,vecDuration,vecTimeOnTask)
  colnames(dfFiles)<-c("filename","n_sessions","n_users","n_actions",
                       "min_time","max_time","duration","time_on_task")
  
  return(dfFiles)
}

generatedfUsers <- function (actions) {
  if(is.null(actions)) return(NULL)
  
  # Muntatge de dfSessions
  vecSessions<-unique(as.character(actions[,"session"]))
  
  vecMinId<-character(length(vecSessions))
  vecMaxId<-character(length(vecSessions))
  vecMinTime<-character(length(vecSessions))
  vecMaxTime<-character(length(vecSessions))
  vecNActions<-numeric(length(vecSessions))
  vecUser<-character(length(vecSessions))
  vecFile<-character(length(vecSessions))
  vecNSeq<-numeric(length(vecSessions))
  vecTimeOnTask<-numeric(length(vecSessions))
  
  for (i in 1:length(vecSessions)) {
    dfActionsPerSession<-actions[as.character(actions[,"session"])==vecSessions[i],]
    dfActionsPerSessionSorted<-dfActionsPerSession[order(dfActionsPerSession[,"number"]),]
    primer<-head(dfActionsPerSessionSorted,1)
    darrer<-tail(dfActionsPerSessionSorted,1)
    vecMinId[i]<-primer[,"number"]
    vecMaxId[i]<-darrer[,"number"]
    vecMinTime[i]<-as.character(primer[,"time"])
    vecMaxTime[i]<-as.character(darrer[,"time"])
    vecNActions[i]<-nrow(dfActionsPerSessionSorted)
    vecFile[i]<-as.character(primer[,"filename"])
    vecUser[i]<-as.character(primer[,"user"])
    vecNSeq[i]<-max(table(dfActionsPerSessionSorted[,"number"]))
    vecTimeOnTask[i]<-darrer[,"diff_time_cum_real"]/60
  }
  
  vecSessionStart<-strptime(vecMinTime, "%Y%m%d%H%M%OS")
  vecSessionEnd<-strptime(vecMaxTime, "%Y%m%d%H%M%OS")
  vecDurationSession<-as.numeric(difftime(vecSessionEnd,vecSessionStart,units="mins"))
  
  dfSessions<-data.frame(vecSessions,vecFile,vecUser,vecNActions,vecNSeq,vecMinId,vecMaxId,vecMinTime,
                         vecMaxTime,vecDurationSession,vecTimeOnTask)
  colnames(dfSessions)<-c("session","filename","user","n_actions","n_seq","min_number","max_number",
                          "min_time","max_time","duration","time_on_task")
  
  #Muntatge dfUsers.
  vecUsers<-unique(as.character(actions[,"user"]))
  
  vecMinTime<-character(length(vecUsers))
  vecMaxTime<-character(length(vecUsers))
  vecNActions<-numeric(length(vecUsers))
  vecNSessions<-numeric(length(vecUsers))
  vecNFiles<-numeric(length(vecUsers))
  vecNSeq<-numeric(length(vecUsers))
  vecDuration<-numeric(length(vecUsers))
  vecTimeOnTask<-numeric(length(vecUsers))
  
  
  for (i in 1:length(vecUsers)) {
    dfActionsPerUser<-actions[as.character(actions[,"user"])==vecUsers[i],]
    dfSessionsPerUser<-dfSessions[as.character(dfSessions[,"user"])==vecUsers[i],]
    
    dfActionsPerUserSorted<-dfActionsPerUser[order(dfActionsPerUser[,"session"],dfActionsPerUser[,"number"]),]
    dfSessionsPerUserSorted<-dfSessionsPerUser[order(dfSessionsPerUser[,"session"]),]
    
    dfSessionsPerUserSorted$duration_previous<-rep(0,nrow(dfSessionsPerUserSorted))
    dfSessionsPerUserSorted$time_on_task_previous<-rep(0,nrow(dfSessionsPerUserSorted))
    if (nrow(dfSessionsPerUserSorted)>1) {
      for(s in 2:nrow(dfSessionsPerUserSorted)) {
        dfSessionsPerUserSorted$duration_previous[s]<-sum(dfSessionsPerUserSorted[which(as.character(dfSessionsPerUserSorted[,"session"])
                                                                                        < as.character(dfSessionsPerUserSorted[s,"session"])),"duration"])
        dfSessionsPerUserSorted$time_on_task_previous[s]<-sum(dfSessionsPerUserSorted[which(as.character(dfSessionsPerUserSorted[,"session"])
                                                                                            < as.character(dfSessionsPerUserSorted[s,"session"])),"time_on_task"])	
      }
    }
    
    primer<-head(dfActionsPerUserSorted,1)
    darrer<-tail(dfActionsPerUserSorted,1)
    vecMinTime[i]<-as.character(primer[,"time"])
    vecMaxTime[i]<-as.character(darrer[,"time"])
    vecNActions[i]<-nrow(dfActionsPerUserSorted)
    vecNSessions[i]<-nrow(dfSessionsPerUserSorted)
    vecNFiles[i]<-length(unique(dfActionsPerUserSorted[,"filename"]))
    
    # com a suma de sessions, en minuts
    vecDuration[i]<-sum(dfSessionsPerUserSorted[,"duration"])
    vecTimeOnTask[i]<-sum(dfSessionsPerUserSorted[,"time_on_task"])
    
  }
  
  dfUsers<-data.frame(vecUsers,vecNSessions,vecNFiles,vecNActions,vecMinTime,
                      vecMaxTime,vecDuration,vecTimeOnTask)
  colnames(dfUsers)<-c("user","n_sessions","n_files","n_actions",
                       "min_time","max_time","duration","time_on_task")
  
  return(dfUsers)
}

#With Milestones
generatedfActionsMilestones <- function (actions, dfMilestones) {
  
  dfActionsSorted <- actions
  
  milestones <- as.character(dfMilestones$milNames)
  regExps <- as.character(dfMilestones$regExps)
  postLogicEval <- as.character(dfMilestones$logTests)
  
  milestonesRegExp <- regExps
  milestonesName <- milestones
  milestonesLogEv <- postLogicEval
  
  milestones <- regExps
  milestones[1:length(milestonesName)]<-milestonesName
  postLogicEval<-rep("",length(regExps))
  postLogicEval[1:length(milestonesLogEv)]<-milestonesLogEv
  
  # Milestones per accio
  testVector<-as.character(dfActionsSorted$xml_cum)
  regExps<-paste("(?=",regExps,")",sep="")
  
  dfRegExpsPerElement<-data.frame()
  for(i in 1:length(testVector)) {
    lisRegExpsElement<-logical(length(regExps))
    for (j in 1:length(regExps)) {
      regexecResults<-gregexpr(regExps[j],testVector[i],perl=TRUE)
      
      evaluated<-FALSE
      for(k in 1:length(regexecResults[[1]])) {
        result<-regexecResults[[1]]
        if(result[1]!=-1 & length(result)>0) {
          m<-NULL
          
          if(!is.null(attr(regexecResults[[1]],"capture.start"))) {
            
            result<-as.vector(attr(regexecResults[[1]],"capture.start")[k,])
            attr(result,"match.length")<-as.vector(attr(regexecResults[[1]],"capture.length")[k,])
            
            for(l in 1:length(result)) {
              posMatch<-result[l]
              lenMatch<-attr(result,"match.length")[l]
              m<-c(m,substr(as.character(testVector[i]),
                            posMatch,posMatch+lenMatch-1))
            }
          }
        }
        evaluated<-evaluated | (postLogicEval[j]=="" & result[1]!=-1)
        plj <<- postLogicEval
        if(postLogicEval[j]!="" & result[1]!=-1) {
          evaluated<-evaluated | as.logical(eval(parse(text=postLogicEval[j])))
        }			
      }
      lisRegExpsElement[j]<-evaluated
    }
    dfRegExpsPerElement<-rbind(dfRegExpsPerElement,lisRegExpsElement)
  }
  colnames(dfRegExpsPerElement)<-milestones
  
  cbind(dfActionsSorted,dfRegExpsPerElement)
}  
  

generatedfFilesMilestones <- function (actionsMilestones, dfMilestones) {
  
  dfActionsSortedMilestones<-actionsMilestones
  
  milestones <- as.character(dfMilestones$milNames)
  regExps <- as.character(dfMilestones$regExps)
  postLogicEval <- as.character(dfMilestones$logTests)
  
  milestonesRegExp <- regExps
  milestonesName <- milestones
  milestonesLogEv <- postLogicEval
  
  milestones <- regExps
  milestones[1:length(milestonesName)]<-milestonesName
  postLogicEval<-rep("",length(regExps))
  postLogicEval[1:length(milestonesLogEv)]<-milestonesLogEv
  
  # Muntatge de dfSessions
  vecSessions<-unique(as.character(dfActionsSortedMilestones[,"session"]))
  
  vecMinId<-character(length(vecSessions))
  vecMaxId<-character(length(vecSessions))
  vecMinTime<-character(length(vecSessions))
  vecMaxTime<-character(length(vecSessions))
  vecNActions<-numeric(length(vecSessions))
  vecUser<-character(length(vecSessions))
  vecFile<-character(length(vecSessions))
  vecNSeq<-numeric(length(vecSessions))
  vecTimeOnTask<-numeric(length(vecSessions))
  
  
  dfMilestonesPerSession<-data.frame(matrix(nrow=length(vecSessions),
                                            ncol=length(milestones)*4),row.names=as.character(vecSessions))
  
  colnames(dfMilestonesPerSession)<-c(paste("n_",milestones,sep=""),paste("t_",milestones,sep=""),
                                      paste("dt_",milestones,sep=""),
                                      paste("dtr_",milestones,sep="")) 
  
  for (i in 1:length(vecSessions)) {
    dfActionsPerSession<-dfActionsSortedMilestones[as.character(dfActionsSortedMilestones[,"session"])==vecSessions[i],]
    dfActionsPerSessionSorted<-dfActionsPerSession[order(dfActionsPerSession[,"number"]),]
    primer<-head(dfActionsPerSessionSorted,1)
    darrer<-tail(dfActionsPerSessionSorted,1)
    vecMinId[i]<-primer[,"number"]
    vecMaxId[i]<-darrer[,"number"]
    vecMinTime[i]<-as.character(primer[,"time"])
    vecMaxTime[i]<-as.character(darrer[,"time"])
    vecNActions[i]<-nrow(dfActionsPerSessionSorted)
    vecFile[i]<-as.character(primer[,"filename"])
    vecUser[i]<-as.character(primer[,"user"])
    vecNSeq[i]<-max(table(dfActionsPerSessionSorted[,"number"]))
    vecTimeOnTask[i]<-darrer[,"diff_time_cum_real"]/60
    
    for(m in 1:length(milestones)) {
      dfActionPerSessionMilestone<-dfActionsPerSessionSorted[
        dfActionsPerSessionSorted[,milestones[m]]==TRUE,]
      dfMilestonesPerSession[i,paste("n_",milestones[m],sep="")]<-
        nrow(dfActionPerSessionMilestone)
      if(nrow(dfActionPerSessionMilestone)>0) {
        dfMilestonesPerSession[i,paste("t_",milestones[m],sep="")]<-
          as.character(head(dfActionPerSessionMilestone,1)[,"time"])
        dfMilestonesPerSession[i,paste("dt_",milestones[m],sep="")]<-
          as.character(head(dfActionPerSessionMilestone,1)[,"diff_time_cum"])
        dfMilestonesPerSession[i,paste("dtr_",milestones[m],sep="")]<-
          as.character(head(dfActionPerSessionMilestone,1)[,"diff_time_cum_real"])
      }
    }
  }
  
  
  vecSessionStart<-strptime(vecMinTime, "%Y%m%d%H%M%OS")
  vecSessionEnd<-strptime(vecMaxTime, "%Y%m%d%H%M%OS")
  vecDurationSession<-as.numeric(difftime(vecSessionEnd,vecSessionStart,units="mins"))
  
  
  dfSessions<-data.frame(vecSessions,vecFile,vecUser,vecNActions,vecNSeq,vecMinId,vecMaxId,vecMinTime,
                         vecMaxTime,vecDurationSession,vecTimeOnTask,dfMilestonesPerSession)
  colnames(dfSessions)<-c("session","filename","user","n_actions","n_seq","min_number","max_number",
                          "min_time","max_time","duration","time_on_task",colnames(dfMilestonesPerSession))
  
  
  vecFiles<-unique(as.character(dfActionsSortedMilestones[,"filename"]))
  
  vecMinTime<-character(length(vecFiles))
  vecMaxTime<-character(length(vecFiles))
  vecNActions<-numeric(length(vecFiles))
  vecNSessions<-numeric(length(vecFiles))
  vecNUsers<-numeric(length(vecFiles))
  vecNSeq<-numeric(length(vecFiles))
  vecDuration<-numeric(length(vecFiles))
  vecTimeOnTask<-numeric(length(vecFiles))
  
  dfMilestonesPerFile<-data.frame(matrix(nrow=length(vecFiles),
                                         ncol=length(milestones)*4),row.names=as.character(vecFiles))
  colnames(dfMilestonesPerFile)<-c(paste("n_",milestones,sep=""),paste("t_",milestones,sep=""),
                                   paste("dt_",milestones,sep=""),
                                   paste("dtr_",milestones,sep=""))
  
  for (i in 1:length(vecFiles)) {
    dfActionsPerFile<-dfActionsSortedMilestones[as.character(dfActionsSortedMilestones[,"filename"])==vecFiles[i],]
    dfSessionsPerFile<-dfSessions[as.character(dfSessions[,"filename"])==vecFiles[i],]
    
    dfActionsPerFileSorted<-dfActionsPerFile[order(dfActionsPerFile[,"session"],dfActionsPerFile[,"number"]),]
    dfSessionsPerFileSorted<-dfSessionsPerFile[order(dfSessionsPerFile[,"session"]),]
    
    dfSessionsPerFileSorted$duration_previous<-rep(0,nrow(dfSessionsPerFileSorted))
    dfSessionsPerFileSorted$time_on_task_previous<-rep(0,nrow(dfSessionsPerFileSorted))
    if (nrow(dfSessionsPerFileSorted)>1) {
      for(s in 2:nrow(dfSessionsPerFileSorted)) {
        dfSessionsPerFileSorted$duration_previous[s]<-sum(dfSessionsPerFileSorted[which(as.character(dfSessionsPerFileSorted[,"session"])
                                                                                        < as.character(dfSessionsPerFileSorted[s,"session"])),"duration"])
        dfSessionsPerFileSorted$time_on_task_previous[s]<-sum(dfSessionsPerFileSorted[which(as.character(dfSessionsPerFileSorted[,"session"])
                                                                                            < as.character(dfSessionsPerFileSorted[s,"session"])),"time_on_task"])	
      }
    }
    
    primer<-head(dfActionsPerFileSorted,1)
    darrer<-tail(dfActionsPerFileSorted,1)
    vecMinTime[i]<-as.character(primer[,"time"])
    vecMaxTime[i]<-as.character(darrer[,"time"])
    vecNActions[i]<-nrow(dfActionsPerFileSorted)
    vecNSessions[i]<-nrow(dfSessionsPerFileSorted)
    vecNUsers[i]<-length(unique(dfActionsPerFileSorted[,"user"]))
    
    # com a suma de sessions, en minuts
    vecDuration[i]<-sum(dfSessionsPerFileSorted[,"duration"])
    vecTimeOnTask[i]<-sum(dfSessionsPerFileSorted[,"time_on_task"])
    
    for(m in 1:length(milestones)) {
      dfMilestonesPerFile[i,paste("n_",milestones[m],sep="")]<-
        sum(dfSessionsPerFileSorted[,paste("n_",milestones[m],sep="")],
            na.rm=TRUE)
      if(dfMilestonesPerFile[i,paste("n_",milestones[m],sep="")] > 0) {
        dfMilestonesPerFile[i,paste("t_",milestones[m],sep="")]<-
          min(dfSessionsPerFileSorted[,paste("t_",milestones[m],sep="")],
              na.rm=TRUE)
        dfMilestonesPerFile[i,paste("dt_",milestones[m],sep="")]<-
          min(as.numeric(dfSessionsPerFileSorted[,paste("dt_",milestones[m],sep="")])+
                dfSessionsPerFileSorted[,"duration_previous"]*60,na.rm=TRUE)
        dfMilestonesPerFile[i,paste("dtr_",milestones[m],sep="")]<-
          min(as.numeric(dfSessionsPerFileSorted[,paste("dtr_",milestones[m],sep="")])+
                dfSessionsPerFileSorted[,"time_on_task_previous"]*60,na.rm=TRUE)
      }
    }	
  }
  
  
  dfFiles<-data.frame(vecFiles,vecNSessions,vecNUsers,vecNActions,vecMinTime,
                      vecMaxTime,vecDuration,vecTimeOnTask,dfMilestonesPerFile)
  colnames(dfFiles)<-c("filename","n_sessions","n_users","n_actions",
                       "min_time","max_time","duration","time_on_task",colnames(dfMilestonesPerFile))
  
  
  dfFiles<-data.frame(dfFiles$filename,
                      dfFiles[,paste("n_",milestones,sep="")]>0)
  
  rownames(dfFiles)<-1:nrow(dfFiles)
  colnames(dfFiles)<-c("filename",milestones)
  
  return(dfFiles)
}

generatedfUsersMilestones <- function (actionsMilestones,dfMilestones) {
  
  dfActionsSortedMilestones<-actionsMilestones
  
  milestones <- as.character(dfMilestones$milNames)
  regExps <- as.character(dfMilestones$regExps)
  postLogicEval <- as.character(dfMilestones$logTests)
  
  milestonesRegExp <- regExps
  milestonesName <- milestones
  milestonesLogEv <- postLogicEval
  
  milestones <- regExps
  milestones[1:length(milestonesName)]<-milestonesName
  postLogicEval<-rep("",length(regExps))
  postLogicEval[1:length(milestonesLogEv)]<-milestonesLogEv
  
  # Muntatge de dfSessions
  vecSessions<-unique(as.character(dfActionsSortedMilestones[,"session"]))
  
  vecMinId<-character(length(vecSessions))
  vecMaxId<-character(length(vecSessions))
  vecMinTime<-character(length(vecSessions))
  vecMaxTime<-character(length(vecSessions))
  vecNActions<-numeric(length(vecSessions))
  vecUser<-character(length(vecSessions))
  vecFile<-character(length(vecSessions))
  vecNSeq<-numeric(length(vecSessions))
  vecTimeOnTask<-numeric(length(vecSessions))
  
  dfMilestonesPerSession<-data.frame(matrix(nrow=length(vecSessions),
                                            ncol=length(milestones)*4),row.names=as.character(vecSessions))
  
  
  colnames(dfMilestonesPerSession)<-c(paste("n_",milestones,sep=""),paste("t_",milestones,sep=""),
                                      paste("dt_",milestones,sep=""),
                                      paste("dtr_",milestones,sep="")) 
  
  for (i in 1:length(vecSessions)) {
    dfActionsPerSession<-dfActionsSortedMilestones[as.character(dfActionsSortedMilestones[,"session"])==vecSessions[i],]
    dfActionsPerSessionSorted<-dfActionsPerSession[order(dfActionsPerSession[,"number"]),]
    primer<-head(dfActionsPerSessionSorted,1)
    darrer<-tail(dfActionsPerSessionSorted,1)
    vecMinId[i]<-primer[,"number"]
    vecMaxId[i]<-darrer[,"number"]
    vecMinTime[i]<-as.character(primer[,"time"])
    vecMaxTime[i]<-as.character(darrer[,"time"])
    vecNActions[i]<-nrow(dfActionsPerSessionSorted)
    vecFile[i]<-as.character(primer[,"filename"])
    vecUser[i]<-as.character(primer[,"user"])
    vecNSeq[i]<-max(table(dfActionsPerSessionSorted[,"number"]))
    vecTimeOnTask[i]<-darrer[,"diff_time_cum_real"]/60
    
    
    for(m in 1:length(milestones)) {
      dfActionPerSessionMilestone<-dfActionsPerSessionSorted[
        dfActionsPerSessionSorted[,milestones[m]]==TRUE,]
      dfMilestonesPerSession[i,paste("n_",milestones[m],sep="")]<-
        nrow(dfActionPerSessionMilestone)
      if(nrow(dfActionPerSessionMilestone)>0) {
        dfMilestonesPerSession[i,paste("t_",milestones[m],sep="")]<-
          as.character(head(dfActionPerSessionMilestone,1)[,"time"])
        dfMilestonesPerSession[i,paste("dt_",milestones[m],sep="")]<-
          as.character(head(dfActionPerSessionMilestone,1)[,"diff_time_cum"])
        dfMilestonesPerSession[i,paste("dtr_",milestones[m],sep="")]<-
          as.character(head(dfActionPerSessionMilestone,1)[,"diff_time_cum_real"])
      }
    }
  }
  
  vecSessionStart<-strptime(vecMinTime, "%Y%m%d%H%M%OS")
  vecSessionEnd<-strptime(vecMaxTime, "%Y%m%d%H%M%OS")
  vecDurationSession<-as.numeric(difftime(vecSessionEnd,vecSessionStart,units="mins"))
  
  
  dfSessions<-data.frame(vecSessions,vecFile,vecUser,vecNActions,vecNSeq,vecMinId,vecMaxId,vecMinTime,
                         vecMaxTime,vecDurationSession,vecTimeOnTask,dfMilestonesPerSession)
  
  colnames(dfSessions)<-c("session","filename","user","n_actions","n_seq","min_number","max_number",
                          "min_time","max_time","duration","time_on_task",colnames(dfMilestonesPerSession))
  
  # Muntatge de dfUsers
  vecUsers<-unique(as.character(dfActionsSortedMilestones[,"user"]))
  
  vecMinTime<-character(length(vecUsers))
  vecMaxTime<-character(length(vecUsers))
  vecNActions<-numeric(length(vecUsers))
  vecNSessions<-numeric(length(vecUsers))
  vecNFiles<-numeric(length(vecUsers))
  vecNSeq<-numeric(length(vecUsers))
  vecDuration<-numeric(length(vecUsers))
  vecTimeOnTask<-numeric(length(vecUsers))
  
  dfMilestonesPerUser<-data.frame(matrix(nrow=length(vecUsers),
                                         ncol=length(milestones)*4),row.names=as.character(vecUsers))
  
  colnames(dfMilestonesPerUser)<-c(paste("n_",milestones,sep=""),paste("t_",milestones,sep=""),
                                   paste("dt_",milestones,sep=""),
                                   paste("dtr_",milestones,sep=""))
  
  for (i in 1:length(vecUsers)) {
    dfActionsPerUser<-dfActionsSortedMilestones[as.character(dfActionsSortedMilestones[,"user"])==vecUsers[i],]
    dfSessionsPerUser<-dfSessions[as.character(dfSessions[,"user"])==vecUsers[i],]
    
    dfActionsPerUserSorted<-dfActionsPerUser[order(dfActionsPerUser[,"session"],dfActionsPerUser[,"number"]),]
    dfSessionsPerUserSorted<-dfSessionsPerUser[order(dfSessionsPerUser[,"session"]),]
    
    dfSessionsPerUserSorted$duration_previous<-rep(0,nrow(dfSessionsPerUserSorted))
    dfSessionsPerUserSorted$time_on_task_previous<-rep(0,nrow(dfSessionsPerUserSorted))
    if (nrow(dfSessionsPerUserSorted)>1) {
      for(s in 2:nrow(dfSessionsPerUserSorted)) {
        dfSessionsPerUserSorted$duration_previous[s]<-sum(dfSessionsPerUserSorted[which(as.character(dfSessionsPerUserSorted[,"session"])
                                                                                        < as.character(dfSessionsPerUserSorted[s,"session"])),"duration"])
        dfSessionsPerUserSorted$time_on_task_previous[s]<-sum(dfSessionsPerUserSorted[which(as.character(dfSessionsPerUserSorted[,"session"])
                                                                                            < as.character(dfSessionsPerUserSorted[s,"session"])),"time_on_task"])	
      }
    }
    
    primer<-head(dfActionsPerUserSorted,1)
    darrer<-tail(dfActionsPerUserSorted,1)
    vecMinTime[i]<-as.character(primer[,"time"])
    vecMaxTime[i]<-as.character(darrer[,"time"])
    vecNActions[i]<-nrow(dfActionsPerUserSorted)
    vecNSessions[i]<-nrow(dfSessionsPerUserSorted)
    vecNFiles[i]<-length(unique(dfActionsPerUserSorted[,"filename"]))
    
    # com a suma de sessions, en minuts
    vecDuration[i]<-sum(dfSessionsPerUserSorted[,"duration"])
    vecTimeOnTask[i]<-sum(dfSessionsPerUserSorted[,"time_on_task"])
    
    for(m in 1:length(milestones)) {
      dfMilestonesPerUser[i,paste("n_",milestones[m],sep="")]<-
        sum(dfSessionsPerUserSorted[,paste("n_",milestones[m],sep="")],
            na.rm=TRUE)
      
      if(dfMilestonesPerUser[i,paste("n_",milestones[m],sep="")] > 0) {
        dfMilestonesPerUser[i,paste("t_",milestones[m],sep="")]<-
          min(dfSessionsPerUserSorted[,paste("t_",milestones[m],sep="")],
              na.rm=TRUE)
        dfMilestonesPerUser[i,paste("dt_",milestones[m],sep="")]<-
          min(as.numeric(dfSessionsPerUserSorted[,paste("dt_",milestones[m],sep="")])+
                dfSessionsPerUserSorted[,"duration_previous"]*60,na.rm=TRUE)
        dfMilestonesPerUser[i,paste("dtr_",milestones[m],sep="")]<-
          min(as.numeric(dfSessionsPerUserSorted[,paste("dtr_",milestones[m],sep="")])+
                dfSessionsPerUserSorted[,"time_on_task_previous"]*60,na.rm=TRUE)
      }
    }	
  }
  
  dfUsers <- data.frame(vecUsers,vecNSessions,vecNFiles,vecNActions,vecMinTime,
                        vecMaxTime,vecDuration,vecTimeOnTask,dfMilestonesPerUser)
  
  colnames(dfUsers)<-c("user","n_sessions","n_files","n_actions",
                       "min_time","max_time","duration","time_on_task",colnames(dfMilestonesPerUser))
  
  dfUsers<-data.frame(dfUsers$user,
                      dfUsers[,paste("n_",milestones,sep="")]>0)
  
  rownames(dfUsers)<-1:nrow(dfUsers)
  colnames(dfUsers)<-c("user",milestones)
  
  return(dfUsers)
}

generatedStudentsMilestonesEv <- function (studentsObsMilestones,dfMilestonesEv) {
  if(is.null(studentsObsMilestones)) return(NULL)
  if(is.null(dfMilestonesEv)) {
    df<-studentsObsMilestones
    colnames(df)[1]<-"student"
  } else {
    df <- data.frame(as.character(studentsObsMilestones[,1]))
    om <- studentsObsMilestones[,-1]
    
    evNames <- as.character(dfMilestonesEv[,1])
    evTests <- as.character(dfMilestonesEv[,2])
    evTests <- gsub("om['","om[,'",evTests,fixed=TRUE)
    
    for(j in 1:nrow(dfMilestonesEv)) {
      df <- cbind(df,as.logical(eval(parse(text=evTests[j]))))
    }              
    colnames(df)<-c("student",evNames)
  }
  
  df$grades <- (rowSums(df[,2:length(df)])/(length(df)-1))*100
  df
}

generatedfObs <- function(actionsMilestones, milestones) {
  selCols <- c("filename","user","number","time","diff_time_cum","diff_time_cum_real")[
    c("filename","user","number","time","diff_time_cum","diff_time_cum_real") %in%
    colnames(actionsMilestones)] 
  actionsMilestones[
    apply(actionsMilestones[,milestones],1,sum)>0,
    c(selCols,milestones)]
}


#COLORS
getdfOIColors <- function(dfObsItems, dfEVMil) {
  if(is.null(dfObsItems)) return(NULL)
  
  dfObsItemsEvMilest<-data.frame(item=as.character(dfObsItems$milNames),
                                 stringsAsFactors = FALSE)
  if(is.null(dfEVMil)) {
    dfObsItemsEvMilest$evMil <- dfObsItemsEvMilest$item
  } else {
    dfObsItemsEvMilest$evMil <- 
      sapply(dfObsItemsEvMilest$item,
             function(x) {paste(ifelse(grepl(x,dfEVMil$evTests),
                                       as.character(dfEVMil$milNames),""),collapse = "")})
  }
  
  ncolors <- length(unique(dfObsItemsEvMilest$evMil[dfObsItemsEvMilest$evMil!=""]))
  colorsMil <- rep(c(brewer.pal(9,"Set1"),brewer.pal(12,"Set3")),100)[1:ncolors]
  if (ncolors!=length(unique(dfObsItemsEvMilest$evMil))) colorsMil <- c(colorsMil, "grey60")
  
  dfEvColor <- data.frame(evMil=unique(dfObsItemsEvMilest$evMil),colorMil=colorsMil)
  dfObsItemsEvMilest <- merge(dfObsItemsEvMilest,dfEvColor,by="evMil") %>% arrange(item)
  dfObsItemsEvMilest$colorInk <- ifelse(sapply(dfObsItemsEvMilest$colorMil, function(x) mean(ColToRgb(x))/255)<.5,
                                         "white","black")
  dfObsItemsEvMilest
}

#GRAPHS.
#without milestones.
plotDistribution <- function(time=NA,maxTime=NULL,xlabel="") {
  if(is.null(time)) return(NULL)
  
  if (is.null(maxTime)) {
    maxTime<-max(time,na.rm=TRUE)
    maxTime<-ceiling(maxTime/10^floor(log(maxTime)/log(10)-1))*10^floor(log(maxTime)/log(10)-1)
  }
  
  # Freedman-Diaconis rule
  bw <- maxTime / (2 * IQR(time) / length(time)^(1/3))
  
  bw2 <- floor(bw/10^floor(log(bw)/log(10)))
  bw2<-ifelse(bw2>=5,5,ifelse(bw2>=2,2,1))
  bw2<-bw2*10^floor(log(bw)/log(10))
  
  nbins<-ceiling(diff(range(time))/bw2)
  bw2<-ifelse(nbins>=7,bw2,bw2/2+.Machine$double.eps)
  bw2 <- floor(bw2/10^floor(log(bw2)/log(10)))*10^floor(log(bw2)/log(10))
  
  ggmaxTime<-ceiling(maxTime/bw2)*bw2
  
  dense<-density(time,adjust=1,bw="SJ")
  df_density=data.frame(x=dense$x,y=dense$y/sum(dense$y)*bw2*
                          length(time)/mean(diff(dense$x)))
  theme_remove_all <- theme(
    axis.text.x = element_text(margin=margin(0,0,0,0,"lines")),
    axis.ticks.length = unit(0, "cm"))
  
  ghist<-ggplot(data=NULL,aes(x=time)) +
    geom_histogram(color="black", alpha=.2, fill="skyblue",
                   breaks=seq(0,ggmaxTime, by=bw2))+ 
    geom_area(data=df_density,aes(x=x,y=y),
              alpha=.2, fill="#FFFF00", color="#CC6600", size=1) + 
    geom_vline(aes(xintercept=mean(time)), linetype="dashed", size=1)+
    geom_rug(alpha=.5) +
    labs(x=xlabel, y="Frequency")+
    coord_cartesian(xlim= c(0, ggmaxTime)) +
    theme_bw() + theme_remove_all +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))
  
  ghist
}

rcmdrtrTimeEventsPerId <- function(time=NA,ids=NA,sessions=NA,colorsScale=NULL) {
  if(is.null(time)|is.null(ids)|is.null(sessions)) return(NULL)
  
  uniqueIds<-unique(as.character(ids))
  ordering<-order(as.character(ids),as.character(sessions),as.character(time))
  
  time<-time[ordering]
  sessions <- sessions[ordering]
  ids<-ids[ordering]
  
  RealTime<-rep(0,length(time))
  
  for(i in 2:length(time)) {
    if(as.character(ids[i])==as.character(ids[i-1])) {
      thisTime<-as.numeric(as.POSIXct(as.character(time[i]),
                                      format="%Y%m%d%H%M%OS"))
      previousTime<-as.numeric(as.POSIXct(as.character(time[i-1]),
                                          format="%Y%m%d%H%M%OS"))
      diffTime <- thisTime-previousTime
      
      RealTime[i]<-ifelse(diffTime > timeLimit |
        as.character(sessions[i])!=as.character(sessions[i-1]), 0, diffTime/60) +
        RealTime[i-1]
    } else {
      diffTime<-NA
      RealTime[i] <- 0
    }
  }
  
  maxTime<-numeric(length(uniqueIds))
  for (i in 1:length(uniqueIds)) {
    maxTime[i]<-max(RealTime[as.character(ids[])==as.character(uniqueIds[i])],na.rm=TRUE)
  }
  uniqueIdsSorted<-as.character(uniqueIds[order(maxTime)])
  
  #Plot
  linea <- as.data.frame(cbind(1:length(maxTime), maxTime[order(maxTime)]))
  colnames(linea) <- c("position", "RealTime")
  
  idsWithPositions <- merge(data.frame(ids),data.frame(ids=uniqueIdsSorted,
                                                       position=1:length(uniqueIdsSorted)),
                            by=c("ids","ids"), incomparables=NA)
  
  punts <- as.data.frame(cbind(idsWithPositions, RealTime))
  return (list(punts, linea, maxTime))
}

drawPlotActionPoints <- function (punts, linea, maxTime) {
  
  ggplot(punts, aes(x = position, y = RealTime)) + 
    geom_point(size = 2, alpha = 0.2) + geom_line(data = linea, aes(x = position, y = RealTime)) +
    labs(y="Time (in minutes)") + ylim(c(0,max(maxTime,na.rm=TRUE))) + theme_bw() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
}

drawCommandsWordCloud <- function(xmls) {
  if(is.null(xmls)) return(NULL)
  
  xmls <<- xmls
  xmls1<-gsub("value='with(?:[(]|%28)[^,%]*(?:,|%2C)%20","value='",xmls)
  regs<-regexec("name='Command' value='([a-zA-Z.0-9_]*)(?:[(]|%28)",xmls1)
  start<-sapply(regs,function(x){x[2]})
  length<-sapply(regs,function(x){attr(x,"match.length")[2]})
  cmd<-sapply(substr(xmls1,start,start+length-1),function(x) {URLdecode(x)})
  names(cmd)<-NULL
  
  corpus_cmd<-VCorpus(VectorSource(cmd[!is.na(cmd)]))
  tdm<-TermDocumentMatrix(corpus_cmd)
  return(wordcloud(Terms(tdm),row_sums(tdm),min.freq=1,
         max.words=50,rot.per=0,fixed.asp=FALSE, 
         random.order=F, colors=brewer.pal(8, "Dark2")))
}

#With milestones.
rcmdrtrMilestonesDifficulty <- function(bMilestones=NULL) {
  if (is.null(bMilestones)) return(NULL) 
  
  vecExits<-100*apply(bMilestones[,-1],2,sum)
  dfResultats<-as.data.frame(rbind(yes=vecExits/nrow(bMilestones),no=100-vecExits/nrow(bMilestones)))
  dfResultats$grades <- NULL
  
  dfResults <- dfResultats
  
  par(las=2) # make label text perpendicular to axis
  par(mar=c(12,5,2,2)) # increase y-axis margin.
  par(cex=0.7)
  
  barplot(as.matrix(dfResultats),space=0,col=c("skyblue","white"))
  
}

rcmdrtrHeatMapAchievedMilestonePerId<-function(bMilestones=NULL,labels=NULL) {
  if (!is.null(bMilestones)) {
    require(gplots)
    
    nMilestones<-data.matrix(bMilestones[,-1])
    if(!is.null(labels)) {
      rownames(nMilestones)<-labels
    } else {
      rownames(nMilestones)<-bMilestones[,1]
    }
    mode(nMilestones)<-"numeric"
    
    colHM<-c("#EEEEEEFF","skyblue")
    
    heatmap.2(nMilestones,dendrogram="row",Colv=NULL,Rowv=TRUE,margins=c(6,6),
              cexRow=0.6,cexCol=0.8,breaks=c(-0.5,0.5,1.5),key=FALSE,trace="none",
              scale="none",col=colHM,
              colsep=0:ncol(nMilestones)-1,rowsep=0:nrow(nMilestones)-1,
              lmat=rbind(c(4,3),c(2,1)), 
              lhei=c(0.5,9.5),
              lwid=c(2,8))
  }	
}

gradeDistribution <- function(dfStudentsMilestonesEv=NA) {
  if(is.null(dfStudentsMilestonesEv)) return(NULL)
  
  ggplot(data=NULL,aes(x=dfStudentsMilestonesEv)) +
    geom_histogram(color="black", alpha=.2, fill="skyblue", binwidth=5, boundary = 0) + 
    geom_vline(aes(xintercept=mean(dfStudentsMilestonesEv)), linetype="dashed", size=1) +
    geom_rug(alpha=.5) +
    labs(x="Grades", y="Frequency")+theme_bw()
}

nActVStoTVSGrade <- function (dfStudents=NULL, dfStudentsMilestones=NULL, id=NULL){
  
  if(is.null(dfStudents)|is.null(dfStudentsMilestones)|is.null(id)) return(NULL)
  
  df <- merge(x = dfStudents, y = dfStudentsMilestones, by.x = id, by.y = "student")
  
  ggplot(df, aes(x=time_on_task, y=n_actions)) +  
    geom_point(size = I(4), alpha = I(0.8), aes(color = grades)) +
    labs(x = "Time on Task", y = "Number of Actions") + scale_color_gradientn(
      colors=c("red4","red","white","green","forestgreen"),values=c(0,.4,.5,.6,1))+theme_bw()
}
