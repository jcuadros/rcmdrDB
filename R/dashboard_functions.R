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
  xmls1<-gsub("value='with(?:[(]|%28)[^,%]*(?:,|%2C)(?:%20|%20%28)","value='",xmls)
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

dhms <- function(t){
  paste(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
        ,formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
        ,formatC(t %% 60, width = 2, format = "d", flag = "0")
        ,sep = ":"
  )
  
}

obtainObsAndFormatTime <- function(df){
  
  df$results<-c() 
  df<-within(df, rm(number))
  for(i in (1:nrow(df))){
    df$results[i] <- paste((colnames(df)[which(df[i,1:(ncol(df)-1)] ==  "TRUE")]),collapse = ",")
  }
  
  df$time<-as.POSIXct(strptime(df$time,format="%Y%m%d%H%M%S"))
  df$Date<-as.Date(df$time)
  df$diff_time_cum<-dhms(df$diff_time_cum)
  df<-df[c("filename","xml","diff_time_cum","results")]
  colnames(df)[3] <- "time"
  df$results <- ifelse(df$results == "", NA , df$results)
  return(df)
}

insertEvMilestonesToCmd <- function (df,Evs){
  if(is.null(Evs)){
    df$EvMilestone<- NA
  } else {
  Evs[] <- lapply(Evs, gsub, pattern='om', replacement='')
  Evs$evTests<-gsub("[][]|['']", "", Evs$evTests)
  
  
  df$EvMilestone<- NA
  
  for (x in c(1:nrow(Evs)) ){
    index1<-grepl(Evs$evTests[x], df$results,useBytes = TRUE)
    
    for (y in c(1:length(index1))){
      if(index1[y] == TRUE){
        df$EvMilestone[y]<-Evs$milNames[x]
      }
    }
  }
  }
  return(df)
}

applyRegexAndObtainVariableDataframe <- function(dfActionsSorted) {
  
  xmls<-dfActionsSorted$xml
  xmls <<- xmls
  
  xmls1<-gsub("value='with(?:[(]|%28)[^,%]*(?:,|%2C)(?:%20|%20%28)","value='",xmls)
  xmls1<-gsub("value='[^local](.*?)%3C\\-%20","value='",xmls1)
  
  df<-dfActionsSorted
  xmls <- as.character(df$xml)
  regs<-regexec("name='Command' value='([^']*?)[']",xmls)
  start<-sapply(regs,function(x){x[2]})
  length<-sapply(regs,function(x){attr(x,"match.length")[2]})
  cmd<-sapply(substr(xmls,start,start+length-1),function(x) {URLdecode(x)})
  
  
  #------------------------Obtener COLNAMES-------------------------------
  
  regs<-regexec("name='Result' value='([^']*?)[']",as.character(df$xml))
  start<-sapply(regs,function(x){x[2]})
  length<-sapply(regs,function(x){attr(x,"match.length")[2]})
  Result<-sapply(substr(as.character(df$xml),start,start+length-1),function(x) {URLdecode(x)})
  
  
  
  #--------------------------------------------------------------------------
  
  
  regs2<-regexec("name='Command' value='([a-zA-Z.0-9_]*)(?:[(]|%28)",xmls1)
  start2<-sapply(regs2,function(x){x[2]})
  length2<-sapply(regs2,function(x){attr(x,"match.length")[2]})
  func<-sapply(substr(xmls1,start2,start2+length2-1),function(x) {URLdecode(x)})
  
  nombres<-df$filename
  
  if("results" %in% colnames(dfActionsSorted))
  {
    df <- data.frame(Name=df$filename,Command=cmd,time=df$time,func=func, ObsMilestone = dfActionsSorted$results, EvMilestone = dfActionsSorted$EvMilestone, 
                     stringsAsFactors = FALSE)
  } else {
  df <- data.frame(Name=df$filename,Command=cmd,time=df$time,func=func, 
                   stringsAsFactors = FALSE)
  }
  
  df <- df[!df$Command=="NA",]
  
  
  
  dfResult<- data.frame(Name=nombres,Result=Result, stringsAsFactors = F)
  dfResult <- dfResult[!dfResult$Result=="NA",]
  return(list(dfResult,df)) #dfresult es un dataset que contiene la info para encontrar las variables
}



#Buscar activeDataSet y todos los datasets usados por alumno

obtainActiveDatasetAndallSelectedDatasets <- function(df) {
  
  df<-df[order(df$Name, df$time),]
  
  df$ActiveDataSet<-NA
  j <- NA 
  
  
  for (x in c(2:nrow(df)) ){
    
    if (df$Name[x]==df$Name[x-1]){
      if (grepl("ActiveDataSet",df$Command[x],fixed=T,useBytes = T)){
        j <- substr(df$Command[x],attr(gregexpr(pattern ='ActiveDataSet=',df$Command[x])[[1]], 'match.length')+1,nchar(df$Command[x]))
        df$ActiveDataSet[x] <- j
      }
      
      
    } else {
      j <- NA
    }
  }
  
  dfDs<-df[c("Name","ActiveDataSet")]
  dfDs <- dfDs[!is.na(dfDs$ActiveDataSet),]
  dfDs <- unique(dfDs)
  dfDs <- aggregate(dfDs$ActiveDataSet, list(dfDs$Name), paste, collapse="|")
  colnames(dfDs)[1] <- "Name"
  colnames(dfDs)[2] <- "Dataset"
  df<-df[, !(colnames(df) %in% c("ActiveDataSet"))]
  
  return(list(df,dfDs)) #Return devuelve el df con una columna con el dataset del cmd y un dataframe con cada dataset usado por cada alumno
  
}


searchVariable <- function(dfResult,dfDs,df){
  
  dfResult<-dfResult[order(dfResult$Name),]
  
  dfResult$Variable<-NA
  chr <- NA
  chr2 <- NA
  
  
  for (x in c(2:nrow(df)) ){
    
    if (dfResult$Name[x]==dfResult$Name[x-1]){
      if ( grepl("character",dfResult$Result[x-1],fixed=T)==T & is.na(gregexpr(pattern = "~~",dfResult$Result[x-1])[[1]][1])==F & is.na(gregexpr(pattern = "~~",dfResult$Result[x-1])[[1]][2])==F){
        
        chr<-dfResult$Result[x-1]
        
        chr2<-substr(chr,attr(gregexpr(pattern ='character~~ ',chr)[[1]], 'match.length') + gregexpr(pattern ='character~~ ',chr)[[1]][1],nchar(chr))
        
        dfResult$Variable[x-1] <- chr2
      }
      
    } else {
      chr <- NA
      chr2 <- NA
      
    }
  }
  
  dfResult<-dfResult[c("Name","Variable")]
  dfResult <- dfResult[!is.na(dfResult$Variable),]
  dfResult <- unique(dfResult)
  dfResult$Variable <- str_replace_all(dfResult$Variable,"~~","|")
  dfResult$Variable <- str_replace_all(dfResult$Variable,"\\|mean","")
  dfResult <- aggregate(dfResult$Variable, list(dfResult$Name), paste, collapse="|")
  colnames(dfResult)[1] <- "Name"
  colnames(dfResult)[2] <- "Variable"
  
  #merge
  
  df<-merge(df,dfDs,by="Name")
  df<-merge(df,dfResult,by="Name")
  
  return(df)
}


obtainVariableAndDatasetUsedInEachCmd <- function(df){
  
  df<-df[order(df$Name,df$time),]
  df<-df[, !(colnames(df) %in% c("time"))]
  
  df$isDataSet <- NA
  
  for (j in c(1:nrow(df))) {
    
    m <- match(T,gregexpr(pattern=df$Dataset[j],df$Command[j])[[1]]>0)
    if (is.na(m)) {m <- 0}
    
    if (m > 0){
      
      for (i in c(1:length(as.list(strsplit(df$Dataset[j],"\\|"))[[1]]))) {
        
        b<- as.list(strsplit(df$Dataset[j], "\\|")[[1]])[[i]]
        
        if (grepl(b,df$Command[j],fixed=T, useBytes = TRUE)){  if (is.na(df$isDataSet[j])){df$isDataSet[j] <- b} else {df$isDataSet[j] <- paste(df$isDataSet[j],b)}}
        
      }
    }
  }
  
  df$isVar <- NA
  
  for (j in c(1:nrow(df))) {
    
    m <- match(T,gregexpr(pattern=df$Variable[j],df$Command[j])[[1]]>0)
    if (is.na(m)) {m <- 0}
    
    if (m > 0){
      
      for (i in c(1:length(as.list(strsplit(df$Variable[j],"\\|"))[[1]]))) {
        
        b<- as.list(strsplit(df$Variable[j], "\\|")[[1]])[[i]]
        
        if (grepl(b,df$Command[j],fixed=T)){  if (is.na(df$isVar[j])){df$isVar[j] <- b} else {df$isVar[j] <- paste(df$isVar[j],b,sep='|')}}
        
      }
    }
  }
  return(df)
  
}

obtainVectorWithAllDatasetsUsedInActivity <- function(dfActionsSorted){
  datasetVector <- dfActionsSorted[!(grepl("ActiveDataSet",dfActionsSorted$xml)==F),]
  datasetVector <- datasetVector$xml
  
  datasetVector<- gsub(".*ActiveDataSet%3D","",datasetVector)
  datasetVector<- gsub("\\'.*","",datasetVector)
  datasetVector <- paste(unique(datasetVector),collapse="|")
  return(datasetVector)
  
}

addAsteriskToCommandsWithWrongDataset <- function(dfMilestones,datasetVector,df){
  
  if(!is.null(dfMilestones)){
    for (i in 1:nrow(df)){
      a = 0
      for (j in 1:nrow(dfMilestones)){
        if(df$DataSet[i] != ""){
          if (grepl(substring(df$DataSet[i], 2, nchar(df$DataSet[i])-1),dfMilestones$regExps[j])== F) {
            a = a + 1
          } 
        }
      }
      if(a == nrow(dfMilestones)) {df$x[i] <- paste("*",df$x[i],sep="")}
    }
  }
  
  return(df)
}

eraseDatasetNameAndVariableAndPathFromCmd <- function(df){
  
  df$Command<-gsub("with([^,]*),([^a-zA-Z]*)","with(", df$Command, perl=T)
  df$Command<-gsub("showData([^,]*),([^a-zA-Z]*)","showData(", df$Command, perl=T)
  df$Command<-gsub("load.*","load()", df$Command, perl=T)
  df <- df[, !(colnames(df) %in% c("Dataset","Variable"))]
  df$func <- ifelse(df$func == 'NA', NA, df$func)
  df$Command <- ifelse(is.na(df$isVar)==F,str_replace_all(df$Command,df$isVar,""),df$Command) 
  df$Command <- ifelse(is.na(df$isDataSet)==F,str_replace_all(df$Command,df$isDataSet,""),df$Command)
  return(df)
}

#Agrupar milestones en tres grupos: 1) Obs que tienen Evs 2) Obs sin Evs 3) No Obs no Evs
classifyMilestones <- function(df){
  df$ms <- NA
  df$ms <- ifelse(!is.na(df$obsMilestone) & !is.na(df$evMilestone),"Obs + Ev", df$ms)
  df$ms <- ifelse(!is.na(df$obsMilestone) & is.na(df$evMilestone),"Obs", df$ms)
  return(df)
}

#añadir brackets y parentesis segun formato (var1|var2)[dataset]function_cmd

addBracketsAndParenthesisToGiveSelectorFormat <- function(df){
  
  df$DataSet <- paste('[',df$DataSet,sep='')
  df$DataSet <- paste(df$DataSet,']',sep='')
  df$DataSet <- ifelse(df$DataSet == '[]','',df$DataSet)
  
  df$Variable <- paste('(',df$Variable,sep='')
  df$Variable <- paste(df$Variable,')',sep='')
  df$Variable <- ifelse(df$Variable == '()','',df$Variable)
  
  
  df$Function <- paste(df$Function,'_',sep='')
  df$Function <- ifelse(df$Function == '_','',df$Function)
  
  return(df)
}

#eliminar summaries y anovas que tienen NA en la columna dataset

removeSummariesAndAnovasWithNAInDatasetColumn <- function(df){
  df <- df[!(df$Function == "summary" & is.na(df$DataSet)),]
  df <- df[!(df$Function == "Anova" & is.na(df$DataSet)),]
  
  df$Variable <- ifelse(is.na(df$Variable),'',df$Variable)
  df$DataSet <- ifelse(is.na(df$DataSet),'',df$DataSet)
  df$Function <- ifelse(is.na(df$Function),'',df$Function)
  return(df)
}


#Rellenar con espacios en blanco y hacer distance matrix + cluster para ggplot

createEditDistanceClusterFromFreqTable <- function(cmd_freq){

  long<-max(nchar(names(cmd_freq)))
  for(i in 1:length(names(cmd_freq))) {
    if (nchar(names(cmd_freq)[i])<long){
      names(cmd_freq)[i]<-paste(names(cmd_freq)[i],strrep(" ", -1 + long - nchar(names(cmd_freq)[i])))
    }
  }
  
  d  <- adist(row.names(cmd_freq))
  diag(d) <- NA
  rownames(d) <- row.names(cmd_freq)
  
  hc <- hclust(as.dist(d))
  dhc <- as.dendrogram(hc)
  ddata <- dendro_data(hc, type = "rectangle")
  
  return(ddata)
}



#eliminar el \n que rompe la frase en dos en el eje

deleteLineBreakInLabelsDdata <- function(ddata){

  ddata[["labels"]]$label<-gsub("[\\n]","", ddata[["labels"]]$label, perl=T) 
  return(ddata)
}

#weights iniciales (labels)

addWeightsToDendrogram <- function(ddata,cmd_freq){

  ddata[["segments"]]$freq<-0
  ddata[["labels"]]$freq<-0
  ddata[["segments"]]<- ddata[["segments"]][with(ddata[["segments"]], order(xend)), ] 
  
  names(cmd_freq)<-gsub("[ X]+","", names(cmd_freq), perl=T)
  names(cmd_freq)<-gsub("[\\n]","", names(cmd_freq), perl=T)
  df_freq<-as.data.frame(cmd_freq)
  
  for (i in 1:nrow(df_freq)) {
    for (j in 1:nrow(ddata[["labels"]])){
      if (df_freq[i,"Var1"] == ddata[["labels"]][j, "label"] ) {
        ddata[["labels"]][j, "freq"] = df_freq[i,"Freq"]
      }
    }
  }
  
  for (i in 1:nrow(ddata[["labels"]])) {
    for (j in 1:nrow(ddata[["segments"]])) {
      if ((ddata[["segments"]][j,"xend"] == ddata[["segments"]][j,"x"]) & (ddata[["segments"]][j,"x"] == ddata[["labels"]][i,"x"]) & (ddata[["segments"]][j,"yend"] == 0)){
        ddata[["segments"]][j,"freq"]<-ddata[["labels"]][i,"freq"]
      }
    }
  }
return(ddata)
}

aestheticsSymmetriseOfDendrogram <- function(ddata){
  
  ddata[["segments"]]$x <- round(ddata[["segments"]]$x * 4) / 4
  ddata[["segments"]]$xend <- round(ddata[["segments"]]$xend * 4) / 4
  ddata[["segments"]]$y <- round(ddata[["segments"]]$y * 4) / 4
  ddata[["segments"]]$yend <- round(ddata[["segments"]]$yend * 4) / 4
  return(ddata)
}

introduceMilestonesInDdata <- function(dfMilestone,ddata){
  
  ddata[["labels"]]$Milestone <- NA
  
  for (i in 1:nrow(dfMilestone)){
    for (j in 1:nrow(ddata[["labels"]])){
      if (dfMilestone$Command[i] == ddata[["labels"]]$label[j]) {
        ddata[["labels"]]$Milestone[j] <- dfMilestone$ms[i] 
      }
    }
  }
  
  return(ddata)
}

#añadir espacio para ver el grosor de los segmentos más cortos
addSpaceToSeeInitialWeights <- function(ddata){
  
  ddata[["segments"]]$x <- ifelse(ddata[["segments"]]$x == 0, -5 , ddata[["segments"]]$x)
  ddata[["segments"]]$xend <- ifelse(ddata[["segments"]]$xend == 0, -5 , ddata[["segments"]]$xend)
  ddata[["segments"]]$xy<- ifelse(ddata[["segments"]]$y == 0, -5 , ddata[["segments"]]$y)
  ddata[["segments"]]$yend <- ifelse(ddata[["segments"]]$yend == 0, -5 , ddata[["segments"]]$yend)
  return(ddata)
}

twenty_unique_colours<- function(){
  
  a <- c('#4363d8','#009933',"#808000","#3cb44b","#f58231","#ff0000","#a000a0",
                         '#a9a9a9', '#ffe119', '#f032e6', '#46f0f0', '#fabebe', '#bcf60c', '#e6beff',
                         '#aaffc3', '#fffac8', '#800000', '#ffd8b1', '#911eb4', '#008080', '#000075', 
                          '#808080', '#ffffff', '#000000')
  return(a)
}

#dataset with structure c("Name","Command","time","func") Only func is necessary
classifyFunctionsByCategory <- function(df){
  df$funcGroup <- "Otros"
  df$sigla <- "O"
  
  #Plot
  df$funcGroup <- ifelse(((df$func %in% c("Boxplot","local","Barplot","Dotplot",
                                          "indexplot","plotMeans","qqPlot","densityPlot"))),"plot", df$funcGroup)
  df$sigla <- ifelse(df$funcGroup == "plot","P", df$sigla)
  #anova
  df$funcGroup <- ifelse(((df$func %in% c("lm","aov"))),"anova", df$funcGroup)
  df$sigla <- ifelse(df$funcGroup == "anova","A", df$sigla)
  #load
  df$funcGroup <- ifelse(((df$func %in% c("load"))),"load", df$funcGroup)
  df$sigla <- ifelse(df$funcGroup == "load","L", df$sigla)
  #showData
  df$funcGroup <- ifelse(((df$func %in% c("showData"))),"showData", df$funcGroup)
  df$sigla <- ifelse(df$funcGroup == "showData","D", df$sigla)
  #summary
  df$funcGroup <- ifelse(((df$func %in% c("numSummary","summary"))),"summary", df$funcGroup)
  df$sigla <- ifelse(df$funcGroup == "summary","S", df$sigla)
  #test
  df$funcGroup <- ifelse(((df$func %in% c("Hist","shapiro.test","bartlett.test","t.test","kruskal.test",
                                          "wilcox.test"))),"test", df$funcGroup)
  df$sigla <- ifelse(df$funcGroup == "test","T", df$sigla)
  
  return(df)
}



createCountColumnAscending <- function(df){
  df<-df[order(df$Name, df$time),]
  i=1
  df$Count<- NA
  df$Count[1]=1
  cuanto<-nrow(df)-1
  
  for (x in c(2:nrow(df)) ){
    
    if (df$Name[x]==df$Name[x-1]){
      i=i+1
      df$Count[x]=i
    } else {
      i=1
      df$Count[x]=i
    }
  }
  return(df)
}





plotGroupOfFunctionsVSTime <- function(df){
  #GGPLOT
  df$time <- as.POSIXct(df$time, format ='%H:%M:%OS')
  indice<-df
  indice<-aggregate(indice$time, by = list(indice$Name), max)
  newdata <- indice[order(indice$x, decreasing=FALSE),] #Creciente o decreciente
  newdata<-t(newdata[c(1)])
  df<-df[order(df$Name, df$time,decreasing=TRUE),]
  
  limites <- c("00:00:00","1:45:00")
  limites <- as.POSIXct(limites, format ='%H:%M:%OS')
  
  desired_breaks<-seq(
    from=as.POSIXct(limites[1],"%H:%M", tz="Europe/Madrid"),
    to=as.POSIXct(limites[2], "%H:%M", tz="Europe/Madrid"),
    by="15 min"
  )
  
  colfunc <- colorRampPalette(c("brown", "white"))
  
  twenty_unique_colours<-c('#a9a9a9','#42d4f4',"#ffe119","#3cb44b","#f58231","#ff0000","#72399d",
                           '#46f0f0', '#4363d8', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff',
                           '#911eb4', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', 
                           '#808080', '#ffffff', '#000000')
  
  
  df$sigla <- factor(df$sigla, levels = c("O", "A", "L","P","D","S","T"))
  
  numLeyenda<-length(unique(unlist(df[c("sigla")])))
  
  p <- ggplot(data=df,aes(x=time,y=Name,fill=sigla))+geom_point(shape=25, size=5, alpha=.7)+
    theme_classic() + scale_x_datetime(breaks =desired_breaks, 
                                       limits = limites,labels = scales::date_format("%H:%M", tz = "Europe/Madrid")) +
    geom_hline(yintercept=seq(1.5, length(unique(df$Name))-0.5, 1), lwd=.35, colour="black", linetype="dashed") +
    ylim(newdata) + theme(legend.spacing.y = unit(0.5, 'cm'),legend.key = element_rect(size = 0, color = NA)) +
    scale_fill_manual(values=twenty_unique_colours[1:numLeyenda], name = "Function category", labels = c("O: Other", "A: Anova", "L: Load", "P: Plot", "D: showData","S: Summary","T: Test")) +
    geom_text_repel(size=2.6,label=df$sigla, direction="x",nudge_y = 0.25, vjust=0, force=0.01, box.padding = 0.05,
                    segment.alpha = 0) +
    labs(x = "Time", y="Student id") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"),
          legend.title=element_text(size=16), 
          legend.text=element_text(size=14),
          legend.key.size = unit(1, "cm"))
  return(p)
}

`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

#ordenar el dataframe según la función que quiera usarse para centrar el plot. Entries = (dataframe, sigla de la función escogida, Centrar en la primera o en la última, input de centrar = TRUE)
sortDataframeByCentringOptionsChosen <- function(df,chooseFuncToCentre,firstOrLast,centreTrue){
  
  if(firstOrLast == "First"){headsAndTails <- T} else {headsAndTails <- F}
  
  if(centreTrue){
    if(headsAndTails){
      for(i in 1:length(unique(df$Name))){
        
        if (length(which(df$sigla == chooseFuncToCentre & df$Name == unique(df$Name)[i], arr.ind = TRUE)) == 0){
          df[head(which(df$Name == unique(df$Name)[i], arr.ind = TRUE),n=1):tail(which(df$Name == unique(df$Name)[i], arr.ind = TRUE),n=1),"Count"] %+=% - 
            (df[tail(which(df$Name == unique(df$Name)[i], arr.ind = TRUE),n=1),"Count"] + 1)
          
        } else {
          df[head(which(df$Name == unique(df$Name)[i], arr.ind = TRUE),n=1):tail(which(df$Name == unique(df$Name)[i], arr.ind = TRUE),n=1),"Count"] %+=% - 
            df[min(which(df$sigla == chooseFuncToCentre & df$Name == unique(df$Name)[i], arr.ind = TRUE)),"Count"]
          
        }
      }
      
    } else {
      
      for(i in 1:length(unique(df$Name))){
        
        if (length(which(df$sigla == chooseFuncToCentre & df$Name == unique(df$Name)[i], arr.ind = TRUE)) == 0){
          df[tail(which(df$Name == unique(df$Name)[i], arr.ind = TRUE),n=1):head(which(df$Name == unique(df$Name)[i], arr.ind = TRUE),n=1),"Count"] %+=% - 
            (df[tail(which(df$Name == unique(df$Name)[i], arr.ind = TRUE),n=1),"Count"] + 1)
          
        } else {
          df[tail(which(df$Name == unique(df$Name)[i], arr.ind = TRUE),n=1):head(which(df$Name == unique(df$Name)[i], arr.ind = TRUE),n=1),"Count"] %+=% - 
            df[max(which(df$sigla == chooseFuncToCentre & df$Name == unique(df$Name)[i], arr.ind = TRUE)),"Count"]
          
        }
      }
      
      
    }
    
    
  }
  return(df)
}


plotGroupOfFunctionsSequence <- function(df){
  indice<-df
  indice<-aggregate(indice$Count, by = list(indice$Name), max)
  
  newdata <- indice[order(indice$x, decreasing=TRUE),] #Creciente o decreciente
  newdata<-t(newdata[c(1)])
  
  limites <- c("00:00:00","1:45:00")
  limites <- as.POSIXct(limites, format ='%H:%M:%OS')
  
  desired_breaks<-seq(
    from=as.POSIXct(limites[1],"%H:%M", tz="Europe/Madrid"),
    to=as.POSIXct(limites[2], "%H:%M", tz="Europe/Madrid"),
    by="15 min"
  )
  
  colfunc <- colorRampPalette(c("brown", "white"))
  
  twenty_unique_colours<-c('#a9a9a9','#42d4f4',"#ffe119","#3cb44b","#f58231","#ff0000","#72399d",
                           '#46f0f0', '#4363d8', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff',
                           '#911eb4', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', 
                           '#808080', '#ffffff', '#000000')
  
  
  df$sigla <- factor(df$sigla, levels = c("O", "A", "L","P","D","S","T"))
  numLeyenda<-length(unique(unlist(df[c("sigla")])))
  
  p <- ggplot(data=df,aes(x=Count,y=Name,fill=sigla,label=sigla))+geom_tile(width=.9, height=.8,  alpha=.9,color="black")+
    theme_classic() +
    ylim(newdata) + theme(legend.spacing.y = unit(0.5, 'cm')) +
    scale_fill_manual(values=twenty_unique_colours[1:numLeyenda],  name = "Function category", labels = c("O: Other", "A: Anova", "L: Load", "P: Plot", "D: showData","S: Summary","T: Test")) +
    geom_text(size=3.3, label=df$sigla) +
    labs(x = "relative position", y="Student id") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"),
          legend.title=element_text(size=16), 
          legend.text=element_text(size=14),
          legend.key.size = unit(1, "cm"))
  
  return(p)
  
}

#dataset with structure c("Name","Command","sigla","func")
aggregateCmdGroupOfFunctionsAndInitials <- function(X){
  
  dfCmd <- aggregate(X$Command, list(X$Name), paste, collapse="")
  colnames(dfCmd)[1] <- "Name"
  colnames(dfCmd)[2] <- "Command"
  
  dfFunc <- aggregate(X$func, list(X$Name), paste, collapse="")
  colnames(dfFunc)[2] <- "Function"
  
  dfSigla <- aggregate(X$sigla, list(X$Name), paste, collapse="")
  colnames(dfSigla)[2] <- "Initial"
  
  dfCmd$Function <- dfFunc$Function
  dfCmd$Initial <- dfSigla$Initial
  
  return(dfCmd)
}
roundUp10 <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}

plotStudentCluster <- function(ddata){
  max <- max(ddata[[1]]$y)
  ord<-seq(0,roundUp10(max),roundUp10(max)/10)
  ycolor<-ifelse(ord < 0, "white", "black")
  
  p <- ggplot() +
    geom_segment(data=segment(ddata), aes(x=x, y=y, xend=xend, yend=yend)) + 
    geom_text(data=label(ddata), aes(x=x, y=y, label=label, angle=-90, hjust=-0.1), size=4) + 
    scale_y_continuous(breaks=ord, expand=c(0.25, 0), limits = c(-25,max)) +
    scale_x_continuous(expand=c(0.01, 0.01)) +
    theme(panel.background=element_rect(fill="white"),
          panel.grid=element_blank()) +
    theme(axis.text.x = element_text(colour="white")) + 
    theme(axis.ticks.x = element_line(colour="white")) +
    xlab("") + ylab("height") +
    theme(axis.text.y = element_text(colour = ycolor)) +
    theme(axis.ticks.y = element_line(colour="white"))
  
  return(p)
}
