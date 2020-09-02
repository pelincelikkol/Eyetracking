###################################
# Eye tracking data preprocessing #
###################################

# This script loads eyemovement and behavioral data, detect the number of
# fixations and microsaccades per trial, creates and saves data frame for
# each subject.

rm(list=ls())
N <- 5 #number of participants
L <- N*2 #number of iterations
s <- 1 #start from this subject

for (i in (((s-1)*2)+1):L){
  #----------------------------------------
  # 1. Load eyemovement and behavioral data
  #----------------------------------------
  
  setwd('G:/EEG_and_eyetracking_data/eyetracking') # setwd to data file location
  
  # read eyelink .asc data 
  require("eyelinker")
  sub <- readline(prompt="Subject number: ")
  ses <- readline(prompt="Baseline(0), Notraining(1), Training(2): ")
  
  if (ses == "0") {
    file_eye <- paste("./base",sub,".asc",sep="")} else if (ses == "2"){
                        file_eye <- paste("./train",sub,".asc", sep="")}
  dat <- read.asc(file_eye)
  raw <- dat$raw # raw eyemovement data 
  
  # Detect pause intervals
  msg <-dat$msg
  pauses <- msg$block[msg$text=="PAUSE"]
  pause_times <- matrix(data=NA, nrow=length(pauses), ncol=2)
  
  for (i in 1:length(pauses)){
    pause_times[i,] = c(msg$time[msg$block==pauses[i] & msg$text=="PAUSE"], 
                        msg$time[msg$block==(pauses[i]+1) & msg$text=="STARTTIME"])}
  
  # Exclude pause intervals from dataframe
  for (i in 1:length(pauses)) {
    raw <- subset(raw, time<pause_times[i,1] | time>pause_times[i,2])}
  
  # read behavioral data 
  require(R.matlab)
  file_beh <- paste("G:/EEG_and_eyetracking_data/behav_data/FixCrossExp_s",sub,
                    "_ses",ses,"cfgdata.mat",sep="")
  dat_cfg <- readMat(file_beh)
  exp_data <- dat_cfg$data #data collected during the experiment
  
  #---------------------
  # 2. Create Data frame
  #---------------------
  
  ntrials <- 1500
  
  eye_df <- data.frame(sub=rep(sub,ntrials),
                       block=NA,
                       trial=rep(1:ntrials),
                       cross=NA,
                       training=NA,
                       category=NA,
                       response=NA,
                       checkResp=NA,
                       microsac=NA,
                       fixation=NA)
  
  #--------------------------------------------
  # 3. Assign experimental data into data frame
  #--------------------------------------------
  
  if (ses==0){eye_df$training = "baseline"} else if(ses==2){eye_df$training = "training"}
  blocks <- t(dat_cfg$block)
  cond <- dat_cfg$cond #standard(2) or bullseye(1)
  resp <- exp_data[8] #keyboard press
  resp <- t(resp[[1]])
  cat <- exp_data[[7]] #category
  
  eye_df$block <- blocks[,1]
  eye_df$cross <- as.factor(cond[,1])
  eye_df$response <- resp[,1]
  for (i in 1:ntrials){eye_df$category[i] <- cat[[i]]}
  
  #check keyboard responses
  eye_df$checkResp[eye_df$category=="paperclip" & eye_df$response==1] ='Hit'
  eye_df$checkResp[eye_df$category!="paperclip" & eye_df$response==1] ='FA'
  eye_df$checkResp[eye_df$category!="paperclip" & eye_df$response==0] ='CR'
  eye_df$checkResp[eye_df$category=="paperclip" & eye_df$response==0] ='Miss'
  
  eye_df$acc <- round(length(eye_df$checkResp[eye_df$checkResp=='Hit']) / 
    (length(eye_df$checkResp[eye_df$checkResp=='Miss'])+
       length(eye_df$checkResp[eye_df$checkResp=='Hit'])),2)
  
  #-------------------------------
  # 4. Microsaccades and fixations
  #-------------------------------
  
  #------------------
  # 4.1 Microsaccades
  #------------------
  
  # Set wd to ms toolbox location
  setwd('G:/EEG_and_eyetracking_data/MS_Toolbox_R/')
  
  # Load functions for microsaccade detection
  source("vecvel.R")
  source("microsacc.R")
  
  # Set parameters
  SAMPLING = 1000
  MINDUR = 6 #duration criterion
  VFAC = 5 #velocity criterion
  
  # Detect microsaccades in all trials and store them in a vector
  ms_all <- c()
  for (i in 1:1500){
    xyp <- as.matrix(na.omit(raw[raw$block==i,][,3:4]))
    if (dim(xyp)[1] > 0)
      ms <- microsacc(xyp,VFAC,MINDUR,SAMPLING)
    else 
      ms_all[i] <- 0
    
    if (is.null(ms$table)==TRUE)
      ms_all[i] <- 0
    else
      ms_all[i] <- nrow(ms$table)
  }
  
  # Assign the number of microsaccades into the data frame
  eye_df$microsac <- ms_all
  
  setwd('G:/EEG_and_eyetracking_data/eyetracking') #back to old directory
  
  #--------------
  # 4.2 Fixations
  #--------------
  
  #if the 'saccades' package has not installed, these are the commands to run: 
  #require("devtools")
  #install_github("tmalsburg/saccades/saccades", dependencies=TRUE) 
  
  require("saccades")
  
  #we only need the trial, x position, y position and time columns
  raw_fix <- raw[,c(2,3,4,1)]
  
  #rename the columns for the function to read
  names(raw_fix)[names(raw_fix) == "xp"] <- "x"
  names(raw_fix)[names(raw_fix) == "yp"] <- "y"
  names(raw_fix)[names(raw_fix) == "block"] <- "trial"
  raw_fix$time <- raw_fix$time - raw_fix$time[1]
  raw_fix <- na.omit(raw_fix)
  fixation <- detect.fixations(raw_fix) #returns a data frame of fixation properties 
  
  #exclude NAs (blinks) 
  fixation <- na.omit(fixation)
  
  #exclude fixations longer than 1000ms and shorter than 20ms
  fixation <- subset(fixation, dur>20, dur<1000)
  
  stats_fix<- calculate.summary(fixation) #this returns the statistical properties
  save(stats_fix, file=paste("fix_stats",sub,ses,".Rda", sep=""))
  
  #the number of fixations per trial
  fix_vec <- c()
  for(i in 1:ntrials){fix_vec[i] <-length(fixation$trial[fixation$trial==i])}
  
  eye_df$fixation <- fix_vec
  
  #--------------------
  # 5. Save data frames
  #--------------------
  
  if (ses == 0) {
    base_df <- eye_df
    save(base_df,file = paste("base_df",sub,".Rda", sep=""))} else if (ses == 2) {
      train_df <- eye_df
      save(train_df, file = paste("train_df",sub,".Rda",sep=""))}
  
  if (exists("base_df")==TRUE & exists("train_df")==TRUE) {
    basetrain_df <- rbind(base_df, train_df)
    basetrain_df$training = as.factor(basetrain_df$training)
    save(basetrain_df, file =paste("basetrain",sub,".Rda",sep=""))
    rm(list=ls())}
  else {
    next
  }
}

