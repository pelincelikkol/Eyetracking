##############################################
# Data simulation for 2x3 factorial design   #
#                                            #
# Factors: Fixation Cross, Fixation Training #
# Levels:  Standard/Bulls eye                #
#          Baseline/Notraining/Training      #
##############################################

library("dplyr")
library("lme4")
library("MASS")
set.seed(2)

#the mean number of microsaccades per trial for each condition is as follows:
(M <- as.table(matrix(c(3, 2.5, 
              2.8, 2.3,
              1.5,1), 
              ncol=2, byrow=TRUE,
              dimnames=list(c("base", "notraining", "training"),
                            c("standard", "bulls")))))


#Define trials, blocks
ntrials <- 1500
nblock <- 10
ntrial <- 150

#randomize the order of fixation cross 
cross <- rep(c("a", "b"),5) #standard=a, bulls=b
rand_cross <- rep(sample(cross), each=150)

#------------------------------------------
# 1.Simulated data for "base + no training"
#------------------------------------------

training <- rep(c("base","notraining"), each=ntrials)

#data of a single subject
dat <- data.frame(sub=rep(1,ntrials*2),
                   block=rep(1:nblock, each=ntrial),
                   trial=rep(1:ntrials,2),
                   cross=as.factor(rand_cross),
                   training=as.factor(training),
                   saccade=NA)

#assign DV per trial for each condition with rnorm
dat$saccade[dat$cross=="a" & dat$training=="base"] = rnorm(750,3,1)
dat$saccade[dat$cross=="b" & dat$training=="base"] = rnorm(750,2.5,1)
dat$saccade[dat$cross=="a" & dat$training=="notraining"] =rnorm(750,2.8,1)
dat$saccade[dat$cross=="b" & dat$training=="notraining"]=rnorm(750,2.3,1)

#fit a linear model to simulate DV for same conditions
model <- lm(saccade ~ training*cross, data=dat)

#create data frame for 15 subjects "base + notraining"
dat_notrain <- data.frame(sub=rep(1:15,each=ntrials*2),
                   block=rep(1:nblock, each=ntrial),
                   trial=rep(1:ntrials,2),
                   cross=as.factor(rand_cross),
                   training=as.factor(training),
                   saccade=NA)

#assign simulated DV based on the model
for (i in 1:15){
  dat_notrain$saccade[dat_notrain$sub==i] <- simulate(model)[,1]
}


#---------------------------------------
# 2.Simulated data for "base + training"
#---------------------------------------

training1 <- rep(c("base","training"), each=ntrials)

#data of a single subject
dat_train1 <- data.frame(sub=rep(1,ntrials*2),
                  block=rep(1:nblock, each=ntrial),
                  trial=rep(1:ntrials,2),
                  cross=as.factor(rand_cross),
                  training=as.factor(training1),
                  saccade=NA)

#assign DV with rnorm
dat_train1$saccade[dat_train1$cross=="a" & dat_train1$training=="base"] = rnorm(750,3,1)
dat_train1$saccade[dat_train1$cross=="b" & dat_train1$training=="base"] = rnorm(750,2.5,1)
dat_train1$saccade[dat_train1$cross=="a" & dat_train1$training=="training"] =rnorm(750,1.5,1)
dat_train1$saccade[dat_train1$cross=="b" & dat_train1$training=="training"]=rnorm(750,1,1)

#fit the model
model1 <- lm(saccade ~ training*cross, data=dat_train1)

#data frame for 15 subjects "base + training"
dat_train <- data.frame(sub=rep(16:30,each=ntrials*2),
                          block=rep(1:nblock, each=ntrial),
                          trial=rep(1:ntrials,2),
                          cross=as.factor(rand_cross),
                          training=as.factor(training1),
                          saccade=NA)


#assign DV based on the model
for (i in 16:30){
  dat_train$saccade[dat_train$sub==i] <- simulate(model1)[,1]
}

#------------------------------
# 3. Create complete data frame
#------------------------------

dat_all <- rbind(dat_notrain,dat_train)

#add random effects by subject
randeff <- rnorm(30,0,.5) 
rand <- rep(randeff, each=3000)
dat_all$saccade <- dat_all$saccade + rand

#-------------
# 4.Statistics
#-------------

#means by condition
dat_all %>% group_by(training,cross) %>% summarize(mean=mean(saccade)) %>%
  data.frame()

#apply contrast coding
contrasts(dat_all$training) <- contr.sdif(3)
contrasts(dat_all$cross) <- contr.sum(2)

#fit linear mixed model
model2 <- lmer(saccade~training*cross + (1+cross*training|sub), data=dat_all)
summary(model2)

