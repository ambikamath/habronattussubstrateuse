#Final compiled code for analysis of data in Sun et al. Code by Yuheng (Shirley) Sun and Ambika Kamath
library(lme4)
library(ggplot2)
library(nlme)



#Output from BORIS which lists substrate use as events
path <- "./boris data"
fileNames <- dir(path)
filePath <- sapply(fileNames, function(x){paste(path, x, sep = '/')})
data <- lapply(filePath, function(x){read.csv(x, header = F, skip = 17)})

#loop that finds the first instance of a substrate that is one of "rock", "sand" or "leaves".
firstchoice <- matrix(NA,68, 2)
rock <- 0
leaves <- 0
sand <- 0
ii <- 1
while (ii <= length(data)) {
  testdata = data[[ii]]
  i <- 1
  while (i>0) {
    if (testdata[i,6] == "rock") {
      rock <- rock+1
      firstchoice[ii,] <- c(fileNames[ii], "rock")
      break
    } else if (testdata[i,6] == "leaves") {
      leaves <- leaves+1
      firstchoice[ii,] <- c(fileNames[ii], "leaves")
      break
    } else if (testdata[i,6] == "sand") {
      sand <- sand+1
      firstchoice[ii,] <- c(fileNames[ii], "sand")
      break
    } else {
      i = i+1
    }
  }
  ii = ii+1
}

df=as.data.frame(firstchoice)
colnames(df)=c("ID", "FirstChoice")
df$ID<-gsub(".csv", "", df$ID)


#loop that determines if the spider climbed up the wall before the end of the trial
#subsequent data are not collected for those individuals that climbed walls?


ii <- 1
while (ii <= length(data)) {                          
  testdata = data[[ii]]
  
  i = 1
  found = F
  while (found == F) {                           # Find start point
    if (testdata[i,6] == "in syringe") {
      i = i+1
    } else if (testdata[i,6] == "on syringe") {
      i = i+1
    } else {
      found = T
      sp = testdata[i,1]
    }
  }
  
  nowall = F
  found = F
  while (found == F) {                         # Find end point
    if (testdata[i,6] == "wall") {
      found = T
      ep = testdata[i,1]
    } else if (i >= nrow(testdata)) {
      nowall = T
      break
    } else {
      i = i+1
    }
  }
  
  if (nowall == F) {                       # If climbed wall, calculate time between leaving the syringe and climbing
    t = ep-sp
    df$timetowallclimb[ii] <- t
  } else {                                 # If didn't climb wall, print "didn't climb wall"
    df$timetowallclimb[ii] <- NA
  }


  ii = ii+1
}


df$timetowallclimb=as.numeric(as.character(df$timetowallclimb))


#now loading BORIS output that has times instead of events

path <- "./borisdata2"
fileNames <- dir(path)
filePath <- sapply(fileNames, function(x){paste(path, x, sep = '/')})
data2 <- lapply(filePath, function(x){read.csv(x, header = F, skip=10)})

#assigning variable to describe whether the individual stayed on the first substrate it chose
#i.e. does it spend 0 time on two of the three substrates?

leavestay <- matrix(NA, 68, 2)
ii <- 1
while (ii <= length(data2)) {
  testdata = data2[[ii]]
  leavestay[ii,1] <- fileNames[ii]
  if ((testdata[3,5] == 0 & testdata[4,5] == 0) | (testdata[3,5] == 0 & testdata[5,5] == 0) | (testdata[4,5] == 0 & testdata[5,5] == 0)) {
    leavestay[ii,2] <- c("stay")
  } else {
    leavestay[ii,2] <- c("leave")
  }
  ii = ii+1
}

dfleavestay=as.data.frame(leavestay, stringsAsFactors = F)
colnames(dfleavestay)=c("ID", "LeaveStay")
dfleavestay$ID<-gsub(".csv", "", dfleavestay$ID)


#extracting time spent on each substrate

i <- 1
while (i <= length(data2)) {
  checkdata = data2[[i]]
  dfleavestay$time.rock[i] = checkdata[3,5]
  dfleavestay$time.leaves[i] = checkdata[4,5]
  dfleavestay$time.sand[i] = checkdata[5,5]
  dfleavestay$jumps[i]=checkdata[6,4] + checkdata[7,4] + checkdata[8,4] + checkdata[9,4]
  dfleavestay$jumps.rock[i]=checkdata[6,4] 
  dfleavestay$jumps.leaves[i]=checkdata[7,4] 
  dfleavestay$jumps.sand[i]=checkdata[8,4] 
  i = i+1
}

df=merge(df, dfleavestay, by="ID")
df$trialtime=df$time.leaves+df$time.rock+df$time.sand
hist(df$trialtime)
hist(df$timetowallclimb)

#deciding which individuals to include or not
#In Shirley's thesis, all individuals that spent 400s or more in the arena prior to jumping onto a wall were included in analysis of time-based variables (included1). 
#However, since many of these individuals then returned to the arena after jumping to the wall, and those times are counted, it makes more sense to make the cutoff on the basis of total trial time (included2).600s = individuals spent at least ten minutes within the arena in total.

for (i in 1:nrow(df)){
  df$included1[i]=if (df$timetowallclimb[i]>=300|is.na(df$timetowallclimb[i])) {"yes"} else {"no"}
}
df$included1=as.factor(df$included1)

for (i in 1:nrow(df)){
  df$included2[i]=if (df$trialtime[i]>=600) {"yes"} else {"no"}
}
df$included2=as.factor(df$included2)

#to add sex
path="./"
sex=read.csv("sexetc.csv")
df=merge(df, sex, by="ID")

df$prop.rock=df$time.rock/df$trialtime
df$prop.leaves=df$time.leaves/df$trialtime
df$prop.sand=df$time.sand/df$trialtime

for (i in 1:nrow(df)){
  df$maj[i]= which.max(df[i, 5:7])
}

df$maj=as.factor(as.character(df$maj))
levels(df$maj)=c("rock", "leaves", "sand")
df$maj=factor(df$maj, levels= c("leaves", "rock", "sand"))

for (i in 1:nrow(df)){ 
  df$majtime[i]=if (df$FirstChoice[i]=="sand") {df$prop.sand[i]} else{if(df$FirstChoice[i]=="rock") {df$prop.rock[i]} else {df$prop.leaves[i]}} 
  }
  


#to assess if time spent on each substrate is different, need to restructure data.
dfsub=df[df$included2=="yes",]
dftime=data.frame(rep(dfsub$ID, 3))
colnames(dftime)="ID"
dftime$sex=rep(dfsub$Sex, 3)
 dftime$substrate=as.factor(c(rep("rock", nrow(dfsub)), rep("sand", nrow(dfsub)), rep("leaves", nrow(dfsub))))
 dftime$time=c(dfsub$time.rock, dfsub$time.sand, dfsub$time.leaves)
 dftime$jumps=c(dfsub$jumps.rock, dfsub$jumps.sand, dfsub$jumps.leaves)
 dftime$firstchoice=rep(dfsub$FirstChoice, 3)

dftimem= dftime[dftime$sex=="M",] 
dftimem=droplevels(dftimem)
dftimef=dftime[dftime$sex=="F",]
dftimef=droplevels(dftimef)
 
boxplot(dftime$time~dftime$substrate)
boxplot(dftimem$time~dftimem$substrate)
boxplot(dftimef$time~dftimef$substrate)  


#Okay! Analysis time!

#Are the spiders' first choice of substrate different than expected if they chose randomly?
chisq.test(table(df$FirstChoice))
chisq.test(table(df$FirstChoice)[1:2])
chisq.test(table(df$FirstChoice)[c(1,3)])
chisq.test(table(df$FirstChoice)[2:3])

#Do the sexes differ in their first choice of substrate?
fisher.test(table(df$FirstChoice, df$Sex))

dfm=df[df$Sex=="M",]
dff=df[df$Sex=="F",]

chisq.test(table(dfm$FirstChoice))
chisq.test(table(dff$FirstChoice))

chisq.test(table(dff$FirstChoice)[1:2])
chisq.test(table(dff$FirstChoice)[c(1,3)])
chisq.test(table(dff$FirstChoice)[2:3])


#on which substrate do individuals spend a majority of their time?
friedman.test(time ~ substrate|ID, data = dftime)
#post-hoc

friedman.test(time~substrate|ID, data=droplevels(dftime[dftime$substrate!="sand",]))
friedman.test(time~substrate|ID, data=droplevels(dftime[dftime$substrate!="leaves",]))
friedman.test(time~substrate|ID, data=droplevels(dftime[dftime$substrate!="rock",]))


#for males
friedman.test(time ~ substrate|ID, data = dftimem)
#post-hoc
friedman.test(time~substrate|ID, data=droplevels(dftimem[dftimem$substrate!="sand",]))
friedman.test(time~substrate|ID, data=droplevels(dftimem[dftimem$substrate!="leaves",]))
friedman.test(time~substrate|ID, data=droplevels(dftimem[dftimem$substrate!="rock",]))



friedman.test(time ~ substrate|ID, data = dftimef)
#post-hoc
friedman.test(time~substrate|ID, data=droplevels(dftimef[dftimef$substrate!="sand",]))
friedman.test(time~substrate|ID, data=droplevels(dftimef[dftimef$substrate!="leaves",]))
friedman.test(time~substrate|ID, data=droplevels(dftimef[dftimef$substrate!="rock",]))


#because trial time is a constrained variable, the time spent on any substrate is curtailed by trial time. thus comparing times, we would confound two different questions--are individuals that spend less time on sand spending *relatively* less time on sand or are they just the individuals with shorter trials
#more importantly, time spent on each substrate is not independent of time spent on other substrates, since they must add up to total time. 

kruskal.test(dfsub$majtime, dfsub$FirstChoice)

dftime$prop=c(dfsub$prop.rock, dfsub$prop.sand, dfsub$prop.leaves)
hist(dftime$prop)
#could try the proportion of time spent on each substrate but the distribution of that is very bimodal with peaks at 0 and 1, suggesting that it seems like a binary variable, largely, captured pretty well by "majority"

#So then, what to do about jumps? 
#
jump.model = glmer(jumps~substrate+(1|ID), data=dftime, family=poisson())
summary(jump.model)

dftime$substrate=relevel(dftime$substrate, "rock")
#run again


#And now, figures time!!

plot1=ggplot()

plot1+theme_light(20)+geom_bar(aes(x=df$FirstChoice, fill=df$Sex), stat="count", position=position_dodge2(preserve = "single"))+xlab("First Substrate Chosen")+ ylab("Count")+ scale_fill_manual(values=c("#87D0E2", "#7D5495"), name="Sex")+scale_x_discrete(drop=FALSE, labels=c("Leaf Litter", "Rocks", "Sand"))


plot1+theme_light(20)+geom_bar(aes(x=df$maj, fill=df$Sex), stat="count", position=position_dodge2(preserve = "single"))+xlab("Majority Substrate Chosen")+ ylab("Count")+ scale_fill_manual(values=c("#87D0E2", "#7D5495"), name="Sex")+scale_x_discrete(drop=FALSE, labels=c("Leaf Litter", "Rocks", "Sand"))



plot1+theme_light(20)+ geom_bar(aes(x=dftime$firstchoice, y=dftime$time, fill=dftime$substrate), position="fill", stat="identity")+scale_fill_manual(values=c("#82BA4F", "#E89E23", "#F3EA1F"), labels=c("Leaf Litter", "Rocks", "Sand"), name= "Substrate")+xlab("First Substrate Chosen")+ ylab("Proportion of Time")+scale_x_discrete(drop=FALSE, labels=c("Leaf Litter", "Rocks", "Sand"))


#finally, analysis and figure for vibration transmission data
att <- read.csv("attenuation.csv")

#Analysis
library(nlme)
test <- lme(RMS..dB. ~ Distance..mm.*Substrate, data = att, random = ~1 | Group)
anova(test)

#Plot
#Define summarySE() to calculate standard error
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  #Calculate lengths
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Calculate t-statistic for confidence interval
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#Output the results to a new list
tgc <- summarySE(att, measurevar = "RMS..dB.", groupvars = c("Substrate","Distance..mm."))

#Make the figure

ggplot(tgc, aes(x=Distance..mm., y=RMS..dB., colour=Substrate, shape=Substrate)) + 
  geom_errorbar(aes(ymin=RMS..dB.-se, ymax=RMS..dB.+se), width=.2) +
  scale_color_manual(values=c("#82BA4F", "#E89E23", "#F3EA1F", "black"))+
  geom_line(size=2) +
  geom_point(size=5) +
  scale_shape_manual(values=c(15, 17, 16, 18))+
  ylab("Relative dB") + xlab("Distance(mm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        axis.line = element_line(size=1, colour = "black", arrow = arrow(length = unit(0.5, 'cm'))),
        legend.title = element_text(size=14),
        legend.text = element_text(size = 12))

