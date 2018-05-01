# Ismail, Qatrunnada
# 2018-04-22



rm(list=ls())  # Clears all the variables from memory.
graphics.off() # Clears all the graphs.


# Set working directory 
#setwd("~/Box Sync/newds/Service/ISCC-consulting/Ismail, Qatrunnada")
setwd("/Users/QN/THIRD-crowdperm-analysis/R-code")


library(car)
library(reshape)
library(Hmisc)
library(nlme)
library(lme4)
library(gplots)
library(MASS)
library (gmodels)

# Overview of the analysis 

## Part 1: Using the survey data 
# Do users feel more comfortable with some permissions than others? 
# What's the relationship between comfort level and enable/disable? 
# Does comfort level influence uninstall and reinstall? 


# Part 2: Using the background data 
# Are some permissons more likely to be enabled/disabled? 
# What is the relationship between usage time and % enabled permissions? 
# Does the app cateogry influence permissions? 
# What's the relationship between perceived danger and allowed permissons? 


################################
################################
################################


# Part 1: Using the survey data 

## Do users feel more comfortable with some permissions than others? 
data1 = read.csv("surveyDatacsv.csv", header = T,stringsAsFactors = FALSE)
names(data1)
data1$avg.comfort <- (rowMeans(data1[17:21],na.rm=T))

leveneTest(avg.comfort ~ as.factor(permission), data = data1) #Test of Homogeneity of Variances
summary(aov(data1$avg.comfort ~ data1$permission)) # ANOVA
pairwise.t.test(data1$avg.comfort, data1$permission, p.adj = "bonf") # Multiple comparisons


data1$permission[data1$permission=="CONTACTS"] <- "Con"
data1$permission[data1$permission=="CAMERA"] <- "Cam"
data1$permission[data1$permission=="LOCATION"] <- "Loc"
data1$permission[data1$permission=="STORAGE"] <- "Stor"
data1$permission[data1$permission=="MICROPHONE"] <- "MIC"
data1$permission[data1$permission=="PHONE"] <- "Phone"

plotmeans(as.numeric(avg.comfort) ~ permission,xlab="Permission",
          ylab="Average Comfort Score", main="Mean Plot with 95% CI", 
          n.label = T, data = data1) 



# What's the relationship between comfort level and enable/disable? 
data2 = read.csv("surveyDatacsv.csv", header = T)
names(data2)


vars <- c("id",
          "changesettingsApp1",
          "changesettingsApp2",
          "changesettingsApp3",
          "changesettingsApp4",
          "changesettingsApp5",
          "comfortLevelApp1",
          "comfortLevelApp2",
          "comfortLevelApp3",
          "comfortLevelApp4",
          "comfortLevelApp5"
          )
data2.subset <- data2[vars]



meltdata2 <- melt(data2.subset, id=c("id",
                                    "changesettingsApp1",
                                    "changesettingsApp2",
                                    "changesettingsApp3",
                                    "changesettingsApp4",
                                    "changesettingsApp5"
                                    ))

newdata2 <- melt(meltdata2, id=c("id","variable","value"))

names(newdata2) <- c("id","comfort.app","comfort.val","change.app","change.app.per")

d1 <- newdata2[ which(newdata2$comfort.app=='comfortLevelApp1'& newdata2$change.app=='changesettingsApp1'), ]
d2 <- newdata2[ which(newdata2$comfort.app=='comfortLevelApp2'& newdata2$change.app=='changesettingsApp2'), ]
d3 <- newdata2[ which(newdata2$comfort.app=='comfortLevelApp3'& newdata2$change.app=='changesettingsApp3'), ]
d4 <- newdata2[ which(newdata2$comfort.app=='comfortLevelApp4'& newdata2$change.app=='changesettingsApp4'), ]
d5 <- newdata2[ which(newdata2$comfort.app=='comfortLevelApp5'& newdata2$change.app=='changesettingsApp5'), ]

combined <- rbind.data.frame(d1,d2,d3,d4,d5)

newdata2.subset <- na.omit(combined)

#does comfort level affect the decision of changing permission? no
summary(glmer(change.app.per ~ as.factor(comfort.val) + (1 | id), data = newdata2.subset, family = binomial))

#the other way around, does changing permission affect comfort level?
summary(glmer(as.factor(comfort.val)~change.app.per + (1 | id), data = newdata2.subset, family = binomial))




# Does comfort level influence App uninstall and reenable? 
data3 = read.csv("surveyDatacsv.csv", header = T, stringsAsFactors = F)

names(data3)

data3$storyUninstallComfort[data3$storyUninstallComfort=="strongly agree"] <- 5
data3$storyUninstallComfort[data3$storyUninstallComfort=="agree"] <- 4
data3$storyUninstallComfort[data3$storyUninstallComfort=="neutral"] <- 3
data3$storyUninstallComfort[data3$storyUninstallComfort=="disagree"] <- 2
data3$storyUninstallComfort[data3$storyUninstallComfort=="strongly disagree"] <- 1
data3$storyEnableComfort[data3$storyEnableComfort=="strongly agree"] <- 5
data3$storyEnableComfort[data3$storyEnableComfort=="agree"] <- 4
data3$storyEnableComfort[data3$storyEnableComfort=="neutral"] <- 3
data3$storyEnableComfort[data3$storyEnableComfort=="disagree"] <- 2
data3$storyEnableComfort[data3$storyEnableComfort=="strongly disagree"] <- 1

comfort.combined <- cbind(storyEnableComfort, storyUninstallComfort)
data3$comfort.combined <- comfort.combined

vars <- c("id",
          "storyEnableComfort",
          "storyUninstallComfort","randomint")
data3.subset <- data3[vars]

meltdata3 <- melt(data3.subset, id=c("id",
                                     "randomint"
))

write.csv(meltdata3, file = "meltdata3.csv")

#meltdata3$value[meltdata3$value==""] <-NA
#meltdata3 <- na.omit(meltdata3)
with(meltdata3, table(meltdata3$value,meltdata3$variable))
tab = table(meltdata3$variable, meltdata3$value, exclude = "") 
tab
tab = table(meltdata3$value,meltdata3$variable, exclude = "") 
tab
x<-chisq.test(tab)
x$expected
x
y<-fisher.test(tab)
y$expected
y


a=c(1,6,14,23,8)
b=c(2,0,1,8,24)

y1<-fisher.test(a,b,,alternative = "greater",hybrid = TRUE)
y1

y1<-fisher.test(tab,alternative = "less",hybrid = TRUE)
y1


y5<-fisher.test(tab,alternative = "greater",hybrid = TRUE)
y5
#for fishers test:
#http://in-silico.net/tools/statistics/fisher_exact_test

#for chi square-how to report it:
#http://www.stat.purdue.edu/~tqin/system101/method/method_fisher_sas.htm
#https://www.spss-tutorials.com/spss-chi-square-independence-test/
  
# You get a warning meessage because there are not 
# at least 5 obs in each cell of the table 

#Try recoding #combine the agree to one, the disagree to one and we end up with 3 instead of 5
meltdata3$value[meltdata3$value==1] <- 2
#meltdata3$value[meltdata3$value==3] <- 2

meltdata3$value[meltdata3$value==4] <- 5
write.csv(meltdata3, file = "meltdata3.csv")


tab2 = table(meltdata3$variable, meltdata3$value, exclude = c("",1,4)) 
#tab2 = table(meltdata3$variable, meltdata3$value, exclude = 1) 
#tab2 = table(meltdata3$value,meltdata3$variable, exclude = c("",1,4)) 
tab2

#tab2 = tab2(meltdata3$variable, meltdata3$value, exclude = "3") 
#tab2
x2<-chisq.test(tab2)
x2$expected
x2

#CrossTable(tab2,fisher=T, chisq=T, expected = T,prob.c=F, prob.t=F,prob.chisq=F, sresid=T, format = "SPSS" )
CrossTable(tab2,fisher=T, chisq=T, expected = T, sresid=T, format = "SPSS" )
odds
y2<-fisher.test(tab2)
y3<-fisher.test(tab2,alternative = "greater")
y3
y2$expected
y2
y2$alternative

#tab2.exclude("3")
#chisq.test(tab2) 

#meltdata4: 

vars <- c("variable",
          "value")
meltdata4 <- meltdata3[vars]

# You still get the error message because combining categories 
# still does not get at least 5 obs in each cell. 
# Your data prevents this requirement from happening. 
# Perhaps put in a footnote in the paper to explain 
# this limiitation when presenting results. 







# Part 2: Using the background data 


#remove the ones with fewer than 5 permissions requested (in python) and read the new file
# Do users tend to allow specific permissions to apps more than other?
data4 = read.csv("permissionDanger_out2.csv", header = T)
names(data4) 

# recode names to shorten name to help read output 

#colnames(data4)[colnames(data4)=="fraction.of.apps.granted.microphone"] <- "mic"
#colnames(data4)[colnames(data4)=="fraction.of.apps.granted.location"] <- "loc"
#colnames(data4)[colnames(data4)=="fraction.of.apps.granted.camera"] <- "cam"
#colnames(data4)[colnames(data4)=="fraction.of.apps.granted.contacts"] <- "con"
#colnames(data4)[colnames(data4)=="fraction.of.apps.granted.phone"] <- "phn"
#colnames(data4)[colnames(data4)=="fraction.of.apps.granted.storage"] <- "stor"
#colnames(data4)[colnames(data4)=="fraction.of.apps.granted.calendar"] <- "cal"
#colnames(data4)[colnames(data4)=="fraction.of.apps.granted.sensor"] <- "sen"
#colnames(data4)[colnames(data4)=="fraction.of.apps.granted.sms"] <- "sms"

#reorder them from lowest to highest
colnames(data4)[colnames(data4)=="fraction.of.apps.granted.contacts"] <- "Contacts"
colnames(data4)[colnames(data4)=="fraction.of.apps.granted.camera"] <- "Camera"
colnames(data4)[colnames(data4)=="fraction.of.apps.granted.microphone"] <- "Mic"
colnames(data4)[colnames(data4)=="fraction.of.apps.granted.phone"] <- "Phone"
colnames(data4)[colnames(data4)=="fraction.of.apps.granted.calendar"] <- "Calendar"
colnames(data4)[colnames(data4)=="fraction.of.apps.granted.location"] <- "Location"
colnames(data4)[colnames(data4)=="fraction.of.apps.granted.storage"] <- "Storage"
colnames(data4)[colnames(data4)=="fraction.of.apps.granted.sms"] <- "SMS"
newdata4<- melt(data4, id=c("randomint",
                             "danger.level.of.microphone",
                             "danger.level.of.location",
                             "danger.level.of.camera",
                             "danger.level.of.contacts",  
                             "danger.level.of.phone",    
                             "danger.level.of.storage",   
                             "danger.level.of.calendar", 
                             "danger.level.of.sms"))
#newdata4<- melt(data4, id=c("randomint"))
attach(newdata4)

leveneTest(value ~ as.factor(variable)) #Test of Homogeneity of Variances
kruskal.test(value~variable) # Kruskal-Wallis rank sum test
pairwise.wilcox.test(value,variable,p.adjust.method="bonf") # Multiple comparisons, bonferroni 

plotmeans(as.numeric(value) ~ variable,xlab="Permission",
          ylab="Mean of fraction of times it was granted", main="Mean Plot with 95% CI", data = newdata4,
          n.label = F,connect=FALSE,y.ticks =5) 
#grid(NA, 5, lwd = 2)
#angleAxis(1, at=p, labels = c("a","b"), srt=-45, adj=0)
#labels=c("a","b")
#text(p, labels = labels, srt = 45)
#axis(2)

#do it using repeated measures anova
#https://www.gribblelab.org/stats/notes/RepeatedMeasuresANOVA.pdf
#between subjects: aov(dv ~ treatment, data=mydata)
#within subjects: aov(dv ~ treatment + Error(subject/treatment), data=mydata)

#between subjects
#summary(aov(value~variable)) 

#This notation tells R to slice out of the model an additional error term corresponding to the variance accounted for by subjects. 
#The subjects/treatment notation tells R that the treatment factor is the repeated-measures factor over which subjects applies
#within subjects (repeated measures anova):
#So now, the effect of variable (permission) is significant at p = <0.001
summary(aov(value~variable + Error(randomint/variable), data=newdata4))
pairwise.t.test(value,variable,p.adjust.method="bonf",paired=TRUE) # Multiple comparisons, bonferroni 


#spherity is violated, so use MANOVA


#check spherity assumption
#my.matrix<- with(newdata4,cbind(value[variable=="mic"],value[variable=="loc"],value[variable=="cam"],value[variable=="phn"],value[variable=="stor"],value[variable=="con"],value[variable=="sms"],value[variable=="cal"]))
#my.matrix
#model <- lm(my.matrix ~ 1)
#design <- factor(c("mic", "loc", "cam", "phn","stor","con","sms","cal"))
#design
#model
#options(contrasts=c("contr.sum", "contr.poly"))
#results <- Anova(model, idata=data.frame(design), idesign=~design, type="III")
#summary(results, multivariate=F)
#http://homepages.gold.ac.uk/aphome/spheric.html






# What is the relationship between usage time and % enabled permissions? 
data5 = read.csv("Background_UsageTimeData.csv", header = T)
names(data5)

attach(data5)
data5$per.granted <- (number.of.permissions.granted / number.of.permissions.requested)

# Filter out zeros in granted apps (0 requested) 
data5$number.of.permissions.requested[data5$number.of.permissions.requested==0] <- NA

# Create caregorical variable 
data5$per.granted.cat[data5$per.granted==0] <- 0
data5$per.granted.cat[data5$per.granted>0 & data5$per.granted<1] <- 1
data5$per.granted.cat[data5$per.granted==1] <- 2

vars <- c("usagetime", "per.granted", "per.granted.cat",
          "number.of.permissions.requested", "randomint")
data5.subset <- na.omit(data5[vars])

attach(data5.subset)
qqnorm(usagetime) # Is the data normally distributed?
qqnorm(log(usagetime+.1)) # Is the data normally distributed?

hist(per.granted)
qqnorm(per.granted) # Is the data normally distributed?
qqnorm(log(per.granted+.1)) # Is the data normally distributed?


hist(per.granted.cat) # Categorical variable makes more sense



#this formula is for Linear Mixed Effect analysis, with requested permissions as the fixed effect and subject as random effect to take into account the within subjects design
summary(lme(log(usagetime+.1) ~ as.factor(per.granted.cat) + number.of.permissions.requested,
            random=~1|randomint, data = data5.subset))


anova(lme(log(usagetime+.1) ~ as.factor(per.granted.cat) + number.of.permissions.requested,
          random=~1|randomint, data = data5.subset))
#plot(jitter(log(usagetime+.1))~jitter(data5.subset$per.granted.cat))

# Does the app cateogry influence permissions? 
data6 = read.csv("Background_PermissionsWithAllInfo.csv", header = T,stringsAsFactors = F)
names(data6) 

vars <- c("randomint", "category", "numberOfPermissionsGranted", "numberOfPermissionsRequested")

data6.subset <- subset(data6, category=="Books and reference" |
                         category=="Business" |            
                         category=="Communication" |     
                         category=="Entertainment" |      
                         category=="Finance" |             
                         category=="Food & drink" |        
                         category=="games" |               
                         category=="Games" |              
                         category=="Health & Fitness" |    
                         category=="Maps & Navigation" |   
                         category=="Music & Audio" |      
                         category=="productivity" |        
                         category=="Productivity" |        
                         category=="Shopping" |            
                         category=="Social" |             
                         category=="Tools" |               
                         category=="Travel & Local", select=vars) 

attach(data6.subset)

data6.subset$per.granted <- (numberOfPermissionsGranted / numberOfPermissionsRequested)

# Filter out zeros in granted apps (0 requested) 
data6$numberOfPermissionsRequested[data6$numberOfPermissionsRequested==0] <- NA

data6.subset <- na.omit(data6.subset)

hist(per.granted)
qqnorm(per.granted) # is it normally distributed 
qqnorm(log(per.granted+.1)) # is it normally distributed 


# Create caregorical variable 
data6.subset$per.granted.binary[data6.subset$per.granted==0] <- 0
data6.subset$per.granted.binary[data6.subset$per.granted>0] <- 1
hist(data6.subset$per.granted.binary) # Categorical variable makes more sense


summary(glmer(per.granted.binary ~ as.factor(category) + numberOfPermissionsRequested + (1 | randomint), 
              data = data6.subset, family = binomial))

summary(lme(per.granted.binary ~ as.factor(category) + numberOfPermissionsRequested + (1 | randomint), 
              data = data6.subset))

# Model failed to converge, so results are unstable 






# What's the relationship between perceived danger and allowed permissions?
data7 = read.csv("permissionDanger_out.csv", header = T)
names(data7)


table(data7$danger.level.of.microphone)
#Try recoding 
data7$danger.level.of.microphone[data7$danger.level.of.microphone==1] <- 2 



table(data7$danger.level.of.location)
table(data7$danger.level.of.camera)
table(data7$danger.level.of.contacts)
table(data7$danger.level.of.phone)
table(data7$danger.level.of.storage)
table(data7$danger.level.of.calendar)
table(data7$danger.level.of.sensor)
table(data7$danger.level.of.sms)

data7$danger.level.of.microphone[data7$danger.level.of.microphone==0] <- NA
table(data7$danger.level.of.microphone)

data7$danger.level.of.calendar[data7$danger.level.of.calendar==0] <- NA
table(data7$danger.level.of.calendar)


# Correlations 
# (first result is the correlaton matrix, second matrix is the pvalue for the correlation)

attach(data7)
rcorr(fraction.of.apps.granted.microphone, danger.level.of.microphone, type="spearman") 
rcorr(fraction.of.apps.granted.location, danger.level.of.location, type="spearman") 
rcorr(fraction.of.apps.granted.camera, danger.level.of.camera, type="spearman") 
rcorr(fraction.of.apps.granted.contacts, danger.level.of.contacts, type="spearman") 
rcorr(fraction.of.apps.granted.phone, danger.level.of.phone, type="spearman") 
rcorr(fraction.of.apps.granted.storage, danger.level.of.storage, type="spearman") 
rcorr(fraction.of.apps.granted.calendar, danger.level.of.calendar, type="spearman")
rcorr(fraction.of.apps.granted.sensor, danger.level.of.sensor, type="spearman") 
rcorr(fraction.of.apps.granted.sms, danger.level.of.sms, type="spearman")


# ANVOA with multiple comparisons 

#Test of Homogeneity of Variances
leveneTest(fraction.of.apps.granted.microphone ~ as.factor(data7$danger.level.of.microphone)) 
summary(aov(fraction.of.apps.granted.microphone ~ data7$danger.level.of.microphone)) 
pairwise.t.test(data7$fraction.of.apps.granted.microphone, data7$danger.level.of.microphone, p.adj = "bonf")  

leveneTest(fraction.of.apps.granted.location ~ as.factor(danger.level.of.location)) 
summary(aov(fraction.of.apps.granted.location ~ danger.level.of.location))
pairwise.t.test(fraction.of.apps.granted.location, danger.level.of.location, p.adj = "bonf")

leveneTest(fraction.of.apps.granted.camera ~ as.factor(danger.level.of.camera)) 
summary(aov(fraction.of.apps.granted.camera ~ danger.level.of.camera))
pairwise.t.test(fraction.of.apps.granted.camera, danger.level.of.camera, p.adj = "bonf")

leveneTest(fraction.of.apps.granted.contacts ~ as.factor(danger.level.of.contacts)) 
summary(aov(fraction.of.apps.granted.contacts ~ danger.level.of.contacts))
pairwise.t.test(fraction.of.apps.granted.contacts, danger.level.of.contacts, p.adj = "bonf")

leveneTest(fraction.of.apps.granted.phone ~ as.factor(danger.level.of.phone)) 
summary(aov(fraction.of.apps.granted.phone ~ danger.level.of.phone))
pairwise.t.test(fraction.of.apps.granted.phone, danger.level.of.phone, p.adj = "bonf")

leveneTest(fraction.of.apps.granted.storage ~ as.factor(danger.level.of.storage)) 
summary(aov(fraction.of.apps.granted.storage ~ danger.level.of.storage))
pairwise.t.test(fraction.of.apps.granted.storage, danger.level.of.storage, p.adj = "bonf")

leveneTest(fraction.of.apps.granted.calendar ~ as.factor(data7$danger.level.of.calendar)) 
summary(aov(fraction.of.apps.granted.calendar ~data7$danger.level.of.calendar))
pairwise.t.test(fraction.of.apps.granted.calendar, data7$danger.level.of.calendar, p.adj = "bonf")

leveneTest(fraction.of.apps.granted.sensor ~ as.factor(danger.level.of.sensor)) 
summary(aov(fraction.of.apps.granted.sensor ~ danger.level.of.sensor))
pairwise.t.test(fraction.of.apps.granted.sensor, danger.level.of.sensor, p.adj = "bonf")

leveneTest(fraction.of.apps.granted.sms ~ as.factor(danger.level.of.sms)) 
summary(aov(fraction.of.apps.granted.sms ~ danger.level.of.sms))
pairwise.t.test(fraction.of.apps.granted.sms, danger.level.of.sms, p.adj = "bonf")



