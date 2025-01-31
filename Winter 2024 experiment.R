
setwd("/Users/irisflores/Desktop/STATIC EXPERIMENT/static stats")

# install.packages("dplyr")
library(tidyverse)
# library(readxl)
# library(sciplot)
# library(ggmap)
# library(mapdata)
# library(readr)
# # install.packages("Rmisc")
#library(Rmisc)
# # install.packages("lme4")
#rm(list = ("D1")) 

m=read.csv("grazing.csv")
str(m)


#Create Grazing rate columns#####
m2<-m%>% #new column for grazing rate 
  mutate(grazing_rate=Kelp_st_weight-Kelp_end_weight)
m2$grazing_rate<- ifelse(m2$grazing_rate< 0, 0,  m2$grazing_rate) #makes all negative grazing rates zero


m3<-m2 %>% #new column for grazing by size
  mutate(grazing_by_size=(grazing_rate)/diameter)

m4<-m3%>% #new column for grazing by buoyant weight
  mutate(grazing_by_buoywt=(grazing_rate)/buoyant.weight)

m4<-m4%>% #column grazing by wet weight
  mutate(grazing_by_ww=(grazing_rate)/wet.weight)

m4<-m4[-c(668,924,992,995,997),] #row delete 668 and 924 urchins k68 did not eat/died remove 3 that ate all food from 1B K5 K74 K29

m4<-m4 %>% # CALCULATE HOURLY GRAZING RATE 
  mutate(HourlyGrazingBuoyWT=case_when(
    Trial==1~grazing_by_buoywt/24,
    Trial==2~grazing_by_buoywt/24,
    Trial==3~grazing_by_buoywt/21,
    Trial==4~grazing_by_buoywt/21,
    TRUE~NA_real_
  ))
 
m4<-m4 %>% # CALCULATE HOURLY GRAZING RATE 
  mutate(HourlyGrazingSZ=case_when(
    Trial==1~grazing_by_size/24,
    Trial==2~grazing_by_size/24,
    Trial==3~grazing_by_size/21,
    Trial==4~grazing_by_size/21,
    TRUE~NA_real_
  ))   
    
m4<-m4 %>% # CALCULATE HOURLY GRAZING RATE 
  mutate(HourlyGrazing=case_when(
    Trial==1~grazing_rate/24,
    Trial==2~grazing_rate/24,
    Trial==3~grazing_rate/21,
    Trial==4~grazing_rate/21,
    TRUE~NA_real_
  ))

m4<-m4 %>% # CALCULATE HOURLY GRAZING RATE by wet weight
  mutate(HourlyGrazingWW=case_when(
    Trial==1~grazing_by_ww/24,
    Trial==2~grazing_by_ww/24,
    Trial==3~grazing_by_ww/21,
    Trial==4~grazing_by_ww/21,
    TRUE~NA_real_
  ))

m4 <- m4 %>% # create day column
  mutate(Day = case_when(
    Trial == 1 ~ 4,
    Trial == 2 ~ 7,
    Trial == 3 ~ 10,
    Trial == 4 ~ 13,
    TRUE ~ NA_real_  # This will set Day to NA for any other TRIAL values
  ))

#test for normality
shapiro.test(m4$HourlyGrazingBuoyWT)
library(ggpubr)
ggqqplot(m4$HourlyGrazingBuoyWT)

#Prelim figures without PCA####

m4%>% #plot with grazing rate on y and  Treatment on x
  ggplot(mapping = aes(x=Treatment, y=grazing_rate, color=habitat))+ geom_point()+facet_grid(.~Trial)

m4%>% #plot with  grazing by size on y and Temp on x-axis
  ggplot(mapping = aes(x=Treatment,y=grazing_by_size ,color=habitat))+ geom_point()

m4%>% #plot with grazing rate by size on y and treatment on x-axis
  ggplot(mapping = aes(x=Treatment, y=grazing_by_size, color=habitat))+ geom_point()+facet_grid(.~Trial)

m4%>% #plot with  grazing by wet weight on y and temp on x-axis
  ggplot(mapping = aes(x=Treatment,y=grazing_by_ww ,color=habitat))+ geom_point()+facet_grid(.~Trial)

#Group by std error####

m5<-m4 %>% #grazing rate only
  group_by(Header,Treatment,habitat)%>%
  summarize(MeanGrazing=mean(HourlyGrazing),stdGrazing=sd(HourlyGrazing))

m6<-m4 %>% #mean grazing by WET weight and by header bucket,treatment,habitat
  group_by(Header, Treatment, habitat, Trial,tank,Day)%>%
  summarize(MeanGrazingwt=mean(HourlyGrazingWW),stdGrazingwt=sd(HourlyGrazingWW))

m7<-m4%>% #grazing by size
  group_by(Header, Treatment, habitat)%>%
  summarize(MeanGrazingsize=mean(HourlyGrazingSZ),stdGrazingsize=sd(HourlyGrazingSZ))

m15<-m4%>%
  group_by(Trial, habitat)%>%
  summarize(MeanGrazing=mean(HourlyGrazingWW),std=sd(HourlyGrazingWW))

m16<-m4%>%
  group_by(Trial, habitat, Treatment)%>%
  summarize(MeanGrazing=mean(HourlyGrazingWW),std=sd(HourlyGrazingWW))
 #PLOTS##### 
  m15%>%
  ggplot(mapping = aes(x=Trial,y=MeanGrazing,fill=habitat))+ geom_bar(stat="identity", position = position_dodge(width = 0.85))+ ylab(bquote('Average grazing rate (g kelp wet wt'~hr^-1*')')) + scale_fill_manual(values = c("indianred2", "palegreen4"))


m15 %>%
  ggplot(mapping = aes(x = Trial, y = MeanGrazing, color = habitat)) + 
  geom_point(position = position_dodge(width = 0.5)) +  # Adds side-by-side points
  geom_errorbar(aes(ymin = MeanGrazing - std, ymax = MeanGrazing + std), 
                position = position_dodge(width = 0.5), width = 0.2) +  # Side-by-side error bars
  labs(y = "Average grazing rate (g kelp wet wt'~hr^-1*')") + 
  labs(colour = "Habitat") + 
  scale_color_manual(labels = c("Barren", "Kelp"), values = c("indianred", "palegreen4"))

  m16%>%
    ggplot(mapping = aes(x=Treatment,y=MeanGrazing,fill=habitat))+ geom_bar(stat="identity", position = position_dodge(width = 0.85))+ ylab(bquote('Average Grazing rate (g kelp wet wt'~hr^-1*')')) +facet_grid(.~Trial)

  
#  m4%>% #grazing rate PLOT by weight!
  ggplot(mapping = aes(x=Trial,y=HourlyGrazingWT, color= habitat))+ geom_bar(stat="identity", position = position_dodge(width = 0.85)) 

#m6%>% #grazing rate PLOT by weight!
  ggplot(mapping = aes(x=Trial,y=MeanGrazingwt, color= habitat))+ geom_bar(stat="identity", position = position_dodge (width = 0.85)) 

#m3%>% #grazing rate PLOT
  ggplot(mapping = aes(x=Treatment,y=MeanGrazing,color=Header))+ geom_point()+
  geom_errorbar(aes(ymin = MeanGrazing - stdGrazing, ymax = MeanGrazing + stdGrazing))+
  theme(axis.text.x = element_text(angle = 90)) + facet_grid(.~habitat) #rotates labels on x-xis

#m6%>% #BY weight
  ggplot(mapping = aes(x=Trial,y=MeanGrazingwt,fill=Treatment))+ geom_bar(stat="identity", position = position_dodge(width = 0.85))+ ylab(bquote('Average Grazing rate (g kelp wet wt'~hr^-1*')'))+facet_grid(.~habitat)


#Daily PH and TEMP over experiment####
X<-read.csv("COMBINED.csv") %>%  #not corrected
  mutate(DAY = as.character(DAY))
X$DT <- as.POSIXct(X$DateTime, format = "%m/%d/%Y %H:%M:%S %p", tz = "UTC")

X$DAY %>% unique()

X1 <- X %>% as_tibble() %>% 
  # pivot_longer(cols = c(3:ncol(X)),
  # names_to = "Measurement",
  #              values_to = "Value") %>%
  # group_by(DAY, Measurement)%>%
  # summarize(mean = mean(Value, na.rm = TRUE))
  group_by(DAY,) %>% 
  summarise(pH1 = mean(H1,na.rm=TRUE),
            pH2 = mean(H2,na.rm=TRUE),
            pH3=mean(H3,na.rm=TRUE), 
            pH4=mean(H4,na.rm=TRUE),
            pH5=mean(H5,na.rm=TRUE),
            pH6=mean(H6,na.rm=TRUE),
            pH8=mean(H8,na.rm=TRUE),
            pH9=mean(H9,na.rm=TRUE),
            pH10=mean(H10,na.rm=TRUE),
            pH11=mean(H11,na.rm=TRUE),
            pHSump=mean(Sump,na.rm=TRUE),
            T1=mean(T1,na.rm=TRUE),
            T2=mean(T2,na.rm=TRUE),
            T3=mean(T3,na.rm=TRUE),
            T4=mean(T4,na.rm=TRUE),
            T5=mean(T5,na.rm=TRUE),
            T6=mean(T6,na.rm=TRUE),
            T8=mean(T8,na.rm=TRUE),
            T9=mean(T9,na.rm=TRUE),
            T10=mean(T10,na.rm=TRUE),
            T11=mean(T11,na.rm=TRUE),
            TSUMP=mean(Tsump,na.rm=TRUE))

X1
X1<-X1[,-c(12,23)] #get rid of sump ph and temp
  
X2<-X1%>% #ADD COLUMNS WITH corrected pH with spec offset
  mutate(ph1corr =pH1-0.022014, ph2corr=pH2+0.012213,ph3corr=pH3-0.009951,ph4corr=pH4-0.006242,ph5corr=pH5-0.062175,ph6corr=pH6-0.032637,ph8corr=pH8-0.010323,ph9corr=pH9-0.007322,ph10corr=pH10+0.014162,ph11corr=pH11-0.031334)

X3<- X2 %>%
  summarise(across(where(is.numeric), 
                         list(mean = ~mean(., na.rm = TRUE), 
                              sd = ~sd(., na.rm = TRUE))))

#Daily DO DATA####
d2<-read.csv("COMBINED_DO.csv")
str(d2) #view structure

##didn't have to do this because I created a date column. If you dont, you could create a vector where the first 20 rows are labeled "A" and 95 rows after labeled "B", LAST 38 ROWS ... etc 

#DC<-c(rep("A",20),rep("B",95), rep("C",96),rep("D",95),rep("E",94),rep("F",95),rep("G",95),rep("H",95),rep("I",96),rep("J",95),rep("K",95),rep("L",49)) 
#DC reads it out

#d2$day<-DC CREATES A COLUMN "DAY" WITH A,B,C

#get daily average D0 by date and column
daily_averages <- d2 %>%
  group_by(Date) %>%
  summarise(across(c(HB1SAL, HB2SAL, HB3SAL, HB4SAL, HB5SAL, HB6SAL, HB8SAL, HB9SAL, HB10SAL, HB11SAL), mean, .names = "avg_{col}"))

D3<- d2 %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

#PCA####

Z<-read.csv("PHDOT1.csv") #file with corrected ph, vernier do and temp for 10 days

Z<-Z[-c(47:48),] #probe header 4 broke near the end so this removes those readings

#Broken Vernier probe= caluculate DO based on Line of best fit

r1 <- lm(Z$vernDO~ Z$phcorr) # plot do vs ph to get a slope
plot.new()
plot.window(xlim = range(Z$phcorr), ylim = range(Z$vernDO))
# Plot data points with labels and title
plot(Z$phcorr, Z$vernDO, pch = 19, main="Scatter Plot with Line of Best Fit",
     xlab="phcorr", ylab="vernDO")
# Add the line of best fit
abline(r1, col="blue")
# Extract coefficients for the line equation
intercept <- coef(r1)[1]
slope <- coef(r1)[2]
# Create the equation text
equation <- paste("vernDO =", round(intercept, 2), "+", round(slope, 2), "* phcorr")
# Add equation to the plot (adjust position as needed)
text(x = min(Z$phcorr), y = max(Z$vernDO), labels = equation, pos = 4, col = "blue")
#using line of best fit, calculated DO for hb6 which was broken and inserted those values here
indices <- c(59,60,61,62,63,64,65,66,67,68,69,70)
# Replacement values for each specified row

replacement_values <- c(4.25486848, 4.2617182, 4.25838874, 4.25705314, 4.2518443,  4.24911586, 4.24301026, 4.25459182, 4.24448896, 4.23761062, 4.2455479, 4.24404058)

Z$vernDO[indices] <- replacement_values

#PCA CONTINUED

Z1<-Z%>% group_by(Header)%>% #MEAN PH,DO,TEMP, SD PH,DO,TEMP
  summarise(MeanpH=mean(phcorr),stdpH=sd(phcorr),MeanDO=mean(vernDO),stdDO=sd(vernDO),MeanT=mean(temp),StdT=sd(temp))

PCA <- prcomp(Z1 [2:7], scale=TRUE) #Run PCA of pH, DO, tempDO, temppH
summary(PCA)
library(lme4)
library(factoextra)
library(wesanderson)
biplot(PCA)

BIP<-biplot(PCA,col= wes_palette("Zissou1",2)) #BIPLOT

fviz_eig(PCA, addlabels = TRUE, ylim = c(0, 100)) #screeplot

A<-fviz_pca_biplot(PCA, 
                repel = TRUE, 
                col.var = "red", 
                col.ind = "blue",
                title = "PCA Biplot")
A

Load <- data.frame(PCA$x) #Turn loadings into dataframe
TrtPCA <- data.frame(c(Z1, Load)) #combine PCA loadings to OG dataframe

m4$Trial <- as.factor(m4$Trial) #turn trial into category
str(m4)

m4 <- m4 %>%
  mutate(PC1 = case_when(
    Header == "H1" ~ TrtPCA$PC1[1],
    Header == "H2" ~ TrtPCA$PC1[4],
    Header == "H3" ~ TrtPCA$PC1[5],
    Header == "H4" ~ TrtPCA$PC1[6],
    Header == "H5" ~ TrtPCA$PC1[7],
    Header == "H6" ~ TrtPCA$PC1[8],
    Header == "H8" ~ TrtPCA$PC1[9],
    Header == "H9" ~ TrtPCA$PC1[10],
    Header == "H10" ~TrtPCA$PC1[2],
    Header == "H11" ~ TrtPCA$PC1[3],
    TRUE ~ NA_real_))

model<- lmer(HourlyGrazingWW ~ PC1* habitat + PC1*Trial+ Trial*habitat + Trial + (1|tank), data = m4)
summary(model)
hist(resid(model),breaks=20)
model<- glmer(HourlyGrazingWW ~ PC1* habitat + PC1*Trial+ Trial*habitat + Trial + (1|tank), 
             family=Gamma(link="log"),data = m4)
model2<- glmer(HourlyGrazingWW ~ PC1* habitat + PC1*Trial+ Trial*habitat + Trial + (1|tank), 
              family=Gamma(link="log"),data = m4[m4$HourlyGrazingWW>0,]);summary(model2)


B <- ggplot(m4, aes(x=PC1, y=HourlyGrazingWW, group = as.factor(Trial), 
                    color = as.factor(Trial), fill = as.factor(Trial))) +
  geom_point(alpha = 0.25) +
  stat_smooth(method = "lm" , formula = y~x, se = TRUE) +
  scale_color_brewer(name = "Trial", labels = c("1","2","3","4"), palette = "Dark2") +
  scale_fill_brewer(name = "Trial", labels = c("1","2","3","4"), palette = "Dark2") +
  ylab(bquote('Mass corrected grazing rate (g kelp wet wt'~hr^-1*')'))+ 
  xlab("PC1") +
  theme_classic() +
  theme(legend.position = c(0.85, 0.85)) 
#theme(text=element_text(size=12, family="Times New Roman"))
B <- B + scale_y_continuous(breaks = c(0.050,0.125,0.20))
B

m6 <- m6 %>% #using grazing weight by wet weight of urchins
  mutate(PC1 = case_when(
    Header == "H1" ~ TrtPCA$PC1[1],
    Header == "H2" ~ TrtPCA$PC1[4],
    Header == "H3" ~ TrtPCA$PC1[5],
    Header == "H4" ~ TrtPCA$PC1[6],
    Header == "H5" ~ TrtPCA$PC1[7],
    Header == "H6" ~ TrtPCA$PC1[8],
    Header == "H8" ~ TrtPCA$PC1[9],
    Header == "H9" ~ TrtPCA$PC1[10],
    Header == "H10" ~TrtPCA$PC1[2],
    Header == "H11" ~ TrtPCA$PC1[3],
    TRUE ~ NA_real_))

model2<- lmer(MeanGrazingwt ~ PC1* habitat + PC1* Trial+Trial*habitat+ Trial + (1|tank) , data = m6)
anova(model2)
summary(model2)

C <- ggplot(m6, aes(x=PC1, y=MeanGrazingwt, group = as.factor(Trial), 
                    color = as.factor(Trial), fill = as.factor(Trial))) +
  geom_point(alpha = 0.25) + 
  stat_smooth(method = "lm" , formula = y~x, se = TRUE) +
  scale_color_brewer(name = "Trial", labels = c("1","2","3","4"), palette = "Dark2") +
  scale_fill_brewer(name = "Trial", labels = c("1","2","3","4"), palette = "Dark2") +
  ylab(bquote('Grazing rate (g kelp wet wt'~hr^-1*')'))+ 
  xlab("PC1") +
  theme_classic() +
  theme(legend.position = c(0.85, 0.85)) 
#theme(text=element_text(size=12, family="Times New Roman"))
C <- C + scale_y_continuous(breaks = c(0.050,0.125,0.20))
C
#PCA WITH YSI DO ####
y<-read.csv("pHDOT.csv") 
summary(y)
str(y)

y2<-y[ ,-c(7:56)] # REMOVES COLUMNS
str(y2)

r2<-lm(y2$YSI.DO~y2$pHCORR)
plot.new()
plot.window(xlim = range(y2$pHCORR), ylim = range(y2$YSI.DO))
# Plot data points with labels and title
plot(y2$pHCORR, y2$YSI.DO, pch = 19, main="Scatter Plot with Line of Best Fit",
     xlab="phcorr", ylab="YSIDO")
# Add the line of best fit
abline(r2, col="blue")
# Extract coefficients for the line equation
interceptY <- coef(r2)[1]
slopeY <- coef(r2)[2]
# Create the equation text
equation <- paste("YSIDO =", round(interceptY, 2), "+", round(slopeY, 2), "* phcorr")
# Add equation to the plot (adjust position as needed)
text(x = min(y2$pHCORR), y = max(y2$YSI.DO), labels = equation, pos = 4, col = "blue")

y3<-y2 %>% #USING YSI D0 
  group_by(HEADER)%>%
  summarise(MeanpH=mean(pHCORR),stdpH=sd(pHCORR),MeanDO=mean(YSI.DO),stdDO=sd(YSI.DO),MeanT=mean(T),StdT=sd(T))

PCAen <- prcomp(y3 [2:7], scale=TRUE) #Run PCA of pH, DO, tempDO, temppH
summary(PCAen) #Look at the amount of variance explained
BIP<-biplot(PCAen,col= wes_palette("Zissou1",2))

screeplot(PCAen, type = "lines", main = "Scree Plot")
#SCREE PLOT
fviz_eig(PCAen, addlabels = TRUE, ylim = c(0, 100))

biplot(PCAen, main = "PCA Biplot")

fviz_pca_biplot(PCAen, 
                repel = TRUE, 
                col.var = "red", 
                col.ind = "blue",
                title = "PCA Biplot")

Loaden <- data.frame(PCAen$x) #Turn loadings into dataframe
TrtPCAen <- data.frame(c(y3, Loaden)) #Combine PCA loadings with OG data

 m10<- m4 %>%
  mutate(PC1 = case_when(
    Header == "H1" ~ TrtPCAen$PC1[1],
    Header == "H2" ~ TrtPCAen$PC1[4],
    Header == "H3" ~ TrtPCAen$PC1[5],
    Header == "H4" ~ TrtPCAen$PC1[6],
    Header == "H5" ~ TrtPCAen$PC1[7],
    Header == "H6" ~ TrtPCAen$PC1[8],
    Header == "H8" ~ TrtPCAen$PC1[9],
    Header == "H9" ~ TrtPCAen$PC1[10],
    Header == "H10" ~TrtPCAen$PC1[2],
    Header == "H11" ~ TrtPCAen$PC1[3],
    TRUE ~ NA_real_))

 
modelen<- lmer(HourlyGrazingWT ~ PC1* habitat + PC1* Trial+Trial*habitat+ Trial + (1|tank), data = m10)
anova(modelen)
summary(modelen)


D <- ggplot(m10, aes(x=PC1, y=HourlyGrazingWT, group = as.factor(Trial), 
                    color = as.factor(Trial), fill = as.factor(Trial))) +
  geom_point(alpha = 0.25) + 
  stat_smooth(method = "lm" , formula = y~x, se = TRUE) +
  scale_color_brewer(name = "Trial", labels = c("1","2","3","4"), palette = "Dark2") +
  scale_fill_brewer(name = "Trial", labels = c("1","2","3","4"), palette = "Dark2") +
  ylab(bquote('Mass corrected grazing rate (g kelp wet wt'~hr^-1*')'))+ 
  xlab("PC1") +
  theme_classic() +
  theme(legend.position = c(0.85, 0.85)) 
#theme(text=element_text(size=12, family="Times New Roman"))
D <- D + scale_y_continuous(breaks = c(0.050,0.125,0.20))
D

#####

D2%>% #correlation between pH and oxygen
  ggplot(mapping = aes(x=MeanpH,y=MeanVDO))+ geom_point()
D2%>% #correlation between YSI DO and pH
  ggplot(mapping = aes(x=MeanpH,y=MeanDO,))+ geom_point()
D2%>% #correlation between pH and Temp
  ggplot(mapping = aes(x=MeanpH,y=MeanT,))+ geom_point()
D2%>% #correlation between Temp and oxygen
  ggplot(mapping = aes(x=MeanT,y=MeanVDO))+ geom_point()

y2%>%
  ggplot(mapping = aes(x=pHCORR,y=DO))+ geom_point()
y2%>% #correlation between YSI DO and pH
  ggplot(mapping = aes(x=pHCORR,y=YSI.DO))+ geom_point() 
y2%>% #correlation between YSI DO and pH
  ggplot(mapping = aes(x=pHCORR,y=T))+ geom_point() 
y2%>% #correlation between YSI DO and pH
  ggplot(mapping = aes(x=T,y=YSI.DO))+ geom_point() 

#GONAD SOMATIC INDEX######
GSI<-read.csv("GSI.csv")

gsi2<-GSI%>%
  group_by(habitat)%>%
  summarise(meanGSI=mean(DW_Ratio),StdGSI=sd(DW_Ratio))

gsi2%>%
  ggplot(mapping = aes(x=habitat, y=meanGSI, color=habitat))+ geom_point()+
  geom_errorbar(aes(ymin = meanGSI- StdGSI, ymax = meanGSI + StdGSI))+ labs(y="Mean G:S",x="Habitat") + labs(colour="Habitat")+ scale_color_manual(labels = c("Barren", "Kelp"), values = c("indianred", "olivedrab4"))

gsi2%>% 
  ggplot(mapping = aes(x=habitat,y=meanGSI,fill=habitat))+ geom_bar(stat="identity", position = position_dodge(width = 0.85))+ labs(y="Mean G:S",x= "Habitat")

print(gsi2%>% 
        ggplot(mapping = aes(x=habitat,y=meanGSI,fill=habitat))+ geom_bar(stat="identity", position = position_dodge(width = 0.85)))
