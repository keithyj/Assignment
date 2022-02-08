# Initial Analysis

load("cancer.rdata")
summary(cancer)
head(cancer)
dim(cancer)
library(VIM)
pbox(cancer, pos = 10)
mosaicMiss(cancer)
for (i in 2:18){ 
  pbox(cancer,pos=i) 
}  
pbox(cancer, pos = 10)
pbox(cancer, pos = 4)
par(mfrow = c(1,1))
pbox(cancer, pos = 4)
pbox(cancer, pos = 1)
barMiss(cancer, pos = 4)
marginmatrix(cancer[,1:9])
par(mfrow = c(3,6))
for (i in 2:18){ 
  pbox(cancer,pos=i) 
} 
#Probably MCAR, in that case we just ditch the missing values
pairs(cancer[,-c(1,4)])
library(GGally)
ggpairs(na.omit(cancer), columns = c(2:3, 5:18), mapping = aes(alpha = 0.6),
        upper = list(continuous=wrap("cor", size=2.5, digits=2, stars=FALSE)),
        lower = list(continuous=mod_points),
        diag = list(continuous=modified_density),
        progress = FALSE) +
  theme(text=element_text(size=6))


mod_points=function(data,mapping,...) {
  ggally_points(data, mapping,pch=20, ...) +
    theme(text = element_text(size=8))
} 

modified_density = function(data, mapping, ...) {
  ggally_densityDiag(data, mapping, ...) +
    theme(text = element_text(size=5))
}
boxplot(cancer$incidenceRate)
par(mfrow = c(1,1))
strange <- filter(cancer, AvgHouseholdSize < 0.5)
summary(strange)
dim(strange)
ggpairs(na.omit(strange), columns = c(2:4, 5:18), mapping = aes(alpha = 0.6),
        upper = list(continuous=wrap("cor", size=2.5, digits=2, stars=FALSE)),
        lower = list(continuous=mod_points),
        diag = list(continuous=modified_density),
        progress = FALSE) +
  theme(text=element_text(size=6))
normal <- filter(cancer, AvgHouseholdSize > 0.5)
boxplot(cancer$binnedInc)
#Look more into how to deal with dummy variables
plot(cancer$binnedInc, cancer$povertyPercent)

d <- melt(na.omit(cancer)[,-c(4)])
nCancer <- na.omit(cancer)

hist(as.matrix(na.omit(cancer)[,5]))
p<-cancer[,5]
colnames(nCancer)

par(mfrow = c(4,4))
for(i in c(2:3, 5:18)){
  hist(as.matrix(nCancer[,i]), main = colnames(nCancer)[i])
}

for(i in c(2:3, 5:17)){
  plot(nCancer$deathRate~as.matrix(nCancer[,i]), main = colnames(nCancer)[i], xlab = colnames(nCancer[i]), ylab = "Death Rate")
}
summary(nCancer)
cor(nCancer$incidenceRate, nCancer$deathRate)
#0.4287871
cor(nCancer$povertyPercent, nCancer$deathRate)
#0.4250833
#Looking at this set of plots we have that incidence rate positively correlates, 
#Highest correlation to death rate is incdience rate, median income and povertypercent.
#Incidence rate seems to be linearly related with death rate. Maybe not for median income and poverty percent though
par(mfrow = c(1,3))
plot(lm(deathRate~incidenceRate, data = nCancer), 1)
plot(lm(deathRate~medIncome, data = nCancer), 1)
plot(lm(deathRate~povertyPercent, data = nCancer), 1)
#Looking at residual plots our data does seem to be linearly related with the dependent variable.
#A big problem we have though is heteroscedasticity.
spreadLevelPlot(lm(deathRate~incidenceRate, data = nCancer))
spreadLevelPlot(lm(deathRate~medIncome, data = nCancer))
spreadLevelPlot(lm(deathRate~povertyPercent, data = nCancer))
nCancer$TransPov <- 1/((nCancer$medIncome)^(1/3))
plot(lm(deathRate~TransPov, data = nCancer), 1)
spreadLevelPlot(lm(deathRate~TransPov, data = nCancer))

nCancer$TransDeath <- 1/((nCancer$deathRate)^(1/3))
plot(lm(TransDeath~medIncome, data = nCancer), 1)
spreadLevelPlot(lm(TransDeath~medIncome, data = nCancer))
plot(log(deathRate)~medIncome, data = nCancer)
crPlots(lm(deathRate~povertyPercent, data = nCancer))

#Percent black has an incredibly right skewed distribution
#Median income and poverty percent are also left skewed

DensBlack <- ggplot(nCancer, aes(PctBlack)) + geom_density()
DensBlack
nCancer$blackfix <- log(nCancer$PctBlack)
DensBlackFix <- ggplot(nCancer, aes(blackfix)) + geom_density()
DensBlackFix
#This seems to have mostly fixed the skew problem

#Incidence rate has one large outlier
par(mfrow = c(1,1))
plot(lm(deathRate~incidenceRate, data = nCancer), 5)
(nCancer[275,])
#Big outlier for incidence rate is Williamsburg City
plot(deathRate~incidenceRate, data = nCancer)
identify(nCancer$incidenceRate, nCancer$deathRate, labels = nCancer$Geography)
nCancer$Geography[c(250,275)]
#Not actually sure how to deal with the outliers, possibly remove Williamsburg for now

#Look at ggally plot for correlated dependent variables

#Medianagemale and median agefemale are very highly correlated
#Percent employ private and percent public are very negatively correlated
#These could be measuring the same thing

house <- nCancer$AvgHouseholdSize
bad <- which(house < 0.5)
nCancer$Geography[bad]
#All of these points are unrealistic
cancer1 <- cancer
house1 <- cancer1$AvgHouseholdSize
bad1 <- which(house1 < 0.5)
cancer1$AvgHouseholdSize[bad1] <- NA
for (i in c(2:3, 5:18)){ 
  pbox(cancer,pos=i) 
}  
par(mfrow = c(4,4))
par(mfrow = c(1,1))
pbox(cancer1, pos = 18)
par(mfrow = c(2,4))
for (i in c(2:3, 5:10)){ 
  pbox(cancer1,pos=i) 
}
#If these are indeed missing values (use data from other websites to support) then they are almost certainly mcar.

#Reccommendations would be culling the weird values in household size, omitting all rows with missing values, transforming the heteroscedastic variables
summary(cancer)
table(cancer$binnedInc)
cancer$Geography
uwu <- str_split(cancer$Geography[275], pattern = ", ")
uwu1 <- rep(0, )
uwu2 <- str_split(cancer$Geography, pattern = ", ")
cancer2 <- cancer
cancer2$state <- rep(0, length(cancer2$Geography))
uwu2 <- str_split(cancer3$Geography, pattern = ", ")
for(i in 1:length(cancer$Geography)){
  cancer2$state[i] <- uwu2[[i]][2]
}

cancer2$county <- rep(0, length(cancer2$Geography))
for(i in 1:length(cancer$Geography)){
  cancer2$county[i] <- uwu2[[i]][1]
}
cancer2$fcode <- rep("0", length(cancer2$Geography))
for(i in 1:length(cancer2$Geography)){
  cancer2$fcode[i] <- fips(cancer2$state[i], cancer2$county[i])
}
fips("New Mexico", "Dona Ana County")
cancer2$county["Doña Ana County"]
#167
cancer2$county[167] <- "Dona Ana County"
fips("Louisiana", "La Salle Parish")
cancer2$county["LaSalle Parish"]
#821
cancer2$county[821] <- "La Salle Parish"

plot_usmap(data = deathMapF,regions = "counties", values = "deathRate", include = newCancer$fips, color = "red") +
  scale_fill_continuous(low = "yellow", high = "red", name = "Death Rate", label = scales::comma) +
  labs(title = "Death rates in the states") +
  theme(legend.position = "right")
data(countypov)

?subset
colnames(cancer2)[21] <- "fips"
deathMap <- subset(cancer2, select = c("fips", "county", "deathRate"))
privateMap <- subset(cancer2, select = c("fips", "county", "PctPrivateCoverage"))
plot_usmap(data = privateMap,regions = "counties", values = "PctPrivateCoverage", include = privateMap$fips, color = "red") +
  scale_fill_continuous(low = "yellow", high = "red", name = "Percent Coverage", label = scales::comma) +
  labs(title = "Private Healthcare Percentage in the states") +
  theme(legend.position = "right")
incidentMap <- subset(cancer2, select = c("fips", "county", "incidenceRate"))
plot_usmap(data = incidentMapF,regions = "counties", values = "incidenceRate", include = incidentMap$fips, color = "red") +
  scale_fill_continuous(low = "yellow", high = "red", name = "Percent Coverage", label = scales::comma) +
  labs(title = "Private Healthcare Percentage in the states") +
  theme(legend.position = "right")
plot(cancer2$incidenceRate, cancer2$deathRate)
identify(cancer2$incidenceRate, cancer2$deathRate, labels = cancer2$county)
incidentMapF <- incidentMap[-c(256,282,1490),]
deathMapF <- deathMap[-c(256,282,1490),]
