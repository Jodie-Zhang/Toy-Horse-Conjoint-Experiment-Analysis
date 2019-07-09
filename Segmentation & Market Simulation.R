##For Designing Tasks to Present to Respondents
install.packages("AlgDesign")  #install package AlgDesign if not already installed
library(AlgDesign)#load the library AlgDesign

set.seed(123)
levels.design= c(2,2,2,2) #set the number of levels for each variable
f.design <- gen.factorial(levels.design,factors="all")
fract.design <- optFederov(frml=~X1+X2+X3+X4,data=f.design,nTrials=12,approximate=FALSE)
filenm = "MKT412R - Final Analysis Case Data"
load(paste(filenm,".Rdata",sep=""))


##Regression Analysis for getting part-worth estimates
ls()
atts = c("Low Price","Tall Size","Rocking","Glamour")
colnames(desmat) = atts
summary(lm(ratings~desmat))

##by apriori segment age
summary(lm(ratings~desmat*ageD))
summary(lm(ratings~desmat*genderD)); ##run the regression with interactions for segment dummies
##note if significant. can run separately for two categories
summary(lm(ratings~desmat,subset=genderD==1)) # girls
summary(lm(ratings~desmat,subset=genderD==0)) # boys

##by individual
desmatf = cbind(rep(1,nrow(desmat)),desmat); ##add column for constant
partworths = matrix(nrow=sampsize,ncol=ncol(desmatf))
for(i in 1:sampsize){ #for each individual run the regression
  partworths[i,]=lm(ratings~desmat,subset=ID==i)$coef
}
colnames(partworths) = c("Intercept",atts)
head(partworths)
partworths 

##segmenting individuals 
library(cluster)
library(fpc)
set.seed(123456)   

toclust = partworths;
pm1 = pamk(toclust,scaling=TRUE) 
pm1$nc 

wss <- (nrow(toclust)-1)*sum(apply(toclust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(toclust,
                                     centers=i,nstart=2)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

km3 = kmeans(toclust,3,iter.max = 20, nstart=2)


percsize = paste(1:2, "=",format(km2$size/sum(km2$size)*100,digits=2),"%",sep="")
pie(km3$size,labels=percsize)

clusplot(toclust, km4$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0) #plot clusters against principal components

plotcluster(toclust, km3$cluster) #plot against discriminant functions ()

plotClust = function(km,discPlot=FALSE){
  nc = length(km$size)
  if(discPlot){par(mfrow=c(2,2))}
  else {par(mfrow=c(3,1))}
  percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
  pie(km$size,labels=percsize,col=1:nc)
  
  if (nc > 2) {
    clusplot(toclust, km$cluster, color=TRUE, shade=TRUE,
             labels=2, lines=0,col.clus=nc:1); #plot clusters against principal components
  } else {
    clusplot(toclust, km$cluster, color=TRUE, shade=TRUE,
             labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components    
  }
  if(discPlot){
    plotcluster(toclust, km$cluster,col=km$cluster); #plot against discriminant functions ()
  }
  rng = range(km$centers)
  dist = rng[2]-rng[1]
  locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
  bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
  text(bm,locs,formatC(km$centers,format="f",digits=1))
}

plotClust(km3)
plotClust(km2)
plotClust(km4)


rscale = function(x){(x-min(x))/(max(x)-min(x));}
km3m = aggregate(toclust,by=list(km3$cluster),FUN=mean); # calculate profile means

# aggregate() splits data into subsets, computes summary stats for each, and returns the results
km3sd = aggregate(toclust,by=list(km3$cluster),FUN=sd); 

# visualization
km3ms = apply(km3m[,2:ncol(km3m)],2,rscale); 
par(mar=c(2.1,4.1,4.1,2.1)); #setting margins to give room for x axis labels
matplot(t(km3ms),col=c(1,4,2),ylab="Mean Value (Range Normalized)",xaxt="n")




############## part4: Creation of Fitted and Actual Ratings ############
##predicting missing cells (preparing for market simulation)
##repeat individual level partworths for multiplication
partworths.full = matrix(rep(partworths,each=16),ncol=5)
pratings = rowSums(desmatf*partworths.full)
finalratings = ifelse(is.na(ratings),pratings,ratings); #combining actual when available and predicted ratings


##market simulation
##a scenario is a set of products, each with a set of levels.
## create a vector with the indexes for the product profiles from the
##status quo
#Our competitor offers 7 ($139, 26", Rocking, Race Horse)
#We offer 13, ($139, 18", Rocking, Glamour Horse)
scen0 = c(7,13)
##Adding with product 1, a $139, 18", Roking, Race Horse
scen1 = c(7,13,5)
##What if our competitor responds by lowering price?
scen2 = c(8,13,5)

## low prices for 3 segmentation, competitor lower price
scen3 = c(8,4,14,16)
## competitor lower price, we price high for two segments
scen4 = c(8,4,13,15)
## competitor high price, we price high for two segments
scen5 = c(7,4,13,15)
## competitor high price, we price high for one segments
scen6 = c(7,4,14,15)
scen7 = c(7,4,15)
scen8 = c(7,13,5,15) #13 black cluster,5??, 15 green,
scen9 = c(8,13,5,15)


##market simulations
#tranform final ratings into matrix
simDecInput = matrix(finalratings,nrow=nprofiles); ##this has 16 rows for profiles and sampsize columns

##inputmat is the ratings matrix with rows as profiles and cols as ratings
##scen is the list of products in the market for the scenario (these are rows in inputmat)
simDec = function(inputmat,scen){
  inmkt = inputmat[scen,]
  max = apply(inmkt,2,max)
  firstChoices = (inmkt>rep(max,each=length(scen))-.000000000001)
  shares = firstChoices/rep(colSums(firstChoices),each=length(scen))
  rowMeans(shares)
}


simDec0 = simDec(simDecInput,scen0)
simDec1 = simDec(simDecInput,scen1)
simDec2 = simDec(simDecInput,scen2)
simDec3 = simDec(simDecInput,scen3)
simDec4 = simDec(simDecInput,scen4)
simDec5 = simDec(simDecInput,scen5)
simDec6 = simDec(simDecInput,scen6)
simDec7 = simDec(simDecInput,scen7)
simDec8 = simDec(simDecInput,scen8)
simDec9 = simDec(simDecInput,scen9)

##inputmat and scen is as above. myprods are indicators of which prods are the firms,
## prices are the prices for all products, vcosts are the variable costs
## fcosts are the fixed costs for the firm (need to calculate in already the number of products)
simProfit = function(inputmat,scen, myProds, prices, vcosts,fcosts,mktsize=1){
  mktshr = simDec(inputmat,scen);
  vprofit = mktshr * (prices-vcosts)*mktsize;
  sum(vprofit[myProds])-fcosts
}

simProf0 = simProfit(simDecInput,scen0,c(2),c(111.99,111.99),c(41,33),20000,4000)
simProf1 = simProfit(simDecInput,scen1,c(2,3),c(111.99,111.99,111.99),c(41,33,33),40000,4000)
simProf2 = simProfit(simDecInput,scen2,c(2,3),c(111.99,111.99,111.99),c(41,33,33),40000,4000)
simProf3 = simProfit(simDecInput,scen3,c(2,3,4),c(95.99,95.99,95.99,95.99),c(41,29,33,41),60000,4000)
simProf4 = simProfit(simDecInput,scen4,c(2,3,4),c(95.99,95.99,111.99,111.99),c(41,29,33,41),60000,4000)
simProf5 = simProfit(simDecInput,scen5,c(2,3,4),c(111.99,95.99,111.99,111.99),c(41,29,33,41),60000,4000)
simProf6 = simProfit(simDecInput,scen6,c(2,3,4),c(111.99,95.99,95.9,111.99),c(41,29,33,41),60000,4000)
simProf8 = simProfit(simDecInput,scen8,c(2,3,4),c(111.99,111.99,111.99,111.99),c(41,33,33,41),60000,4000)
simProf9 = simProfit(simDecInput,scen9,c(2,3,4),c(95.99,95.99,95.9,111.99),c(41,33,33,41),60000,4000)


##write a loop to get the market simulation result
##attach cost
profileData <- as.data.frame(desmat[1:16, ])
for (i in 1:16){
  if (profileData[i,2]==0&profileData[i,3]==1){
    profileData$cost[i]=33
  }else if (profileData[i,2]==1&profileData[i,3]==1){
    profileData$cost[i]=41
  }else if (profileData[i,2]==0&profileData[i,3]==0){
    profileData$cost[i]=21
  }else if (profileData[i,2]==1&profileData[i,3]==0){
    profileData$cost[i]=29
  }
}

##attach price
for (i in 1:16){
  if (profileData[i,1]==0){
    profileData$price[i]=139.99
  }else{
    profileData$price[i]=119.99
  }
}

##competitor lower the price, three products
prodCombinations <- matrix(data=rep(NA,4),nrow=1,ncol=4)
for (i in 1:16){
  for (j in 1:16){
    for (k in 1:16){
      mkshare=simDec(simDecInput,c(8,i,j,k))
      profit= mkshare[2]*(profileData[i,6]-profileData[i,5])*4000+mkshare[3]*(profileData[j,6]-profileData[j,5]*4000+mkshare[4]*(profileData[k,6]-profileData[k,5]*4000))-6000
      prodCombinations <- rbind(matrix(c(i,j,k,profit),nrow=1,ncol=4),prodCombinations)
    }
  }
}
colnames(prodCombinations) <- c("product 1","product 2","product 3","profit")
prodCombinations <- prodCombinations[-nrow(prodCombinations), ]
as.data.frame((prodCombinations))
for (i in 1:2000){
  if (prodCombinations[i,1]==prodCombinations[i,2]|prodCombinations[i,2]==prodCombinations[i,3]|prodCombinations[i,1]==prodCombinations[i,3]){
    prodCombinations <- prodCombinations[-i, ]}
}
for (i in 1:2000){
  if (prodCombinations[i,1]==prodCombinations[i,2]|prodCombinations[i,2]==prodCombinations[i,3]|prodCombinations[i,1]==prodCombinations[i,3]){
    prodCombinations <- prodCombinations[-i, ]}
}
for (i in 1:3000){
  if (prodCombinations[i,1]==prodCombinations[i,2]|prodCombinations[i,2]==prodCombinations[i,3]|prodCombinations[i,1]==prodCombinations[i,3]){
    prodCombinations <- prodCombinations[-i, ]}
}
for (i in 1:3000){
  if (prodCombinations[i,1]==prodCombinations[i,2]|prodCombinations[i,2]==prodCombinations[i,3]|prodCombinations[i,1]==prodCombinations[i,3]){
    prodCombinations <- prodCombinations[-i, ]}
}
for (i in 2500:3373){
  if (prodCombinations[i,1]==prodCombinations[i,2]|prodCombinations[i,2]==prodCombinations[i,3]|prodCombinations[i,1]==prodCombinations[i,3]){
    prodCombinations <- prodCombinations[-i, ]}
}
for (i in 1:3364){
  if (prodCombinations[i,1]==prodCombinations[i,2]|prodCombinations[i,2]==prodCombinations[i,3]|prodCombinations[i,1]==prodCombinations[i,3]){
    prodCombinations <- prodCombinations[-i, ]}
}

prodCombinations <- prodCombinations[order(prodCombinations[ ,4],decreasing=TRUE), ]
head(prodCombinations,10)
prodCombinations[which.max(prodCombinations[ ,4]), ]

