# Eyerusalem Abebe
## original graphs#####


bet_cet <- read.csv("~/Desktop/bet_cent4.csv", quote="\"", comment.char="")
#this is now the log plot 
plot(bet_cet)

bet_cet$ID <- seq.int(nrow(bet_cet))
colnames(bet_cet)<-c("Y","X")
plot(bet_cet)
min(bet_cet)#=1.78e-08
#calculating alpha for bewtweenness centrality
(7142)/((7142*log(1.78e-08))-sum(log(bet_cet$Y)))+1
#alpha=0.9254983
#plotting mle curve for betweenness 
curve(0.925*x^(-1.78e-8), xlab = "rank", ylab = "mle of betwenness centrality", main="MLE curve of Betweenness Centrality")
b=curve(0.925*x^(-1.78e-8))

#plotting scatterplot of degree distribution 
deg <- read.csv("~/Desktop/degdist_pro.csv", quote="\"", comment.char="") #you fixed this part 10/22/17 now it's proteins
deg$ID <- seq.int(nrow(deg))
colnames(deg)<-c("Y","X")
plot(deg, main="Degree Distribution of Proteins", xlab="Rank", ylab="Degree Distribution")
min(deg)#=1
#calculating alpha for degree distribution
(7142)/((7142*log(1))-sum(log(deg$Y)))+1
#alpha=0.8499932
#plotting mle curve for degree distribution 
curve(.1501*x^(-.8499), xlab = "rank", ylab = "mle of degree distribution", main="MLE curve of Degree Distribution")
d=curve(.1501*x^(-.8499), xlab=NULL, ylab = NULL, lwd=5)


#### simulated power law clustering graph Betweenness centrality and Degree distribution #####

#betwenness centrality of power law clustering graph 
clus_bet <- read.csv("~/Desktop/PLC_betw.csv")
clus_bet$ID <- seq.int(nrow(clus_bet))
colnames(clus_bet)<-c("Y","X")
plot(clus_bet, xlab="Simulated betweenness centrality", 
     main="Betweenness Centrality of Clust Graph")

##finding the alpha of mle using equation alpha=n/(nloga -sumlogxi) 
min(clus_bet)#=2.76e-05=a
(7143)/((7143*log(2.76e-05))-sum(log(clus_bet$Y)))+1
#alpha=0.05021121
b_clus=curve(.0000444*x^(-0.05021121), xlab = "rank", ylab = "mle of betweenness Centrality", main="MLE curve of power law cluster graph")


#degree distribution of power law clustering graph 
clus_deg= read.csv("~/Desktop/PLC_deg.csv")
clus_deg$ID <- seq.int(nrow(clus_deg))
colnames(clus_deg)<-c("Y", "X")
plot(clus_deg, xlab="simulated degree distribution", 
     main="Degree Distribution of Power Graph")

#finding the alpha for the mle calculation using alpha=n/(nloga -sumlogXi)
min(clus_deg)
(7143)/((7143*log(1))- sum(log(clus_deg$Y)))+1

#calculating the MLE funciton
(1-.8598932)
(1-.8598932)*(1)^(1-.8598932)
#(.1401068)Xi^(-.8598932)
d_clus=curve(0.1401068*x^(-.8598932), xlab = "rank", ylab = "mle of degree distribution",
             main="MLE curve of power law cluster graph")

#MLE graph of both the original degree dist and simulated power law cluster graph's degree dist together
plot(d, type = "l", col="blue", xlab=NULL, ylab = NULL, lwd=4)
par(new=TRUE)
plot(d_clus, type= "l", col='red', axes=FALSE, ylab = "maximum likelihood estimator", lwd=2, xlab = "rank", main = "Degree Distribution MLE Plots", 
     sub = "blue=degree distribution, red=PowerLaw sim degree dist.")

#MLE graph of both the original betweenness centrality and simulated power law cluster graph's betweenness centrality together
plot(b, type = "l", col="blue", xlab=NULL, ylab = NULL, lwd=2)
par(new=TRUE)
plot(b_clus, type= "l", col='red', axes=FALSE, ylab = "maximum likelihood estimator", lwd=2, xlab = "rank", main = "Betweenness Centrality MLE Plots", 
     sub = "blue=degree distribution, red=PowerLaw sim bet cent.")


## chi squqre test between origninal and simulated power law clustering graph####
###Chisquare test####
chisq.test(clus_bet, bet_cet) #p-value is 1
chisq.test(clus_deg, deg) #p-value is < 2.2e-16

