#Eyerusalem Abebe

#BarabasiA betweenness centrality MLE calculation
install.packages("xlsx")
install.packages("readr")
library(readr)
bet_cet_BA <- read_csv("Desktop/BA_betcent_graphviz.csv", col_names = FALSE)
bet_cet_BA$ID <- seq.int(nrow(bet_cet_BA))
colnames(bet_cet_BA)<-c("Y","X")
plot(bet_cet_BA , main="Betweeness Centrality Hist for BA", xlab="Rank", ylab="Betweenness Centrality")

#finding the alpha of mle using equation alpha=n/(nloga -sumlogxi) 
(7143)/((7143*log(2.72e-5))-sum(log(bet_cet_BA$Y)))+1
#= 0.06334376
#mle model 
#these two don't work model<-(Y~.00004956*x^(-0.0633))
#plot(model)
#curve of power function for betweenness centrality 
curve(.0000495*x^(-0.0633),  main="MLE plot for Betweenness Centrality")


#MLE Calculation and histogram of Degree Distribution for Barabasi Albert Graph 
deg_dist_BA <- read_csv("Desktop/BA_degdist_graphviz.csv", col_names = FALSE)
deg_dist_BA$ID <- seq.int(nrow(deg_dist_BA))
colnames(deg_dist_BA)<-c("Y","X")
plot((Y)~(X), data=deg_dist_BA , main="Degree Distribution Hist. for BA", xlab="Rank", ylab="Degree Distribution")
lgdegree_ba<-plot(log(Y)~(X), data=deg_dist_BA, main="Degree Distribution of All Proteins BA", xlab="Rank", ylab="Degree Distribution")


#finding the alpha of mle using equation alpha=n/(nloga -sumlogxi) 
(7143)/((7143*log(7.15e2))-sum(log(deg_dist_BA$Y)))+1
#=-1.002048
#mle model 
#these two don't work model<-(Y~1037014.356x^1.002)
#plot(model)
#curve of power function for betweenness centrality 
curve(1037014.356*x^(1.002),  main="MLE plot for Degree Distribution")

#using a=1 for mle calculation of betweenness centrality
#finding the alpha of mle using equation alpha=n/(nloga -sumlogxi) 
(7143)/((7143*log(1))-sum(log(bet_cet_BA$Y)))+1
#= 1.10588
#mle model 
#curve of power function for betweenness centrality 
curve(-0.105*x^(-1.105),  main="MLE plot for Bet. Cent. (a=1)")

#using a=1 for degree distribution
#finding the alpha of mle using equation alpha=n/(nloga -sumlogxi) 
(7143)/((7143*log(1))-sum(log(deg_dist_BA$Y)))+1
#=-1.002048
#mle model 
#these two don't work model<-(Y~1037014.356x^1.002)
#plot(model)
#curve of power function for betweenness centrality 
curve(0.1415*x^(-0.8585),  main="MLE plot for Degree Dist. a=1")




#MLE Calculation and histogram of Degree Distribution for Barabasi Albert Graph after deleting the beginning  
deg_dist_BA2 <- read_csv("Desktop/BA_degdist_2.csv", col_names = FALSE)
deg_dist_BA2$ID <- seq.int(nrow(deg_dist_BA2))
colnames(deg_dist_BA2)<-c("Y","X")
plot((Y)~(X), data=deg_dist_BA2 , main="Degree Distribution Hist BA n=6143", xlab="Rank", ylab="Degree Distribution")
lgdegree_ba<-plot(log(Y)~(X), data=deg_dist_BA2, main="Degree Distribution of (6143 proteins) BA", xlab="Rank", ylab="Degree Distribution")


#finding the alpha of mle using equation alpha=n/(nloga -sumlogxi) 
(6143)/((6143*log(7.15e2))-sum(log(deg_dist_BA2$Y)))+1
#= -1.302272
#mle model 
#these two don't work model<-(Y~1037014.356x^1.002)
#plot(model)
#curve of power function for betweenness centrality 
curve(8564811.862*x^(1.302),  main="MLE plot for Degree Distribution n=6143")

