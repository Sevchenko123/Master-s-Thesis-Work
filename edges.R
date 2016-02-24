library(gWidgets)
library(RMySQL)
library(DBI)
library(dbConnect)
library(igraph)
library(ggplot2)

mydb=dbConnect(MySQL(),user='derek',password='derek1',dbname='7cot-fall15',host='130.108.85.104')

n <- dbSendQuery(mydb,'select * from z_normaledges')
normal_edges <- fetch(n,n=-1)

be <- dbSendQuery(mydb,'select * from z_topblockedges')
topblock_edges <- fetch(be,n=-1)
topblock_edges$listID <- topblock_edges$listID + 1000000
topblock_edges$memID <- topblock_edges$memID + 2000000
normal_edges$listID <- normal_edges$listID + 1000000
normal_edges$memID <- normal_edges$memID + 2000000

# blocked members 1025 nodes 75k edges
block_graph <- graph.data.frame(topblock_edges)
V(block_graph)$type <- V(block_graph)$name %in% topblock_edges[,1]
Bl <- bipartite.projection(block_graph)
b_block <- betweenness(Bl$proj1) # member member projection network (1025 nodes 75452 edges) blocked members
b_block <- b_block/1025          # normalized betweenness centrality
c_block <- closeness(Bl$proj1)   # member member projection network (1025 nodes 75452 edges) blocked members 
c_block <- c_block/1025
deg_block <- degree(Bl$proj1)    # degree centrality
deg_block <- deg_block/1025      # normalized degree centrality
density_block <- graph.density(Bl$proj1)
diameter_block <- diameter(Bl$proj1)
modularity_block <- modularity(Bl$proj1,V(Bl$proj1)) # weak modularity (no signs of community structure)
gclust_block <- clusters(Bl$proj1,mode="weak")
gcc_block <- induced.subgraph(Bl$proj1,V(Bl$proj1)[which(gclust_block$membership==which.max(gclust_block$csize))])
gcc_block # 100% nodes in giant connected component
assort_block <- assortativity.degree(Bl$proj1,directed=F) # degree assortativity
cluscoeff_block <- transitivity(Bl$proj1,type="local") # Local Clustering Coefficient of Member projection network of blocked members 
cluscoeff_block <- na.omit(cluscoeff_block)
apl_block <- average.path.length(Bl$proj1) # Average Path Length of member projection network of blocked members
dd_block <- degree.distribution(Bl$proj1)

# Plotting CDF of Betweenness Centrality of blocked member-member projection network
b <- as.data.frame(b_block)
b1 <- as.integer(row.names(b))
b <- cbind(b,b1)
row.names(b)<-NULL
colnames(b)<- c("bc","listID")
b <- b[order(b[,2]),]
row.names(b)<-NULL
b_ecdf <- ecdf(b$bc)
plot(b_ecdf,xlab="Betweenness Centrality",ylab="Density",main="Betweenness Centrality of Blocked Members",col="firebrick1",lwd=3,lty=1,xlim=c(0,10))

# Plotting CDF of Closeness Centrality of blocked member-member projection network
c <- as.data.frame(c_block)
c1 <- as.integer(row.names(c))
c <- cbind(c,c1)
row.names(c)<-NULL
colnames(c)<- c("cc","listID")
c <- c[order(c[,2]),]
row.names(c)<-NULL
c_ecdf <- ecdf(c$cc)
plot(c_ecdf,xlab="Closeness Centrality",ylab="Density",main="Closeness Centrality of Blocked Members",col="firebrick1",lwd=3,lty=1)

# Degree Distribution of blocked member-member projection network 
plot(degree.distribution(Bl$proj1),log="xy",col="red",xlab="Degree",ylab="Proportion",main="Degree Distribution of Listeners in Log-Log Scale")

# Histogram of local clustering coefficient of blocked member-member projection network
hist(cluscoeff_block,col="red",main="Local Clustering Coefficient",xlab="Clustering Coefficient",ylab="Blocked Members",border="white",type="o",lwd="3")


normal_graph <- graph.data.frame(normal_edges)
V(normal_graph)$type <- V(normal_graph)$name %in% normal_edges[,1]
No <- bipartite.projection(normal_graph)
b_normal <- betweenness(No$proj1) # member member projection network (12419 nodes 1356828 edges) normal members network
b_normal <- b_normal/12419        # normalized betweenness centrality
c_normal <- closeness(No$proj1)   # member member projection network (12419 nodes 1356828 edges) normal members network
c_normal <- c_normal/12419
deg_normal <- degree(No$proj1)    # degree centrality
deg_normal <- deg_normal/12419    # normalized degree centrality
density_normal <- graph.density(No$proj1) # density
diameter_normal <- diameter(No$proj1)     # diameter
modularity_normal <- modularity(No$proj1,V(No$proj1)) # weak modularity (no signs of community structure)
gclust_normal <- clusters(No$proj1,mode="weak")
gcc_normal <- induced.subgraph(No$proj1,V(No$proj1)[which(gclust_normal$membership==which.max(gclust_normal$csize))])
gcc_normal # 100% nodes in giant connected component
assort_normal <- assortativity.degree(No$proj1,directed=F) # degree assortativity
cluscoeff_normal <- transitivity(No$proj1,type="local") # Local Clustering Coefficient of Member projection network of blocked members 
cluscoeff_normal<-na.omit(cluscoeff_normal)
apl_normal <- average.path.length(No$proj1) # Average Path Length of member projection network of blocked members
dd_normal <- degree.distribution(No$proj1)  # Degree Distribution



# Plotting CDF of Betweenness Centrality of normal member-member projection network
b_norm <- as.data.frame(b_normal)
b1_norm <- as.integer(row.names(b_norm))
b_norm <- cbind(b_norm,b1_norm)
row.names(b_norm)<-NULL
colnames(b_norm)<- c("bc","listID")
b_norm <- b_norm[order(b_norm[,2]),]
row.names(b_norm)<-NULL
bnorm_ecdf <- ecdf(b_norm$bc)
lines(bnorm_ecdf,xlab="Betweenness Centrality",ylab="Density",main="Betweenness Centrality of Normal Members",col="blueviolet",lwd=3,lty=1,xlim=c(0,10))
legend(4,0.4,c("Blocked Network","Normal Network"),lty=c(1,1),lwd=c(3,3),col=c("firebrick1","blueviolet"))

# Plotting CDF of Closeness Centrality of normal member-member projection network
c_norm <- as.data.frame(c_normal)
c1_norm <- as.integer(row.names(c_norm))
c_norm <- cbind(c_norm,c1_norm)
row.names(c_norm)<-NULL
colnames(c_norm)<- c("cc","listID")
c_norm <- c_norm[order(c_norm[,2]),]
row.names(c_norm)<-NULL
cnorm_ecdf <- ecdf(c_norm$cc)
plot(cnorm_ecdf,xlab="Closeness Centrality",ylab="Density",main="Closeness Centrality of Normal Members",col="blueviolet",lwd=3,lty=1)
legend(4,0.4,c("Blocked Network","Normal Network"),lty=c(1,1),lwd=c(3,3),col=c("firebrick1","blueviolet"))

# Degree Distribution of blocked member-member projection network 
plot(degree.distribution(No$proj1),col="blueviolet",log="xy",xlab="Degree",ylab="Proportion",main="Degree Distribution of Normal Members")

# Histogram of local clustering coefficient of blocked member-member projection network
hist(cluscoeff_normal,col="blueviolet",main="Local Clustering Coefficient",xlab="Clustering Coefficient",ylab="Normal Members",border="white",type="o",lwd="3")


