library(network)
library(sna)
detach("package:igraph")

setwd('~/Desktop/research/network_analysis/')

# Read & prep data
elData<-data.frame(
  from_id=c("1","2","3","1","3","1","2"),
  to_id=c("1", "1", "1", "2", "2", "3", "3"),
  myEdgeWeight=c(1, 2, 1, 2, 5, 3, 9.5),
 someLetters=c("B", "W", "L", "Z", "P", "Q", "E"),
  edgeCols=c("red","green","blue","orange","pink","brown","gray"),
  stringsAsFactors=FALSE
)


# Construct a network from edge list
testNet <- network(elData, directed=FALSE, matrix.type = "edgelist", ignore.eval = FALSE)
testNet%e%'color' <- elData$edgeCols
plot(testNet, vertex.cex=3, attrname='myEdgeWeight', edge.col='color')

relations <- read.csv("./data/relationalData.csv",header=FALSE,stringsAsFactors=FALSE)
relations <- as.matrix(relations)
nodeInfo <- read.csv("./data/vertexAttributes.csv",header=TRUE,stringsAsFactors=FALSE)
rownames(relations) <- nodeInfo$name
colnames(relations) <- nodeInfo$name
nrelations<-network(relations,directed=FALSE)

# Set vertex name
network.vertex.names(nrelations)
nrelations%v%'vertex.names'<- nodeInfo$name

# Load other attr
nrelations%v%"age" <- nodeInfo$age
nrelations%v%"sex" <- nodeInfo$sex
nrelations%v%"handed" <- nodeInfo$handed
nrelations%v%"lastDocVisit" <- nodeInfo$lastDocVisit

list.vertex.attributes(nrelations)


# Network analysis
betweenness(nrelations, cmode='undirected')
clique.census(nrelations, mode = "graph")
closeness(nrelations)
component.dist(nrelations)
component.size.byvertex(nrelations)
components(nrelations)
connectedness(nrelations)
cutpoints(nrelations)
degree(nrelations)
reachability(nrelations)
hierarchy(nrelations)



############### Collaboration Network ###############
# Read in edge list, convert to weighted matrix
edges <- read.table('./output/161718AllEdgesNumbered.txt', sep = '\t', header=TRUE)
edges$C1 <- as.character(edges$C1)
edges$C2 <- as.character(edges$C2)

# Read in node Info
nodes <- read.table('./output/161718AllcountryID.txt', sep = '\t', header = TRUE, comment.char = "")
nodes$ID <- as.character(nodes$ID)
nodes$Country <- as.character(nodes$Country)
nodes$Color <- as.character(nodes$Color)
nodes$pubCount <- nodes$pubCount

# Construct network from edgelist, convert to matrix, match nodes
ctyNet <- network(edges, directed=FALSE, ignore.eval = FALSE, names.eval='weight', matrix.type = 'edgelist')
mat <- as.matrix(ctyNet, attrname = 'weight')
colname <- colnames(mat)
orderedNodes <- nodes[match(colname, nodes$ID),]
rownames(mat) <- orderedNodes$Country
colnames(mat) <- orderedNodes$Country

summary(ctyNet)
print (ctyNet)

weight <- (as.matrix(ctyNet, attrname = 'weight'))
# summary((1-abs((weight-1)/max(weight)*0.95)))
ecol2 <- matrix(gray((1-abs((weight-1)/max(weight)))*0.95), nrow = network.size(ctyNet))
par(mfrow=c(1.2,1.2), mar=c(0,0,0,0))

plot(ctyNet, displaylabels=TRUE, attrname='weight',
     label.cex=0.6, label.pos=5, label=orderedNodes$Country, vertex.col=orderedNodes$Color, 
     #vertex.cex = 2,
     vertex.cex = 1.2*log(log(orderedNodes$pubCount + 1)),
     edge.col = ecol2,
     vertex.lty=0)
     #edge.lwd=0.5*(as.matrix(ctyNet, attrname = 'weight')/1))
     # edge.curve = 1e-04,
     # edge.lwd=as.matrix(ctyNet, attrname = 'weight'))


legend(-28, 1, legend=c("Asia", "Africa", "Europe", "Latin America", "North America", "Oceanic"),
       col=c("#fffac8", "blue"),
       title="Continents", text.font=0.2)


# equivalence 
cl <- equiv.clust(ctyNet, g=NULL, equiv.dist=NULL, equiv.fun="sedist",
            method="hamming", mode="graph", diag=FALSE,
            cluster.method="complete", glabels=NULL, plabels=NULL)
plot(cl)
# igraph plot gives node labels
library(igraph)
detach('package:igraph')
igraph <- graph_from_data_frame(edges, directed = FALSE)
plot.network(igraph, layout = layout_with_graphopt)     

# Network analysis
bt <- betweenness(ctyNet)
clique.census(ctyNet, mode = "graph")
closeness(ctyNet)
# component.dist(nrelations)
# component.size.byvertex(nrelations)
# components(nrelations)
connectedness(ctyNet)
cutpoints(ctyNet)
degree(ctyNet) # colsums
reachability(ctyNet)
hierarchy(ctyNet)


testNet <- graph.data.frame(edges, newnodes, directed=F)


setwd('~/Downloads/Polnet2015/Data/')
testNodes <- read.csv("./Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
testLinks <- read.csv("./Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

newnodes <- nodes[,c(2,1,3,4)]
E(testNet)$width <- (1+E(testNet)$weight)/5
l <- layout.fruchterman.reingold(testNet)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

l <- layout.kamada.kawai(testNet)
plot(testNet,  vertex.label=V(testNet)$Country, vertex.color=V(testNet)$Color, vertex.cex=V(testNet)$PubCount,
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.5, edge.color="gray70", layout=layout.lgl)
