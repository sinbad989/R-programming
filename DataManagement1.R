
# Sociomatrix/adjacency based network data objects

netmat1 = rbind(c(0,1,1,0,0),c(0,0,1,1,0),c(0,1,0,0,0),c(0,0,0,0,0),c(0,0,1,0,0))
rownames(netmat1) = c("A","B","C","D","E")
colnames(netmat1) = c("A","B","C","D","E")

net1 = network(netmat1,matrix.type="adjacency")

class(net1)
summary(net1)
gplot(net1,vertex.col=2,displaylabels=TRUE)


# Edges list node pairs
netmat2 = rbind(c(1,2),c(1,3),c(2,3),c(2,4),c(3,2),c(5,3))
net2 = network(netmat2,matrix.type="edgelist")
network.vertex.names(net2) = c("A","B","C","D","E")

summary(net2)
gplot(net2,vertex.col=2,displaylabels=TRUE)
