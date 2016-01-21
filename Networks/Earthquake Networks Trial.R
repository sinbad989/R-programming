
netmat = weights[1:9000,1:2]
net = network(netmat,matrix.type="edgelist")

gplot(net,vertex.col = 2,displaylabels=TRUE,gmode="graph",mode="fruchtermanreingold",vertex.cex=1.5,main="Fruchterman-Reingold")




