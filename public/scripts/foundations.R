## ---- echo=F, message=F, warning=F, fig.cap="Two nodes (red) connected by a single undirected edge."----
library(statnet)
g <- network(matrix(c(0,1,1,0), nrow=2),
             directed = F)
par(mar=c(0,0,0,0))
plot(g, vertex.cex = 5, edge.lwd=5)


## ---- echo=F, fig.cap="A single layer (one-mode) network with 10 nodes."----
set.seed(27)
m <- matrix(c(rbinom(100, 1, prob = 0.3)), 
            nrow = 10)
diag(m) <- 0
par(mar=c(0,0,0,0))
plot(network(m, directed = F))


## ---- echo=F, fig.cap="A directed triad. Arrows indicate the flow of edges. A reciprocal edge is colored black."----
e <- rbind(c('i','j'),
           c('j','i'),
           c('i','k'))
g <- as.network(e, directed = T)
par(mar=c(0,0,0,0))
gplot(g, displaylabels = T, mode = "kamadakawai", edge.col = c('black','black','gray85'))


## ---- echo=F, fig.cap="An undirected weighted network. Edges is this network have values ranging from 0 to 3."----
set.seed(27)
m <- matrix( rbinom(100, 3, 0.2), 
             nrow=10)
diag(m) <- 0
g <- as.network(m, directed = F, matrix.type = "adjacency", 
                ignore.eval = F, names.eval = "weight")
par(mar=c(0,0,0,0))
gplot(g, edge.lwd = (g %e% 'weight')^2, usearrows=F)


## ---- echo=F, fig.cap="Two-mode (bipartite) network. Note that edges only occur between red nodes (first layer) and blue nodes (second layer)."----
e <- rbind(c('A','I'),
           c('B','K'),
           c('B','I'),
           c('C','J'),
           c('C','I'), 
           c('D','K'),
           c('D','I'))
g <- as.network(e, bipartite=4, directed=FALSE)
par(mar=c(0,0,0,0))
gplot(g, gmode="twomode", usearrows=FALSE, displaylabels=TRUE)


## ---- echo=F, message=F, warning=F, fig.cap="Multilevel network constructed using `igraph` and `graphlayouts` packages in `R`. Orange nodes and edges are one layer; black nodes and edges are a second layers; and purple edges indicate interlayer connections."----
library(igraph)
library(graphlayouts)

set.seed(27)
m1 <- matrix(rbinom(100, 1, 0.2), 
             nrow=10)
diag(m1) <- 0
rownames(m1) <- colnames(m1) <- LETTERS[1:10]

m2 <- matrix(rbinom(64, 1, 0.2), 
             nrow=8)
diag(m2) <- 0
rownames(m2) <- colnames(m2) <- letters[1:8]


g1 <- graph.adjacency(m1)
g2 <- graph.adjacency(m2)
el1 <- as.data.frame(get.edgelist(g1))
el1$V3 <- 'black'
el2 <- as.data.frame(get.edgelist(g2))
el2$V3 <- 'tomato'
int <- as.data.frame(
    rbind(c('a','A'),
          c('b','B'),
          c('a','B'),
          c('b','A'),
          c('a','C'),
          c('b','D'),
          c('c','F'),
          c('c','E'))
)
int$V3 <- 'purple'


ml <- rbind(el1, el2, int)

vlm <- data.frame(
    id = c(LETTERS[1:10], letters[1:8]), 
    lvl = c(rep(1,10), rep(2,8)), 
    vcol = c(rep('black',10), rep('tomato',8))
)

mlg <- simplify(graph.data.frame(ml, vertices = vlm, directed = F), 
                edge.attr.comb = 'first')

xy <- layout_as_multilevel(mlg, type = 'all')

par(mar=c(0,0,0,0))
plot(mlg, layout = xy, vertex.label = NA,
     vertex.color = V(mlg)$vcol,
     vertex.frame.color = V(mlg)$vcol,
     edge.color = E(mlg)$V3)


## ---- echo=F, fig.cap="A social-ecological motif following the conventions of Bodin and Tengo (2012). Orange nodes are social, with orange social-social edges. Green nodes are ecological, with green ecological-ecological connections. Blue edges represent social-ecological connections."----
e <- rbind(c('S1','S2','tomato'),
      c('E1','E2','forestgreen'),
      c('S1','E1','cornflowerblue'),
      c('S2','E2','cornflowerblue'))
g <- graph.data.frame(e, directed = F)

plot(g, vertex.label = NA,
     vertex.color = c('tomato','forestgreen','tomato',
                      'forestgreen'
                      ), 
     edge.color = E(g)$V3,
     edge.width = 2)


