library(statnet)
library(igraph)
library(tidygraph)
library(ggraph)
library(tidyverse)
library(ggprism)

prickles <- theme(panel.background = element_rect(fill='black'),
                  axis.ticks.length = unit(0.2, 'in'), 
                  panel.grid = element_blank(),
                  prism.ticks.length = unit(0.15, 'in'), 
                  text = element_text(family='mono', size=13))

set.seed(100)

# Directed and undirected graphs ------------------------------------------
N <- 100 
un100 <- network(N, directed = F, density = 0.01)
dn100 <- network(N, directed = T, density = 0.01)

plot(un100)
plot(dn100)

didens <- function(N) N / (N*(N-1)) 
undens <- function(N) N / (N*(N-1)/2) 
didens(100)
undens(100)

# A graph is considered sparse if the number of edges is less the number of vertices; a value given by e < O(n), the orthogonal group. What this means in terms of density depends on whether the graph is directed or undirected, and the total number of nodes. If possible number of edges in a directed graph is n(n-1) and in an undirected graph, n(n-1)/2, then n / n(n-1) and n / (n(n-1) / 2) gives the density threshold for sparseness for a graph of any number of nodes. According to Figure, a graph of any size that has a density larger 0.02 is not sparse. 

Nseq <- seq(1,10000, by=99)
tibble(di = didens(Nseq), un = undens(Nseq), Nseq) %>%
    gather(key=key, value=value, -Nseq) %>%
    ggplot(aes(x=Nseq)) + 
    prickles + 
    geom_line(aes(y=value, color=key), lwd=0.8) + 
    scale_x_continuous(guide = 'prism_minor', 
                       breaks = seq(0,10000,1000)) + 
    scale_y_continuous(guide = 'prism_minor', 
                       breaks = seq(0,0.02,0.002)) + 
    theme(
        legend.box.background = element_rect(fill='white'), 
        legend.key = element_rect(fill='white')) + 
    scale_color_manual(values = c('cyan','magenta'), labels = c('directed','undirected')) + 
    labs(x='Vertices', y='Density', color=NULL)

# Sparse graphs can be characterized by a greater number of components and isolates. But as the number vertices in a graph increases, the number of components in a graph diminishes, even for sparse graphs. For an graph larger than 1000 vertices, a visual inspection is not sufficient to determine whether it is sparse. 

set.seed(777)
Nseq <- seq(100,1000, by=10)

compcurve <- function(Nseq, S=0.01, directed = F) { 
    if ( directed == F ) {
        
        uL <- sapply(Nseq, network, directed = F, 
                     density = S, simplify = F)
        uC <- lapply( lapply( 
            lapply(uL, component.size.byvertex, connected = 'weak'), unique), length)
        
        return(data.frame(components = unlist(uC), 
                          vertices = Nseq, 
                          density = S))
    } else 
        if ( directed == T ) {
            dL <- sapply(Nseq, network, directed = T, 
                         density = S, simplify = F)
            dC <- lapply( lapply( 
                lapply(uL, component.size.byvertex, connected = 'weak'), unique), length)
            
            return(data.frame(components = unlist(uC), 
                              vertices = Nseq, 
                              density = S))
        }
    
}
Nseq <- c(3,4,5)

l <- list()
testfun <- function(Nseq) {
    for(i in seq_along(Nseq)) {
        l[[i]] <- network(Nseq[i], directed = F, density = sparsepoint(Nseq[i], directed = F))
    }
    c <- lapply( lapply( 
        lapply(l, component.size.byvertex, connected = 'weak'), unique), length)
    return(data.frame(Nseq, components = unlist(c)))
}

compcurve(Nseq, S=Nseq)

cc <- compcurve(Nseq)

head(cc)
tibble(cc) %>%
    gather(key=key, value=value, -Nvertices) %>%
    ggplot(aes(x=Nseq)) + 
    prickles + 
    geom_line(aes(y=value, color=key), lwd=0.8) +
    theme(legend.position = c(0.7,0.8), 
          legend.box.background = element_rect(fill='white'), 
          legend.key = element_rect(fill='white'))  +
    scale_x_continuous(guide = 'prism_minor', 
                       breaks = seq(0,1000,100)) + 
    scale_y_continuous(guide = 'prism_minor', 
                       breaks = seq(1,9,2)) + 
    scale_color_manual(values = c('cyan','magenta'), labels = c('directed','undirected')) + 
    labs(x='Vertices', y='Components', color=NULL)





degree(network(, density = 0.1))
