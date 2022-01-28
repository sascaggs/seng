####################################
# Egocentric Networks 
####################################


library(igraph)
library(statnet)
library(tidygraph)
library(ggraph)
library(ggforce)
library(patchwork)
library(intergraph)


egonetwork <- function(
    
    # SETTINGS
    N=30, 
    N_egos=3,
    formula="net ~ edges + nodematch('group')", 
    groups = 4,
    params=c(-3.5,3), 
    seed=777, 
    directed = F, 
    select_egos=F, 
    egoIds) {
    
    # SIMULATION
    set.seed(seed)
    n <- N 
    net <- network(n, directed = directed, density = 0)
    net %v% 'group' <- sample(1:groups, size = n, replace = T)
    g <- simulate(as.formula(formula), coef=params, seed = seed) 
    
    # CONVERT TO IGRAPH
    ig <- asIgraph(g)
    
    # SAMPLE EGOS, FIND NEIGHBORHOODS 
    if (select_egos == F) {
        egos <- sample(V(ig)$vertex.names, size=N_egos, replace = F)
        first <- ego(ig, order=1, nodes=egos, mindist = 0)
        second <- ego(ig, order=2, nodes=egos, mindist = 1) 
    } else if (select_egos == T) { 
        egos <- egoIds 
        first <- ego(ig, order=1, nodes=egos, mindist = 0)
        second <- ego(ig, order=2, nodes=egos, mindist = 1)
    }
    
    # CREATE ATTRIBUTES 
    tg <- ig %>%
        as_tbl_graph() %>%
        activate(nodes) %>%
        mutate(is.ego =  as.factor(ifelse( vertex.names %in% egos, 2, 
                                           ifelse( vertex.names %in% unlist(first), 1, 0))), 
               egolab = ifelse( vertex.names %in% egos, paste('Ego', vertex.names), '' )) %>% 
        activate(edges) %>%
        mutate(Neighborhood = as.factor(ifelse(from %in% egos, 2, 
                                               ifelse(to %in% egos, 2, 
                                                      ifelse( from %in% unlist(first), 1,
                                                              ifelse( to %in% unlist(first), 1,  0 ))))))
    
    # SAVE EGO NETWORKS
    E(ig)$id <- seq_len(ecount(ig))
    V(ig)$vid <- seq_len(vcount(ig))
    egographs <- make_ego_graph(ig,order=2,nodes=egos)
    
    return(list(g, tg, egographs))
    
}


egonetwork()

egonetwork(N=80, 
           params = c(-4,3),
           N_egos = 20) [[2]] %>%
    ggraph() + 
    geom_edge_link0(aes(color=Neighborhood)) + 
    geom_node_point(aes(color=is.ego)) + 
    geom_node_text(aes(label=egolab))
