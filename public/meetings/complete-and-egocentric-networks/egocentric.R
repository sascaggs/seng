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


egonetworks <- function(
    
    # SIMULATION SETTINGS
    N=30,   
    directed = F,
    formula="net ~ edges + nodematch('group')",
    params=c(-3.5,3),
    groups = 4,
    
    # EGONETWORK SETTINGS
    select_egos=F,
    N_egos=3,
    egoIds,
    seed=777) 

{
    
    # SIMULATE A COMPLETE NETWORK
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
        mutate(Neighborhood =  as.factor(ifelse( vertex.names %in% egos, 1, 
                                                 ifelse( vertex.names %in% unlist(first) | vertex.names %in% unlist(second), 2, 'Unobserved'))), 
               egolab = ifelse( vertex.names %in% egos, paste('Ego', vertex.names), '' ), 
               IsEgo = ifelse(vertex.names %in% egos, 'Yes','No')) %>% 
        activate(edges) %>%
        mutate(Neighborhood = as.factor(ifelse(from %in% egos, 1, 
                                               ifelse(to %in% egos, 1, 
                                                      ifelse( from %in% unlist(first), 2,
                                                              ifelse( to %in% unlist(first), 2,  'Unobserved' ))))))
    
    # SAVE EGO NETWORKS
    E(ig)$id <- seq_len(ecount(ig))
    V(ig)$vid <- seq_len(vcount(ig))
    egographs <- make_ego_graph(ig,order=2,nodes=egos)
    
    # RETURN COMPLETE, TIDYGRAPH, AND EGONETWORKS IN A LIST
    return(list(g, tg, egographs))
    
}

egonetworks(N=80, 
           params = c(-4,3),
           N_egos = 20) [[2]] %>%
    ggraph() + 
    geom_edge_link0(aes(color=Neighborhood)) + 
    geom_node_point(aes(color=Neighborhood, shape=IsEgo), size=2) + 
    geom_node_label(aes(label=egolab), repel = T) + 
    theme_void() + #theme(legend.position = 'none') + 
    scale_edge_color_manual(values=c('#3300ff','magenta','#00000033')) + 
    scale_color_manual(values=c('#3300ff','magenta','#00000033'))
