
library(statnet)
library(igraph)
set.seed(666)
N = 40
g = network(N, directed = F, density = 0)
g %v% 'group' = sample(1:3, size = N, replace = T)


sim_g = simulate(g ~ edges + nodematch('group'),  coef = c(-4,3))
plot(sim_g, vertex.col = g %v% 'group')

ig = intergraph::asIgraph(sim_g)

steps = 100
l = list()
for(i in 1:steps) {
    c = cluster_walktrap(ig, steps = i)
    l[[i]] = c$membership
}
l
n_communties = unlist(lapply(lapply(l, unique), length))

plot(1:steps, n_communties, type='o')



r = seq(0.001,1, length=200)
l = list()
for(i in 1:length(r)) {
    c = cluster_leiden(ig, resolution_parameter = r[i])
    l[[i]] = c$membership
}
n_communties = unlist(lapply(lapply(l, unique), length))
plot(1:length(r), n_communties, type='o')



r = seq(0.01,1, length=100)
df = data.frame(matrix(0, nrow = N, ncol = length(r)))
for(i in 1:length(r)) {
    c = cluster_leiden(ig, resolution_parameter = r[i])
    df[,i] = c$membership
}

df
colnames(df) = paste0('r', r)
head(df)
x = apply(df, 1, mean)
y = apply(df, 1, function(x) length(unique(x)))

plot(1:N, x)
plot(r,y )

n_communties = unlist(lapply(lapply(l, unique), length))
plot(1:length(r), n_communties, type='o')




subgraph_isomorphic(c(1,2))


cyc = graph.data.frame(rbind(c(1,2),c(2,3),c(3,4),c(4,1)), directed = F)
star = graph.data.frame(rbind(c(1,2),c(1,3),c(1,4)), directed = F)

subgraph_isomorphic(cyc, ig)
subgraph_isomorphic(star, ig)
subgraph_isomorphisms(cyc, ig)
subgraph_isomorphisms(star, ig)

