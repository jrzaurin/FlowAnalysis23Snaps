######################################################################
####given a trigger move, the code below will plot the flows that#####
####start with that trigger move.#####################################
######################################################################
dir <- "/Users/javier/Working/23snaps/MixPanel/FlowsAndSequences/20150501-20151201"
setwd(dir)

##load("sortDT.newiOS.RData") ##uncomment this line if running locally
source("support_functions_forward.R")
source("acquire_data.R")

trigger.move <- "invitesomeone"
first.move <- select.event(trigger.move)
first.move <- lapply(first.move, function(x) rm.consec.duplicates(x))
mov <- sort(table(sapply(first.move,  "[", 2)),decreasing = TRUE)
mov <- trim.vect(mov, 0.95)
df <- data.frame(V1 = replicate(length(mov), trigger.move), V2 = names(mov))

v.moves <- sapply(first.move,  "[", 2)
first.mov.names <- names(mov) 
first.num.users <- c()
for (i in 1:length(first.mov.names)){
    tmp.users <- length(unique(names(v.moves[which(v.moves == first.mov.names[i])])))
    first.num.users <- c(first.num.users, tmp.users)
}
names(first.num.users) <- names(mov)
rm(tmp.users)

require(data.table)
count = 1
num.levels = 10
flows <- list()
users <- list() 
while (count <= (num.levels-1)) {
    users <- append(users, num.users(df))
    flows <- append(flows, num.flow(df))
    df <- diagram.df(df)
    count = count+1
}
flows <- append(list(mov), flows)
users <- append(list(first.num.users), users)

df <- add.layer(df)
df <- add.node(df)

##Preparing the data for the igraph package
edges <- rbind(na.omit(df[1:2]),
               do.call('rbind',
                       lapply(1:(ncol(df)-2), function(i)
                           na.omit(setNames(df[(1+i):(2+i)],
                                            names(df[1:2]))))))

##remove duplicated edges.
rm.ind <- c()
for (i in 2:nrow(edges)){
    if (edges[i,1] == edges[i-1,1] & edges[i,2] == edges[i-1,2]){
        rm.ind <- c(rm.ind, i)
    }
}
edges <- edges[-rm.ind,]
lab1 <- as.numeric(unlist(flows))
edge.w <- ((lab1/max(lab1))*9) + 1
edge.c <- ifelse(lab1 <= as.numeric(quantile(lab1)[3]), "green",
                 ifelse(lab1 > as.numeric(quantile(lab1)[3]) & lab1 <= as.numeric(quantile(lab1)[4]),
                        "blue","red"))   
lab2 <- as.numeric(unlist(users))
edge.lab <- paste(lab1, lab2, sep = "/")

##select the triggering move and the "branches"
mov.flows <- which(edges[,1] == paste(trigger.move, ".l1", sep = ""))

##create a list with the individual flows to be plotted
flows.list <- list()
for (i in mov.flows){
    temp.l <- final.flow.path(i, num.levels)
    flows.list <- append(flows.list, temp.l)
} 

##plot the branches == flows.
require(igraph)
pdf("test.pdf", width=16, height=15)
par(mar=c(0,0,0,0))
for (i in 1:length(flows.list)){
    flow.g <- graph.data.frame(edges[flows.list[[i]],])
    E(flow.g)$curved <- 0
    E(flow.g)$label <- edge.lab[flows.list[[i]]]
    E(flow.g)$width <- edge.w[flows.list[[i]]]
    E(flow.g)$color <- edge.c[flows.list[[i]]] 
    E(flow.g)$label.cex <- 0.6
    V(flow.g)$label.cex = 0.6
    plot.igraph(flow.g, vertex.size=0, vertex.shape = "circle",
                vertex.label.dist=0.05, vertex.label.degree=-90,
                edge.arrow.size=0,layout=-layout.reingold.tilford(flow.g)[,2:1])
}
dev.off()
