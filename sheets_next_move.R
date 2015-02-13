load("sortDT.newiOS.RData")

events <- sortDT.newiOS$event
indx <- c()
pb <- txtProgressBar(min = 0, max = nrow(sortDT.newiOS), style = 3)
for (i in 1:nrow(sortDT.newiOS)){
    if (grepl("Sheet", events[i]) & grepl("Sheet", events[i+1])) indx <- c(indx, i, i+1)
    setTxtProgressBar(pb, i)
}
close(pb)

indx <- unique(indx)
cols <- c(grep("User.Id", names(sortDT.newiOS)),
          grep("event", names(sortDT.newiOS)),
          grep("connections", names(sortDT.newiOS)),
          grep("children", names(sortDT.newiOS)),
          grep("timeGMT", names(sortDT.newiOS)))

##choose filter from: "none", "connections", "children", "connections and
##children" and "no connections and no children"
sheet.df <- sortDT.newiOS[indx,cols]
filter <- "no connections and no children"
if (filter == "none"){
    sheet.df <- sheet.df
} else if (filter == "connections"){
    sheet.df <- subset(sheet.df, Number.of.connections > 0 & Number.of.children == 0)

} else if (filter == "children"){
    sheet.df <- subset(sheet.df, Number.of.connections == 0 & Number.of.children > 0)
} else if (filter == "connections and children") {
    sheet.df <- subset(sheet.df, Number.of.connections > 0 & Number.of.children > 0)
} else sheet.df <- subset(sheet.df, Number.of.connections == 0 & Number.of.children == 0)

sheet.events <- unique(sheet.df$event) 
users.list <- split(sheet.df$event, sheet.df$User.Id)
users.list <- users.list[which(lapply(users.list, function(x) length(x)) > 1)]

rm.consec.duplicates <- function(vect){
    rmidx <- c()
    for (i in 2:length(vect)){
        if (vect[i] == vect[i-1]){
            rmidx <- append(rmidx, i)
        }
    }
    if (length(rmidx) > 0) return(vect[-rmidx])
    else return(vect)
}

next.move <- function(vect, move){
    t <- table(as.character(vect[which(vect == move)+1]))
    return(t)
}

users.list <- lapply(users.list, function(x) rm.consec.duplicates(x))
list.df <- list()
for (n in sheet.events){
    next.move.l <- lapply(users.list, function(x) next.move(x, n))
    names(next.move.l) <- NULL
    next.move.v <- unlist(next.move.l)
    next.move.df <- aggregate(next.move.v, by=list(names(next.move.v)) , FUN = sum)
    list.df <- append(list.df, list(next.move.df))
}

sort.list <- lapply(list.df, function(y) y[order(y$x, decreasing = TRUE),])

pdf("next_moves.pdf")
for (i in 1:length(sort.list)){
    lab.perc = round(sort.list[[i]]$x/sum(sort.list[[i]]$x), 2)
    lab.num = sort.list[[i]]$x
    par(mar = c(15,4,4,2))
    bplt <- barplot(sort.list[[i]]$x, names = sort.list[[i]]$Group.1,
                    ylim = c(0,max(sort.list[[i]]$x)*1.5),
                    ylab = "Number",
                    main = paste(as.character(sheet.events[i]),as.character(sum(sort.list[[i]]$x)),sep=" "),
                    las=2)
    text(y=sort.list[[i]]$x, x= bplt, labels=lab.perc, pos = 3, cex = 0.75)
    text(y=sort.list[[i]]$x+(0.1*max(sort.list[[i]]$x)), x= bplt, labels=lab.num, pos = 3, cex = 0.75)
    
}
dev.off()
