########################################################################
####A series of functions that will be later used to manipulate and#####
####prepare it for the flow chart plot##################################
########################################################################

##removing consecutive duplicates in a data table
rmDuplicates <- function(DT){
    rmidx <- c()
    pb <- txtProgressBar(min = 0, max = nrow(DT), style = 3)
    for (i in 2:nrow(DT)){
        if (DT$User.Id[i] == DT$User.Id[i-1] &
            DT$time[i] == DT$time[i-1] &
            DT$event[i] == DT$event[i-1]){
            rmidx <- append(rmidx, i)
        }
        setTxtProgressBar(pb, i)
    }
    close(pb)
    return(DT[-rmidx,])
}

##substitute more than one pattern
gsub2 <- function(pattern, replacement, vchar, ...) {
     for(i in 1:length(pattern))
         vchar <- gsub(pattern[i], replacement[i], vchar, ...)
     vchar
 }

##Add a column with the events all in one word
addEvents <- function(DT){
    from <- c(" ", ":", "-", "Sheet", "Flow")
    to <- c("","","","","")
    CleanEvents <- tolower(gsub2(from, to, levels(DT$event)))
    library(plyr)
    CleanEvents <- mapvalues(DT$event,
                             from = levels(DT$event),
                             to = CleanEvents)
    return(CleanEvents)
}


##Trim leading or trailing whitespaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

##Given a list of vectors, split them at a certain element:
extractSeq <- function(inp,splitMove) {

    ##Paste all in a string
    tempSeqL <- lapply(inp, function(x) paste(x, collapse=" "))
    ##split at splitMove
    tempSeqL <- lapply(tempSeqL, function(x) strsplit(x, splitMove))
    ##unlist the list of lists
    tempSeqL <- lapply(tempSeqL, function(x) unlist(x))
    ##remove the values "" and " " from the vectors
    tempSeqL <- lapply(tempSeqL, function(x) x[!(x == "" | x == " ")])

    tempSeqL <- lapply(tempSeqL, function(y) trim(y))
    tempSeqL <- lapply(tempSeqL, function(x) strsplit(x, " "))

    ##Create a list with the adequate format
    tmpIndSeqL <- list()
    for (i in 1:length(tempSeqL)){
        if (length(tempSeqL[[i]]) != 0){
            for (j in 1:length(tempSeqL[[i]])){
                tmpIndSeqL <- append(tmpIndSeqL, list(tempSeqL[[i]][[j]]))
            }
        }
    }

    ##Define names so I can locate the Users that went through this sequences
    UserLengths <- lapply(tempSeqL, function(x) length(x))
    NamesList <- c()
    for (i in 1:length(UserLengths)){
        tempName <- names(UserLengths)[i]
        reptimes <- UserLengths[i]
        NamesList <- append(NamesList, rep(tempName, reptimes))
    }

    ##Just renaming
    names(tmpIndSeqL) <- NamesList
    return(tmpIndSeqL)
}

select.event <- function(eventname){
    ##Split by User.Id
    seq.l <- split(as.character(sortDT.newiOS$CleanEvents), sortDT.newiOS$User.Id)
    ##Main Session start: defined by UserSessionStarted
    seq.l <- seq.l[grep("usersessionstarted", seq.l)]
    seq.l <- extractSeq(seq.l, "usersessionstarted")
    ##select sequences with eventname
    event.l <- seq.l[grep(eventname, seq.l)]
    names.seq.l <- names(event.l)
    ##Names of Users for which the 1st "move" is eventname
    names.seq.l <-names.seq.l[which(lapply(event.l, "[", 1) == eventname)]
    ##Split at eventname
    flow.event <- extractSeq(event.l,eventname)
    names.flow.l <- names(flow.event)

    ##Adding eventname at the beginning of the 1st Flow if required
    idx <- c()
    i=1
    while (i < length(flow.event)){
        if (names.flow.l[i] %in% names.seq.l){
            idx <- append(idx, which(names.flow.l[i] == names.flow.l)[1])
            j <- length(which(names.flow.l[i] == names.flow.l))
        } else {
            j = 1
        }

        i <- i + j
    }
    flow.event[idx] <- lapply(flow.event[idx],
                                     function(x) c(eventname, x))

    ##Adding eventname at the beginning of the Flows afte the 1st one
    idx2 <- c()
    i=1
    pb <- txtProgressBar(min = 0, max = length(unique(names(flow.event))), style = 3)
    for (i in 1:length(unique(names(flow.event)))){
        if (sum(unique(names(flow.event))[i] == names(flow.event)) > 1) {
            idx2 <- which(names(flow.event) == unique(names(flow.event))[i])[-1]
            flow.event[idx2] <- lapply(flow.event[idx2],
                                              function(x) c(eventname, x))
        }
        setTxtProgressBar(pb, i)
    }
    ##Selecting flows for which the 1st move is eventname
    first.event <- flow.event[which(lapply(flow.event,
                                                  "[", 1) == eventname)]
    return(first.event)
}

##Remove consecutive duplicates within the elements of the list
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

##stop when the number of flows accounts for the "perc" of the total flows
##emerging from a certain move
trim.vect <- function(vect, perc) {
    s = 0;i = 1;
    tot = sum(vect)
    if (length(vect) > 2){
        while (s < perc*tot) {
            s = s + as.numeric(vect[i])
            i = i + 1
        }
        return(vect[1:(i-2)])
    } else return(vect)
}

##given a data frame of flows and an index/row, this function will return a list
##with all the flows that start with the sequence of moves in that index/row
select.list <- function(s.df, s.idx) {
    ct = 1
    depth <- ncol(s.df)
    ll <- first.move
    while (ct <= depth){
        t.l <-  ll[lapply(ll, "[", ct) == s.df[s.idx,ct]]
        ct = ct + 1
        ll <- t.l
    }
    return(Filter(Negate(is.null), ll))
}

##given a data frame, a vector of moves and an index, this function will return
##a data frame in wich the last column will be the next move in the
##corresponding flow.
create.df <- function(c.df, vect, c.idx){
    ct = 1
    n.col <- ncol(c.df) + 1
    tdf <- data.frame(matrix(ncol = n.col, nrow = length(vect)))
    while (ct <= ncol(c.df)){
        tdf[,ct] <- replicate(length(vect),c.df[c.idx,ct])
        ct = ct+1
    }
    tdf[,n.col] <- names(vect)
    return(tdf)
}

##Given a data frame this function will combine the previous two function and
##return a data frame with the next move in the flow for all rows.
diagram.df <- function(diag.df) {
    tmp.l <- list()
    for (i in 1:nrow(diag.df)){
        if (sum(is.na(diag.df[i,])) >= 1){
            tmp.df <- data.frame(diag.df[i,], NA)
        } else {
            nc <- ncol(diag.df)
            L <- select.list(diag.df, i)
            tmp.mov <- sort(table(sapply(L,  "[", nc+1)),decreasing = TRUE)
            tmp.mov <- trim.vect(tmp.mov, 0.95)
            if (length(tmp.mov) == 0) tmp.df <- data.frame(diag.df[i,], NA)
            else  tmp.df <- create.df(diag.df, tmp.mov, i)
        }
        tmp.l <- append(tmp.l, list(tmp.df))
    }
    return(as.data.frame(rbindlist(tmp.l)))
}

##Given a data frame of flows, this function will return the number of
##individual flows that are identical = the labels of the edges
num.flow <- function(flow.df) {
    tmp.l <- list()
    for (i in 1:nrow(flow.df)){
        if (sum(is.na(flow.df[i,])) == 0) {
            nc <- ncol(flow.df)
            L <- select.list(flow.df, i)
            tmp.mov <- sort(table(sapply(L,  "[", nc+1)),decreasing = TRUE)
            tmp.mov <- trim.vect(tmp.mov, 0.95)
            tmp.l <- append(tmp.l, list(tmp.mov))
        }
    }
    tmp.l <- tmp.l[which(lapply(tmp.l, function(x) length(x)) > 0)]
    return(tmp.l)
}


##Given a data frame of flows, this function will return the number of
##users that follow that low

num.users <- function(users.df) {
    tmp.l <- list()
    for (i in 1:nrow(users.df)){
        if (sum(is.na(users.df[i,])) == 0) {
            num.users.flow <- c()
            nc <- ncol(users.df)
            L <- select.list(users.df, i)
            V <- sapply(L,  "[", nc+1)
            tmp.mov <- sort(table(V),decreasing = TRUE)
            tmp.mov.names <- names(trim.vect(tmp.mov, 0.95))
            for (j in 1:length(tmp.mov.names)){
                tmp.users <- length(unique(names(V[which(V == tmp.mov.names[j])])))
                names(tmp.users) <- tmp.mov.names[j]
                num.users.flow <- c(num.users.flow, tmp.users) 
            }
            tmp.l <- append(tmp.l, list(num.users.flow))
        }
    }
    tmp.l <- tmp.l[which(lapply(tmp.l, function(x) sum(x != 0)) > 0)]
    return(tmp.l)
}

##renaming the columns of the data frame base on the layer/level and the node in
##the diagram
add.layer <- function(l.df){
    for (j in 1:ncol(l.df)){
        lev <- paste(".l",j,sep = "")
        if (sum(is.na(l.df[,j])) == 0)
            l.df[,j] <- paste(l.df[,j], lev, sep = "")
        else
            l.df[,j][-which(is.na(l.df[,j]))] <-
                paste(l.df[,j][-which(is.na(l.df[,j]))],lev, sep = "")
    }
    return(l.df)
}

add.node <- function(n.df) {
    for (j in 3:ncol(df)){
        idx <- c()
        for (i in 1:(nrow(n.df)-1)){
            if (is.na(n.df[i+1,j-1])) {
                idx <- c(idx, i)
            } else if (!is.na(n.df[i,j-1]) &
                       n.df[i+1,j-1] != n.df[i,j-1]){
                idx <- c(idx, i)
            }
        }
        idx <- c(idx, nrow(n.df))

        n.names <- c()
        for (i in 1:length(idx))
            n.names <- c(n.names, paste(".n",i,sep = ""))

        for (i in 1:length(idx)){
            if (i == 1) ind.list <- list(seq(idx[i]))
            else ind.list <- append(ind.list,
                                    list(seq(idx[i-1]+1,idx[i])))
        }
        names(ind.list) <- n.names

        for (l in 1:length(ind.list)){
            for (i in ind.list[[l]]){
                if (!is.na(n.df[i,j]))
                    n.df[i,j] <- paste(n.df[i,j], names(ind.list[l]), sep="")
            }
        }
    }
    return(n.df)
}

##function to select the sequence of numbers for a certain flow
flow.select <- function(f){
    temp.f <- c()
    for (i in 1:length(f)){
        f.l <- which(edges[,1] == edges[f[i],2])
        temp.f <- c(temp.f, f.l)
    }
    return(temp.f)
}

##create the sequence of numbers comprising the path 
flow.path <- function(init.mov){
    f.p <- init.mov
    temp <- init.mov
    while(length(temp) != 0){
        f.b <- flow.select(temp) ##flow branch
        f.p <- c(f.p, f.b) ##flow path
        temp <- f.b
    }
    return(f.p)
}

##function returning a list with definitive path flows.  Re-analysing branches
##if the final layer has has more than 30 "moves"
final.flow.path <- function(mov, n.l){
    temp.path <- flow.path(mov)
    flag <- paste("l",n.l+1, sep = "")
    layer.size <- length(grep(flag,edges[temp.path,2]))
    if (layer.size <= 30){
        final.path <- list(temp.path)
    } else {
        final.path <- list()
        temp.mov.flows <- which(edges[,1] == edges[temp.path[1],2])
        for (j in temp.mov.flows) {
            ind.path <- flow.path(j)
            final.path <- append(final.path, list(c(i,ind.path)))
        }
    }    
    return(final.path)
}
