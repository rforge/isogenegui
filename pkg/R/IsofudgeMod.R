`IsofudgeMod` <-
function(x,y, stat ,fudge){

  y <- as.matrix(y)
    ordx <- order(x) # compute once
    x <- x[ordx]
    y <- y[,ordx]  # reverse order (sorted two times)
    unx <- unique(x) # compute once
    ydf <- as.data.frame(t(y))
    y.m <- do.call("cbind", unclass(by(ydf, x, colMeans)))
   y.m.tot <- matrix(rep(rowMeans(y), length(x)), ncol = length(x))
    n.p <- table(x)
    n.g <- length(n.p)
    y.is.u <- t(apply(y.m, 1, function(x) pava(x, w = n.p)))
    y.is.d <- t(apply(y.m, 1, function(x) pava(x, w = n.p, decreasing = TRUE)))

    ###################################################
    
    rep.iso.d <- y.is.d[, rep(1:length(n.p),n.p)]
    rep.iso.u <- y.is.u[, rep(1:length(n.p),n.p)]
    
    y.m.all <- y.m[, rep(1:length(n.p), n.p)]
    
    ########################################################
    
    SST0 <- rowSums((y - rowMeans(y))^2)
    
    SSIS.u1 <- rowSums((rep.iso.u - y)^2)
    SSIS.d1 <- rowSums((rep.iso.d - y)^2)
    
    SST <- rowSums((y - y.m.all)^2)
    direction <- NULL
    direction <- ifelse(SSIS.u1 <= SSIS.d1, "u", "d")

    iso.u <- y.m
    SSIS.dir <- NULL
    
    iso.u[direction=="u",] <- y.is.u[direction=="u",]
    iso.u[direction=="d",] <- y.is.d[direction=="d",]
    
    SSIS.dir[direction=="u"] <- SSIS.u1[direction=="u"]
    SSIS.dir[direction=="d"] <- SSIS.d1[direction=="d"]
    
    E2.dif <- sqrt(SST0-SSIS.dir)
    E2.si2 <- sqrt(SST0)
    
    W.dif <- iso.u[,n.g] - y.m[,1]
    W.si2 <- sqrt(2*SST/(sum(n.p)-n.g)/(n.g-1)) 
    
    W.C.dif <- iso.u[,n.g] - iso.u[,1]
    W.C.si2 <- sqrt(2*SST/(sum(n.p)-n.g)/(n.g-1)) 
    
    M.dif <- iso.u[,n.g] - iso.u[,1]
    M.si2 <- sqrt(SSIS.dir/(sum(n.p)-n.g)) 
    
    I.dif <- iso.u[,n.g] - iso.u[,1]
    I.si2 <- sqrt(SSIS.dir/(sum(n.p) - apply(iso.u, 1, function(x) length(unique(x))))) 
    
    if (stat=="E2") fudgeres <- xfudge(E2.dif,E2.si2,fudge)
    if (stat=="Williams") fudgeres <- xfudge(W.dif, W.si2,fudge)
    if (stat=="Marcus") fudgeres  <- xfudge(W.C.dif,W.C.si2,fudge)
    if (stat=="M") fudgeres  <- xfudge(M.dif,M.si2,fudge)
     if (stat=="ModM") fudgeres <- xfudge(I.dif, I.si2,fudge)
  
   return(fudgeres )
  }

