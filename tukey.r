switch.name <- function(name) {
    z <- strsplit(name,'-')
    paste(z[[1]][2:1],collapse='-')
}

##is.diff <- function(i,j,x,tuk.df,alpha){
##  if (i==j) return(FALSE)
##  name <- paste(names(x)[j],names(x)[i],sep='-')
##  tuk.df[name,"p adj"]<alpha
##}

## remplace la version précédente
is.diff <- function(i,j,x,tuk.df,alpha){
  if (i==j) return(FALSE)
  name <- paste(names(x)[j],names(x)[i],sep='-')
  ## teste dans quel sens la fonction TukeyHSD a retourné les 2 parties du nom de la comparaison
  ## permute les 2 parties si nécessaire
  if (!(name %in% row.names(tuk.df)))
      name <- switch.name(name)
  tuk.df[name,"p adj"]<alpha
}

groups <- function(x,tuk.df,alpha,hole.rm=FALSE){
  x <- sort(x)
  n <- length(x)
  m <- sapply(1:n,function(i) { j <- i:n ; max(j[sapply(j,function(k) !is.diff(i,k,x,tuk.df,alpha))]) })
  diff <- c(0,diff(m))
  if (any(diff<0))
    if (hole.rm){
      diff[diff>0] <- 0
      m <- m-diff
      cat("Warning: Holes have been removed.\n")
    }
    else cat("Warning: Groups contain holes. Use 'hole.rm=TRUE' to remove them.\n")
  group.min <- sapply(split(1:n,m),min)
  group.max <- as.numeric(names(group.min))
  ngroup <- length(group.min)
  group <- matrix(" ",nrow=n,ncol=ngroup)
  for (j in 1:ngroup) group[group.min[j]:group.max[j],j] <- letters[j]
  data.frame(x,groups=apply(group,1,paste,collapse=""))
}

TukeyHSD.group <- function(aov.res,which,conf.level=0.95,digits=2,hole.rm=FALSE){
  mt <- model.tables(aov.res,type="means",cterms=which)
  tuk <- TukeyHSD(aov.res,which,ordered=FALSE)
  text <- paste("groups(round(mt$table$",which,",",digits,"),tuk$",which,",1-conf.level,hole.rm=hole.rm)",sep='')
  eval(parse(text=text))
}

TukeyHSD.group.FUN <- function(aov.res,which,conf.level=0.95,digits=2,hole.rm=FALSE,FUN=NULL){
  mt <- model.tables(aov.res,type="means",cterms=which)
  tuk <- TukeyHSD(aov.res,which,ordered=FALSE)
  text <- paste("groups(round(mt$table$",which,",",digits,"),tuk$",which,",1-conf.level,hole.rm=hole.rm)",sep='')
  z <- eval(parse(text=text))
  if (!is.null(FUN))
      z$x <- round(FUN(z$x),digits)
  z
}
