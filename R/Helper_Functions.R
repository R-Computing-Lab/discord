#' Collection of Miscellaneous functions.
#'
#'@description
#'The following functions are ones I have found useful. Many of these (but not all) these functions are adapted from elsewhere. I have tried to credit the function with the source.
#roxygen2::roxygenise()
# detachAllData
## Modified detachAllData function from defunct epicalc package. Allows user to detach all datasets at once.
detachAllData<-function() {
  pos.to.detach <- (1:length(search()))[
    substring(search(),first = 1, last = 8)
    !="package:"&search()
    !=".GlobalEnv"&search()
    !="Autoloads"&search()
    !="CheckExEnv"&search()
    !="tools:rstudio"&search()
    !="TempEnv"]
  for (i in 1:length(pos.to.detach)) {
    if (length(pos.to.detach) > 0) {
      detach(pos = pos.to.detach[1])
      pos.to.detach <- (1:length(search()))[
        substring(search(),first = 1, last = 8)
        !="package:"&search()
        !=".GlobalEnv"&search()
        !="Autoloads"&search()
        !="CheckExEnv"&search()
        !="tools:rstudio"&search()
        !="TempEnv"]}}}
#
# ROUNDING FUNCTIONS
## Function rounds to three digits and defaults to na.rm = TRUE
## Max
Max <- function(x,digit=5,na.rm = TRUE) {
  round(max(x, na.rm=na.rm),digit)}

## Mean
Mean <- function(x,digit=5,na.rm = TRUE) {
  round(mean(x, na.rm=na.rm),digit)}

## Median
Median <- function(x,digit=5,na.rm = TRUE) {
  round(median(x, na.rm=na.rm),digit)}

## Min
Min <- function(x,digit=5,na.rm = TRUE) {
  round(min(x, na.rm=na.rm),digit)}

## RowMedians
RowMedians <-function(x,digit=5,na.rm = TRUE) {
  round(rowMedians(as.matrix(x), na.rm=na.rm),digit)}

## Sd
Sd <- function(x,digit=5,na.rm = TRUE) {
  round(sd(x, na.rm=na.rm),digit)}
#

## Sum
Sum <- function(x,digit=5,na.rm = TRUE) {
  round(sum(x, na.rm=na.rm),digit)}
#
# New Row
## Function adapted from http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
insertRow <- function(existingDF, newrow, r=length(existingDF[,1])) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}
# seed.alpha
## Function converts string into hex. Set "set.seed" to TRUE if you want the seed set. Set "keep.seed" to FALSE if you don"t want the seed value returned.
seed.alpha <- function(x,set.seed=FALSE,keep.seed=TRUE) {
  require("digest")
  hexval <- paste0("0x",digest(x,"crc32"))
  intval <- type.convert(hexval) %% .Machine$integer.max
  if(set.seed){
    set.seed(intval)
  }
  if(keep.seed){
    return(intval)
  }}
#
# substrRight
## Reverse Substring
substrRight <- function(x, n, fromend=0, end=nchar(x)-fromend){
  substr(x, end-n+1,end)}


# Function adapted from http://stackoverflow.com/questions/4094094/modifying-fonts-in-ggplot2
theme_min = function (size=10, font=NA, face='plain',
                      axisColor='#999999',
                      textColor='black'){
  element_text = function(...)

    ggplot2::element_text(#family=font, face=face,
      colour=textColor, size=size, ...)

  theme(
    axis.text.x = element_text(),
    axis.text.y = element_text(),
    axis.line = element_blank(),
    axis.ticks = element_line(colour=axisColor, size=0.25),

    legend.key = element_rect(fill = 'white', size = 0.5),
    legend.text = element_text(hjust=0),
    legend.title = element_text(hjust=0),

    panel.background = element_rect(fill = "white",colour = NA),
    panel.border = element_rect(fill = NA,colour = "grey50"),
    panel.grid.major = element_line(colour = "grey90", size = 0.2),
    panel.grid.minor = element_line(colour = "grey98", size = 0.5),
    plot.title = element_text(hjust=0),
    strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2),
    strip.text.x = element_text(hjust=0),
    strip.text.y = element_text(angle=-90)
  )}

# Mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Source: http://stackoverflow.com/questions/22870198/is-there-a-more-efficient-way-to-replace-null-with-na-in-a-list
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

## cor_1star
### Prints out clean correlation matrix. If you want to replace the upper triangle with the sample size, toggle include.n, whose default is FALSE.
cor_1star <- function(x,digit=3,sig=.05,include.n=FALSE){
  require(Hmisc)
  x <- as.matrix(x)
  R <- rcorr(x)$r
  p <- rcorr(x)$P
  ## truncate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), digit))[,-1]
  ## build a new matrix that includes the correlations with their appropriate stars
  Rnew <- R
  if(!is.na(sig)){
    ## define notions for significance levels; spacing is important.
    mystars <- ifelse(p < sig, "* ", " ")

    ## build a new matrix that includes the correlations with their appropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) }
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  Rnew <- as.matrix(Rnew)
  if(!include.n){
    ## remove upper triangle
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
  }
  if(include.n){
    #replace upper triangle with sample size
    require(psych)
    ct<-corr.test(x)
    ct<-as.matrix(ct$n)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ct[upper.tri(ct, diag = TRUE)]
    Rnew <- as.data.frame(Rnew)
    Rnew <- cbind(Rnew[1:length(Rnew)])#-1])
  }
  return(Rnew)
}
## Add significance test to Corrplot source:https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

efunc <- function(error){
  return(NA)
}
