

library(genbankr)
tmp <- parseGenBank(file="NC_001477.gb")

tmp <- vector("list", 4)


library(IRanges)
ir <- IRanges(start=c(1, 3), end=c(5, 10))
height <- 1
bins <- disjointBins(ir)
plot.new()
plot.window(xlim=c(0,10), ylim=c(0,3))
rect(xleft = start(ir), ybottom =bins, xright = end(ir), ytop = bins+height)


sel_dg





library(reutils)

acc <- c("NC_001477", "NC_001474", "NC_001475", "NC_002640")
for(i in 1:length(acc)){
  ef <- efetch(acc[i], "nuccore", retmode = "text", rettype = "gb")  
  write(content(ef),file=paste(acc[i], ".gb", sep=""))
  Sys.sleep(1)
  cat(i, "/", length(acc), "\n");flush.console()
}

library(genbankr)

acc <- c("NC_001477", "NC_001474", "NC_001475", "NC_002640")
acc_files <- paste(acc, ".gb", sep="")


dg_list <- vector("list", length(acc_files))
#dg_list[[1]] <- parseGenBank(file = acc_files[1])
for(i in 1:length(acc_files)){
  dg_list[[i]] <- parseGenBank(file = acc_files[i])
  cat(i, "/", length(acc_files), "\n");flush.console()
}
names(dg_list) <- acc
dg_list
dg_list[[1]]
dg_list$NC_001477


library(IRanges)

ir <- IRanges(start=c(1,3,12,10), end=c(4,5,25,19))
ir
length(ir)
start(ir)
end(ir)
width(ir)
range(ir)

## plot 
height <- 1 
xlim = c(min(start(ir)), max(end(ir)))
bins = disjointBins(ir)  

plot.new()
plot.window(xlim, c(0, max(bins) * height))
ybottom = bins * height - height
rect(start(ir), ybottom, end(ir), ybottom + height, col = "blue")
axis(1)


plot.new()
plot.window(xlim=c(0,2), ylim=c(0,2))
rect(0, 0, 1, 1)

ir <- IRanges(start=c(1,3), end=c(4,5))



library(ggplot2)

height <- 1 
sep <- 0.5
xm <- c(min(start(ir))-sep, max(end(ir))+sep)
bins <- disjointBins(ir)  
ybottom <- bins*(sep+height) - height
df <- data.frame(ybottom = ybottom, xleft = start(ir) - sep, xright = end(ir) + sep, ytop = ybottom + height)
ggplot(df, aes(xmax = xright, xmin = xleft, ymax = ytop, ymin = ybottom)) + 
  geom_rect() 


plotRanges <- function(ir, height=1, sep=0.5){
  xm <- c(min(start(ir))-sep, max(end(ir))+sep)
  bins <- disjointBins(ir)  
  ybottom <- bins*(sep+height) - height
  df <- data.frame(ybottom = ybottom, 
                   xleft = start(ir) - sep, 
                   xright = end(ir) + sep, 
                   ytop = ybottom + height)
  ggplot(df, aes(xmax = xright, xmin = xleft, ymax = ytop, ymin = ybottom)) + 
    geom_rect() 
}



plotRanges(ir)

####===================

sel_dg <- dg_list[[1]]

n <- length(sel_dg$FEATURES)

#start_pos <- rep(0, n)
#end_pos <- rep(0, n)
start_pos <- c()
end_pos <- c()

for(i in 1:n){
  if(sel_dg$FEATURES[[i]]$type=="mat_peptide"){
    start_pos <- c(start_pos, sel_dg$FEATURES[[i]]$start)
    end_pos <- c(end_pos, sel_dg$FEATURES[[i]]$end)
  }
}

ir <- IRanges(start_pos, end_pos)
bins <- disjointBins(ir)
df <- data.frame(ybottom = bins, 
                 xleft = start(ir), 
                 xright = end(ir), 
                 ytop = bins + 1, 
                 group_no = LETTERS[1:length(ir)])
ggplot(df, aes(xmax = xright, 
               xmin = xleft, 
               ymax = ytop, 
               ymin = ybottom,
               fill = group_no)) + 
  geom_rect()


library(GenomicRanges)
library(ggbio)
library(ggplot2)


sel_dg <- dg_list[[1]]

sel_dg$FEATURES[[1]]$type
sel_dg$FEATURES[[1]]$start
sel_dg$FEATURES[[1]]$end
sel_dg$FEATURES[[1]]$strand
sel_dg$FEATURES[[1]]$product

n <- length(sel_dg$FEATURES)
start.pos <- rep(0, n)
end.pos <- rep(0, n)

for(i in 1:n){
  start.pos[i] <- sel_dg$FEATURES[[i]]$start
  end.pos[i] <- sel_dg$FEATURES[[i]]$end
}

ir <- IRanges(start=start.pos, end=end.pos)
plotRanges(ir)
coverage(ir)

x <- IRanges(start=c(-2L, 6L, 9L, -4L, 1L, 0L, -6L, 10L),
             width=c( 5L, 0L, 6L,  1L, 4L, 3L,  2L,  3L))
coverage(x)
coverage(x, shift=7)
plotRanges(x)


x <- Rle(values=c("a","b","c"), lengths=c(2,3,4)) 
x
as.character(x)
?GRanges


library(GenomicRanges)
gr <- GRanges(seqnames = Rle(c("chr1", "chr2"), c(2,3)),
              ranges = IRanges(start=1:5, end=6:10),
              strand = Rle(strand(c("-", "+", "+", "-")), c(1,1,2,1)),
              score = 1:5, 
              GC = seq(1, 0, length=5))



### =-=============

n <- length(sel_dg$FEATURES)
start.pos <- rep(0, n)
end.pos <- rep(0, n)
dstr <- rep("", n)

for(i in 1:n){
  start.pos[i] <- sel_dg$FEATURES[[i]]$start
  end.pos[i] <- sel_dg$FEATURES[[i]]$end
  dstr[i] <- sel_dg$FEATURES[[i]]$strand
}

snames <- Rle(c("Chr1"), n)
dstrand <- Rle(strand(dstr))
ir <- IRanges(start=start.pos, end=end.pos)

gr <- GRanges(seqnames=snames, ranges=ir, strand=dstrand)

ibrary(ggbio)
ggplot(gr) + geom_rect()


gr <- GRanges(seqnames=snames, ranges=ir, strand=dstrand, group=sample(LETTERS[1:4], length(ir), replace = T))

ggplot(gr) + geom_rect(aes(fill=group))
ggplot(gr) + layout_circle(geom="rect", aes(fill=group))


### =========================

N <- 100
## GRanges
gr <- GRanges(seqnames = sample(c("chr1", "chr2", "chr3"), size = N, replace = TRUE),
              IRanges(start = sample(1:300, size = N, replace = TRUE),width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, replace = TRUE),
              group = factor(sample(letters[1:4], N, TRUE))
)


ggplot(gr) + geom_rect(aes(fill=group, color=group))
ggplot(gr) + layout_circle(geom="rect", aes(fill=group, color=group))

