setwd("//neon/MOBACOM/CPI-DATEN/Sarah/Documents/Projects/Proteome/Codons")
options(stringsAsFactors=FALSE)

library(plyr)
library(pheatmap)
library(ggplot2)

##### TASK #####
## Calculate gene-specific codon frequency in all genes as number of codons per
## 1,000 codons per ORF. Get a genome-wide average of codon frequency and
## and compare codons on gene basis to this with Z-scores (hypergeometric
## distribution, cut-off p<0.01).
## Use only genes that were quantified in the proteome?

##### GOAL #####
## Identify which genes have sequences enriched with which codons. Then check
## and visualise up- and down-regulation of these genes in proteom and
## transcriptome results.


## Load codon counts from publication
counts <- read.table("counts_from_pub.tab", sep="\t", header=TRUE, row.names=1,
                     comment.char="", quote="")
# transpose data because my code was originally written for our own data
counts <- data.frame(t(counts))


## Frequency per 1,000 codons

# Take a single codon's frequency and the number of all codons in a given gene.
# Multiply the codon frequency with 1000, then divide by overall frequency to
# normalise the number to codon frequency per 1000 codons per gene.
# To be used inside NormFreq()
helper <- function(codon, codon_sum) {
  codon_norm <- (1000*codon)/codon_sum
}

# Take the codon counts for a given gene and sum them up to get the overall
# number of codons. Use sapply() on the codon counts and apply the helper()
# function to it to normalise the codon counts.
# To be applied to each column of the counts data.frame.
NormFreq <- function(freq) {
  codon_sum <- sum(freq)
  sapply(freq, function(x) helper(x, codon_sum))
}

# Normalise codon counts
counts.norm <- apply(counts, 2, function(x) NormFreq(x))
counts.norm <- data.frame(counts.norm)


## Hypergeometric test to get p-values for codon enrichment
q <- counts.norm[1, 1] # success / number of codon x in gene
m <- sum(counts.norm[1, ]) # sum of all codon x in genome
n <- sum(c(counts.norm, recursive=TRUE)) - m # all codons - codon x in genome
k <- sum(counts.norm[, 1]) # all codons in gene, always 1000 after normalisation

phyper(q, m, n, k) # probability
# BUT we want the probability that the codon is enriched (so q or higher than q)
# which means we have to use the following function call:
phyper(q-1, m, n, k, lower.tail=FALSE)
# http://mengnote.blogspot.de/2012/12/calculate-correct-hypergeometric-p.html

# Extract the codon frequency of codon x in given gene, then plug it into
# phyper(), together with m and n calculated in GetValues() below. The number of
# codons in the given gene (k) is always 1000 due to the normalisation step.
# To be applied to the frequency of codon x in every gene with sapply().
GetP <- function(codon, m, n) {
  q <- codon[[1]]
  phyper(q-1, m, n, 1000, lower.tail=FALSE)
}

# Take the frequencies of codon x and the number of codons found in the whole
# genome. Sum up codon x frequencies and substract that sum from the overall
# codon count to get m and n for phyper(). Then call GetP() for the last step.
# To be applied to every row of the normalised codon frequency data.frame.
GetValues <- function(codon_row, all_c) {
  m <- sum(codon_row) # sum of all codon x in genome
  n <- all_c - m      # all other codons
  sapply(codon_row, GetP, m=m, n=n)
}

# all_c is the population size: the number of codons found overall
counts.p <- apply(counts.norm, 1, GetValues, all_c=sum(c(counts.norm, recursive=TRUE)))
counts.p <- data.frame(t(counts.p))


## Filter with p<0.01
counts.enrich <- apply(counts.p, 2, function(x) as.numeric(x<=0.01))
counts.enrich <- data.frame(counts.enrich)
row.names(counts.enrich) <- row.names(counts.p)

# This results in a data.frame showing which codon is enriched in which gene.
# Enrichment is marked with 1s, and 0s are used otherwise.


## Up- and downregulation of proteins
proteome <- read.table("regulation_from_pub.tab", header=TRUE, sep="\t",
                       comment.char="", quote="")

## Filter codon enrichment results by accession numbers of interest
counts.filt <- counts.enrich[, names(counts.enrich) %in% proteome$accession]

## Filter proteome by accsession numbers in counts.filt
proteome.filt <- proteome[proteome$accession %in% names(counts.filt), ]

## Add up- and downregulation information
# When a codon is enriched in a gene, replace the 1 with the "up" or "down" from
# the proteome data.frame. Otherwise, keep the 0 (will be turned to character).
counts.reg <- apply(counts.filt, 1, function(x) ifelse(x==1, proteome.filt[proteome.filt$accession==names(x), "normal"], 0))
counts.reg <- data.frame(t(counts.reg))


## Sum up regulation
counts.up <- apply(counts.reg, 1, function(x) sum(x=="Up", na.rm=TRUE))
counts.down <- apply(counts.reg, 1, function(x) sum(x=="Down", na.rm=TRUE))
counts.all <- apply(counts.filt, 1, sum)
codons.proteins <- data.frame(cbind(counts.up, counts.down, counts.all))


## Get D/U ratio (down- versus upregulation)
codons.proteins <- transform(codons.proteins, ratio=counts.down/counts.up)


## Plot number of downregulated proteins on x and D/U ratio on y axis
plot(x=codons.proteins$counts.down, codons.proteins$ratio)
identify(x=codons.proteins$counts.down, codons.proteins$ratio,
         labels=row.names(codons.proteins)) # identify points


## Get percentage of down-regulated proteins to correct for group size
codons.proteins <- transform(codons.proteins, perc.down=(counts.down*100)/(counts.all))


## Plot percentage of downregulated proteins on x and D/U ratio on y axis
plot(x=codons.proteins$perc.down, codons.proteins$ratio)
identify(x=codons.proteins$counts.down, codons.proteins$ratio,
         labels=row.names(codons.proteins)) # identify points

##### THIS DOESN'T MAKE SENSE #####
# Some citations from the paper to think about:
# Figure 2:
# "Groups of genes were identified, each with a significantly high use of one of
# 61 codons. Within each codon group, the number or percentage of significantly
# altered proteins and the ratio of down- to up-regulated proteins (D/U) were
# calculated."
# "Z-scores were calculated as the difference between the frequency of each
# codon used by each transcript and the genome average, divided by the standard
# deviation, with the scores indicating whether a transcript is over- or under-
# represented with a specific codon compared to the genome average."


## Calculate Z-scores
counts.z <- scale(t(counts.norm), center=TRUE, scale=TRUE)
counts.z <- data.frame(t(counts.z))
colnames(counts.z) <- colnames(counts.norm)
# not exactly the same as in the publication??

## hypergeometric distribution?
counts.zp <- apply(counts.z, 1, GetValues, all_c=sum(c(counts.z, recursive=TRUE)))
# definitely not!

## p-values with pnorm()
# again use lower.tail=FALSE, since we want enriched codons only.
counts.zp <- apply(counts.z, 1, function(x) pnorm(x, lower.tail=FALSE))
counts.zp <- data.frame(t(counts.zp))
# Definitely not the same as in the publication.

## Nevertheless, plot that stuff! ##

## First, a heat map
pheatmap(t(counts.z), scale="none", show_rownames=FALSE, show_colnames=FALSE)
# hmm...

## Filter with p<0.01
counts.zenrich <- apply(counts.zp, 2, function(x) as.numeric(x<=0.01))
counts.zenrich <- data.frame(counts.zenrich)
row.names(counts.zenrich) <- row.names(counts.zp)

## Filter codon enrichment results by accession numbers of interest
counts.zfilt <- counts.zenrich[, names(counts.zenrich) %in% proteome$accession]

## Add up- and downregulation information
# When a codon is enriched in a gene, replace the 1 with the "up" or "down" from
# the proteome data.frame. Otherwise, keep the 0 (will be turned to character).
counts.zreg <- apply(counts.zfilt, 1, function(x) ifelse(x==1, proteome.filt[proteome.filt$accession==names(x), "normal"], 0))
counts.zreg <- data.frame(t(counts.zreg))


## Sum up regulation
counts.zup <- apply(counts.zreg, 1, function(x) sum(x=="Up", na.rm=TRUE))
counts.zdown <- apply(counts.zreg, 1, function(x) sum(x=="Down", na.rm=TRUE))
counts.zall <- apply(counts.zfilt, 1, sum)
codons.zproteins <- data.frame(cbind(counts.zup, counts.zdown, counts.zall))


## Get D/U ratio (down- versus upregulation)
codons.zproteins <- transform(codons.zproteins, ratio=counts.zdown/counts.zup)


## Plot number of downregulated proteins on x and D/U ratio on y axis
plot(x=codons.zproteins$counts.zdown, codons.zproteins$ratio)
text(codons.zproteins$counts.zdown, codons.zproteins$ratio,
     labels=row.names(codons.zproteins), cex=0.7, pos=3)
#identify(x=codons.zproteins$counts.zdown, codons.zproteins$ratio,
#         labels=row.names(codons.zproteins)) # identify points


## Get percentage of down-regulated proteins to correct for group size
codons.zproteins <- transform(codons.zproteins, perc.down=(counts.zdown*100)/(counts.zall))


## Plot percentage of downregulated proteins on x and D/U ratio on y axis
plot(x=codons.zproteins$perc.down, codons.zproteins$ratio)
identify(x=codons.zproteins$perc.down, codons.proteins$ratio,
         labels=row.names(codons.zproteins)) # identify points




## Plot with ggplot and colour codons of interest
ggplot(codons.zproteins, aes(counts.zdown, ratio)) + geom_point()

# add groups:  AGA  CAA   GAA   GGA   AAA   GGG   AGG
relevant <- c("R1", "Q1", "E1", "G1", "K1", "G3", "R2")
codons.zproteins$group <- ifelse(rownames(codons.zproteins) %in% relevant, "relevant", "irrelevant")

ggplot(codons.zproteins, aes(perc.down, ratio)) +
  geom_point(aes(colour=factor(group))) +
  coord_cartesian(ylim = c(-1, 50)) +
  geom_text(aes(label=ifelse(codons.zproteins$group=="relevant",
                             as.character(rownames(codons.zproteins)), ""),
                hjust=-0.5, just=0))

## And the hypergeometric distribution?
codons.proteins$group <- ifelse(rownames(codons.proteins) %in% relevant, "relevant", "irrelevant")

ggplot(codons.proteins, aes(perc.down, ratio)) +
  geom_point(aes(colour=factor(group))) +
  geom_text(aes(label=ifelse(codons.zproteins$group=="relevant",
                             as.character(rownames(codons.zproteins)), ""),
                hjust=-0.5, just=0))

##### reference #####
## Deng, W., Babu, I. R., Su, D., Yin, S., Begley, T. J., & Dedon, P. C. (2015).
## Trm9-Catalyzed tRNA Modifications Regulate Global Protein Expression by
## Codon-Biased Translation. PLoS Genetics, 11(12), 1-23.
## doi:10.1371/journal.pgen.1005706