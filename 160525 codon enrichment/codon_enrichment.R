setwd("//neon/MOBACOM/CPI-DATEN/Sarah/Documents/Projects/Proteome/Codons")
options(stringsAsFactors=FALSE)

library(Biostrings)
library(plyr)

##### PREP #####

genome <- read.table("Pseudomonas_aeruginosa_UCBPP-PA14_109.tab", skip=2,
                     header=TRUE, sep="\t", comment.char="", quote="")
colnames(genome)
genome.red <- genome[, c(2, 18)]


###### TESTS #####

test1 <- DNAString(genome.red$Nucleotide.Sequence[5])
test1
freq1 <- trinucleotideFrequency(test1, step=3)
sum(freq1)
freq1
length(freq1)

## TODO: take reverse sequences into account!!!

## freq per 1,000 codons
helper <- function(codon, codon_sum) {
  codon_norm <- (1000*codon)/codon_sum
}

NormFreq <- function(freq) {
  codon_sum <- sum(freq)
  sapply(freq, function(x) helper(x, codon_sum))
}

norm1 <- NormFreq(freq1)

test2 <- DNAString(genome.red$Nucleotide.Sequence[2])
test2
freq2 <- trinucleotideFrequency(test2, step=3)
sum(freq2)
freq2
length(freq2)

norm2 <- NormFreq(freq2)

## genome-wide average per codon
all_norms <- rbind(norm1, norm2)
all_means <- apply(all_norms, 2, mean)

## z-scores (??)
z <- scale(norm1, center=TRUE, scale=TRUE)
pnorm(z, lower.tail=FALSE)


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


## Convert all sequence strings to DNAStrings
sequences <- lapply(genome.red[,2], function(x) DNAString(x))
#pray that sequences are still in original order
names(sequences) <- genome.red$Locus.Tag


## Count codons in each gene
counts <- sapply(sequences, function(x) trinucleotideFrequency(x, step=3))
counts <- data.frame(counts)


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