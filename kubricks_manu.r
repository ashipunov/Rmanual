### Read external commands
library(shipunov)

### Read input tables
## "as.is=TRUE" is to avoid conversion into factors
## Main table (species list), serves also as feature (characters) table:
char <- read.table("_kubricks_char.txt", h=TRUE, sep="\t", row.names=1, as.is=TRUE)
## Filter table to decipher character codes:
desc <- read.table("_kubricks_desc.txt", h=TRUE, sep="\t", as.is=TRUE)
## Feature tables:
pics <- read.table("_kubricks_pics.txt", sep="\t", as.is=TRUE)
subg <- read.table("_kubricks_subg.txt", sep="\t", as.is=TRUE)
## There could be many more feature tables, e.g., common names
## Filter table to replace all sorts of synonyms:
syno <- read.table("_kubricks_syno.txt", sep="\t", as.is=TRUE)

### Apply synonym table
## In this example, only "pics" needs conversion
pics[, 1] <- Recode(pics[, 1], syno[, 1], syno[, 2])

### Make literal descriptions
KUBR <- char # preallocate
for (n in names(char)) KUBR[, n] <- Recode4(char[, n], desc[, 1], desc[, n])
DESC <- apply(KUBR[, 1:3], 1, function(.x) paste0(.x, collapse=", "))
KUBR$DESC <- paste0("\\DD{", DESC, "}") # add LaTeX formatting

### Make species names
SPEC <- row.names(KUBR) # fake species list
KUBR$SPEC <- paste0("\\SP{", paste0("\\KK{", SPEC, "}"), "}")

### Make links to images
## There could be > 1 image per species so:
IMG0 <- aggregate(pics[, 2], list(pics[, 1]),
 function(.x) paste0("\\I{", .x, "}", collapse=" "))
IMG1 <- Recode4(SPEC, IMG0[, 1], IMG0[, 2])
write(KUBR$SPEC[IMG1 == ""], file="0no_pics.txt") # diagnostics!
KUBR$IMG <- paste0("\\II{", IMG1, "}")

### Make names of higher categories (here subgenera)
KUBR$SUBG <- Recode4(SPEC, subg[, 1], subg[, 2])
write(KUBR$SPEC[KUBR$SUBG == ""], file="0no_subg.txt") # diagnostics!
KUBR <- KUBR[order(KUBR$SUBG, KUBR$SPEC), ]
## We need these names only on the first occurrence:
SUBG1 <- KUBR$SUBG
SUBG2 <- c(tail(SUBG1, 1), head(SUBG1, -1))
KUBR$SUBG <- ifelse(SUBG1 != SUBG2,
 paste0("\\FF{", "Subgenus ", "\\KK{", SUBG1, "}}"), "")

### Make output table
write.table(file="0body", KUBR[, c("SUBG", "SPEC", "DESC", "IMG")],
 quote=FALSE, row.names=FALSE, col.names=FALSE, sep=" ", eol="\n\n")
