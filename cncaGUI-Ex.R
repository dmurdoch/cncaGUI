pkgname <- "cncaGUI"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('cncaGUI')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("cnca")
### * cnca

flush(stderr()); flush(stdout())

### Name: cnca
### Title: Canonical Non-symmetrical Correspondence Analysis in R
### Aliases: cnca
### Keywords: multivariate

### ** Examples

data(especies)
data(variables)
cnca(especies, variables)



cleanEx()
nameEx("especies")
### * especies

flush(stderr()); flush(stdout())

### Name: especies
### Title: Species data
### Aliases: especies
### Keywords: datasets

### ** Examples

data(especies)



cleanEx()
nameEx("variables")
### * variables

flush(stderr()); flush(stdout())

### Name: variables
### Title: Environmental variables data
### Aliases: variables
### Keywords: datasets

### ** Examples

data(variables)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
