# TODO:   Generating an example of cover convert data
# 
# Author: Miguel Alvarez
################################################################################

br_bl <- c("r","+","1","2","3","4","5")
br_bl <- list(
        value=factor(br_bl, levels=br_bl),
        conversion=c(0,1,1,5,25,50,75,100)
)

b_bbds <- c("r","+","1","2m","2a","2b","3","4","5")
b_bbds <- list(
        value=factor(b_bbds, levels=b_bbds),
        conversion=c(0,1,1,5,5,15,25,50,75,100)
)

ordin. <- paste(c(1:9))
ordin. <- list(
        value=factor(ordin., levels=ordin.),
        conversion=c(0,1,1,5,5,15,25,50,75,100)
)

braun_blanquet <- new("coverconvert")
braun_blanquet$br_bl <- br_bl
braun_blanquet$b_bbds <- b_bbds

save(braun_blanquet,
        file="M:/WorkspaceEclipse/vegtable/data/braun_blanquet.rda")
