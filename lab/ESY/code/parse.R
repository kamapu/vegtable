################################### #
### Load the expert system file  ####
################################### #
source('./ParsingExpertFile.R')

parsing.result <- parse.classification.expert.file(expertfile)

aggs <- parsing.result$species.aggs
groups <- parsing.result$species.groups
groups.names <- substr(names(groups), 5, nchar(names(groups)))
membership.expressions <- unique(parsing.result$membership.expressions)
conditions <- parsing.result$group.defs
vegtype.formulas <- parsing.result$formulas
vegtype.priority <- parsing.result$membership.priority
vegtype.formula.names <- substr(names(vegtype.formulas), 12, nchar(names(parsing.result$formulas)))
vegtype.formula.names.short <- trim(substr(vegtype.formula.names, 1,5))
anyDuplicated(vegtype.formula.names.short)
# substr(names(groups)[grepl('##D', names(groups), fixed = TRUE)], 1, 3) <- '###'
#################################################### #
# logi2 = Evaluation of vegetation type formulas  ####
#################################################### #
# we replace the inner expressions with TRUE and FALSE
o <- order(nchar(membership.expressions), decreasing =TRUE)
vegtype.formulas.p <- stri_replace_all_fixed(vegtype.formulas, pattern = membership.expressions[o], replacement = paste("col", seq(1:length(membership.expressions)), sep="")[o], vectorize_all = FALSE)

# remove the angle brackets around expressions
vegtype.formulas.p <- gsub("<","",vegtype.formulas.p)
vegtype.formulas.p <- gsub(">","",vegtype.formulas.p)
# replace logical operators with R operators
vegtype.formulas.p <- gsub("AND","&",vegtype.formulas.p)
vegtype.formulas.p <- gsub("OR","|",vegtype.formulas.p)
vegtype.formulas.p <- gsub("NOT","&!",vegtype.formulas.p)

logexpr.formula <- sapply(vegtype.formulas.p, function(x) parse(text=x))
