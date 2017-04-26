

library(readxl)
source('functions.R')

outfile = 'result.html'

edge.list <- read_excel('data.xlsx')
id.list <- read_excel('data.xlsx', sheet = 2)

ch <- reformat(id.list$group, id.list$id, edge.list)

insert_data(ch, outfile)






d <- fread('C:/Users/stephen.fick/Dropbox/TRASE_SEI/blog_analyses/data/flows/RUN_24/Result_jg.csv')



k <- d[,list(v = sum(`SOY (TONS)`), p = sum( `SOY (TONS)`)/sum(d$`SOY (TONS)`) ), by = list(MUNICIPALITY, EXPORTER, BIOME ) ]

setorder(k, -v)

mc <- k[, list(v = sum(v)), by = EXPORTER]
setorder(mc, -v)
maxC <- mc$EXPORTER[1:5]

mm <- k[, list(v = sum(v)), by = MUNICIPALITY]
setorder(mm, -v)
maxM <- mm$MUNICIPALITY[1:100]
maxM <- maxM[-grep('UNKN', maxM)]

k$Municipality <- ifelse(k$MUNICIPALITY %in% maxM, k$MUNICIPALITY, paste0('OTHER MUNICIPALITY ',k$BIOME))

k$Exp <- ifelse(k$EXPORTER %in% maxC, k$EXPORTER, 'OTHER EXPORTER')

#E <- k[which(COUNTRY %in% maxC & MUNICIPALITY %in% maxM),]

setorder(k, -v)
# k[,mun := ifelse(v-v[11] < 0 | grepl('UNKNOWN',MUNICIPALITY), paste0('OTHER Municipality ', BIOME), MUNICIPALITY)  , list(BIOME)]

k[,mun := ifelse(MUNICIPALITY %in% maxM, MUNICIPALITY, "OTHERS")  , list(BIOME)]
k[,mun := ifelse(Municipality %in% maxM, Municipality, "OTHERS")  , list(BIOME)]

E <- k[ , list(v = sum(v)), by = list(Municipality,Exp) ]




# k[,mun := ifelse(v-v[11] < 0 | grepl('UNKNOWN',MUNICIPALITY), paste0('OTHER Municipality ', BIOME), MUNICIPALITY)  , list(BIOME)]

# E <- k[, list(v = sum(v)), by = list(mun, Country, BIOME)]
# setkey(E, BIOME, v)
#top 5 exporters

#el <- el[-which(el$v < exp(quantile(log(el$v), .3))), ] 

el <- as.data.frame(E[, list( Municipality,Exp, v)])

names(el) <- c('from', 'to', 'v')
#el <- el[order(el$from),]


ids <- as.character(unique(el$from))
groups <- rep('Municipalities', length(ids))
# groups <- E$BIOME[ match(ids, E$mun)]


ids2 <- as.character(unique(el$to))
#gr2 <- cx[match(ids2, names(cx))]
#ids2 <- ids2[order(gr2)]
# gr2 <- rep('Biomes', length(ids2))
gr2 <- rep('Soy Exporting Companies', length(ids2))


ids <- c( ids2,ids)
groups <- c( gr2,groups)		   
names(groups) <- NULL

ids <- ids[order(groups)]
groups <- groups[order(groups)]

i <- which(ids == 'OTHER EXPORTER')
ids <- c(ids[-i], ids[i])


ch <- chordify(groups, ids, el)

setwd('C:/projects/trase/code/chord/')

outfile <- 'mun_exporters2.html' 
