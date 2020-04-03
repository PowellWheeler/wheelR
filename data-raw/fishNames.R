nc.fishes <- read.csv('~/Dropbox/NCWRC/fish.names.and.codes/official.NCWRC.fish.codes.csv', na='.', header=T)
save(nc.fishes,file='~/Dropbox/R/packages/wheelR/data/ncFishes.rda')

d9.sportfish <- nc.fishes[nc.fishes$d9.status == 'sportfish',]
d9.sportfish <- d9.sportfish[!(is.na(d9.sportfish$d9.status)),]
rownames(d9.sportfish) <- NULL
save(d9.sportfish,file='~/Dropbox/R/packages/wheelR/data/d9Sportfish.rda')

d9.sportfish.vector <-as.vector(d9.sportfish$code)
names(d9.sportfish.vector) <- d9.sportfish$common.name
save(d9.sportfish.vector,file='~/Dropbox/R/packages/wheelR/data/d9SportfishVector.rda')
