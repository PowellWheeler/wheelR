NCFishes <- read.csv('~/Dropbox/NCWRC/fish.names.and.codes/official.NCWRC.fish.codes.csv', na='.', header=T)
save(NCFishes,file='~/git/wheelR/data/NCFishes.rda')

d9Sportfish <- NCFishes[NCFishes$d9.status == 'sportfish',]
d9Sportfish <- d9Sportfish[!(is.na(d9Sportfish$d9.status)),]
rownames(d9Sportfish) <- NULL
save(d9Sportfish, file='~/git/wheelR/data/d9Sportfish.rda')

d9SportfishVector <-as.vector(d9Sportfish$code)
names(d9SportfishVector) <- d9Sportfish$common.name
save(d9SportfishVector,file='~/git/wheelR/data/d9SportfishVector.rda')
