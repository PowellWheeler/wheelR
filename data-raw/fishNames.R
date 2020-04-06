ncFishes <- read.csv('~/Dropbox/NCWRC/fish.names.and.codes/official.NCWRC.fish.codes.csv', na='.', header=T)
save(ncFishes,file='~/git/wheelR/data/ncFishes.rda')

d9Sportfish <- ncFishes[ncFishes$d9.status == 'sportfish',]
d9Sportfish <- d9Sportfish[!(is.na(d9Sportfish$d9.status)),]
rownames(d9Sportfish) <- NULL
save(d9Sportfish, file='~/git/wheelR/data/d9Sportfish.rda')

d9Sportfish.vector <-as.vector(d9Sportfish$code)
names(d9Sportfish.vector) <- d9Sportfish$common.name
save(d9Sportfish.vector,file='~/git/wheelR/data/d9SportfishVector.rda')
