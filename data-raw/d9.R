require(wheelR) #need the wr function

setwd('~/Dropbox/NCWRC/current.data/')
adger         <- read.csv('adger.csv',        header=T, na ='.')
bear          <- read.csv('bear.csv',         header=T, na ='.')
cedar.cliff   <- read.csv('cedar.cliff.csv',  header=T, na ='.')
chatuge       <- read.csv('chatuge.csv',      header=T, na ='.')
cheoah.lake   <- read.csv('cheoah.lake.csv',  header=T, na ='.')
emory         <- read.csv('emory.csv',        header=T, na ='.')
fontana       <- read.csv('fontana.csv',      header=T, na ='.')
glenville     <- read.csv('glenville.csv',    header=T, na ='.')
green.river   <- read.csv('green.river.csv',  header=T, na ='.')
hiwassee      <- read.csv('hiwassee.csv',     header=T, na ='.')
junaluska     <- read.csv('junaluska.csv',    header=T, na ='.')
nantahala     <- read.csv('nantahala.csv',    header=T, na ='.')
santeetlah    <- read.csv('santeetlah.csv',   header=T, na ='.')
waterville    <- read.csv('waterville.csv',   header=T, na ='.')
wolf          <- read.csv('wolf.csv',         header=T, na ='.')

# combine all datasets
d9 <-rbind(adger,bear, cedar.cliff, chatuge, cheoah.lake, emory, fontana, glenville,
green.river, hiwassee, junaluska, nantahala, santeetlah, waterville, wolf)

#make sure site is a factor
d9$site <- as.factor(d9$site)

# make tl and wt observations numeric rather than integer
d9$tl.mm <- as.numeric(d9$tl.mm)
d9$wt.g  <- as.numeric (d9$wt.g)

# make r-format date and interger year variables
d9$rdate <- as.Date(d9$date, format="%m/%d/%Y")
d9$year  <- as.integer(format(d9$rdate, "%Y"))

#calculate otolith.cohort
d9$otolith.cohort <- d9$year-d9$age

#calculate dna.age
d9$dna.age <- d9$year - d9$dna.cohort

# sort the data.frame
d9 <- d9[order(d9$waterbody, d9$date, d9$gear, d9$site, d9$species, -d9$tl.mm, -d9$wt.g),]

# reorder columns and drop unnecessary ones
# drop:  date (character)
# preferred order:  waterbody, r.date, year, gear, site, net.night,mesh, fish.id, ref.num,
#                   species, tl.mm, wt.g, sex, age, otolith.cohort, otc,
#                   dna.origin, dna.cohort, dna.age

d9 <- d9[,c(1,17,18,3,8,4,5,6,7,9,10,11,12,13,19,14,15,16,20)]

#renumber rows
rownames(d9) <-1:nrow(d9)

#apply the relative weight formula
d9$wr <- mapply(wr, d9$species, d9$tl.mm, d9$wt.g)

#output the dataset and remove the temp dataset
save(d9,file='~/Dropbox/R/packages/wheelR/data/d9.rda')
