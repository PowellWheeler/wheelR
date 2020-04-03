# This creates data sets and saves them as sysdata.rda in the /R
# sysdata.rda holds data that is needed by functions, but not otherwise needed by users.
# If this data were modified, it would break your function and the users should not modify it.
# They don't show-up on a data() call.

#ordinalLookup is a data.frame of calendar dates and their ordinal equivalent.
#wheelR:ordinalDate() uses this data.
ordinalLookup <- data.frame(monthDay = format(seq(as.Date("2001-01-01"), as.Date("2001-12-31"),1),format = "%m-%d"), ordinalDay=1:365, stringsAsFactors=FALSE)
ordinalLookup[366,1] <-'02-29' # Insert an incomplete row and R automatically adds a NAs for the missing elements, which is what I wanted.

# wsLookup is a list of species abbreviations and the variables needed to calculate their standard weight for relative weight calculations.
# This data is used by wheelR:wr()
# Uses standard NC Wildlife Resources Commission fish name abreviations
# Sources for the following are found in Blackwell et al. 2000. Reviews in Fisheres Science. 8:1-44.

wsLookup <- list(
  'LMB'  = list(a = -5.528, b = 3.273, min_tl = 150),
  'SMB'  = list(a = -5.329, b = 3.200, min_tl = 150),
  'SPB'  = list(a = -5.392, b = 3.215, min_tl = 100),
  'WY'   = list(a = -5.453, b = 3.180, min_tl = 150),
  'WB'   = list(a = -5.066, b = 3.081, min_tl = 115),
  'SB'   = list(a = -4.924, b = 3.007, min_tl = 150),
  'WBH'  = list(a = -5.201, b = 3.139, min_tl = 115),
  'YP'   = list(a = -5.386, b = 3.230, min_tl = 100),
  'RBT'  = list(a = -5.023, b = 3.024, min_tl = 120),
  'BNT'  = list(a = -4.867, b = 2.960, min_tl = 140),
  'BKT'  = list(a = -5.186, b = 3.103, min_tl = 120),
  'FC'   = list(a = -5.542, b = 3.230, min_tl = 130),
  'CC'   = list(a = -5.800, b = 3.294, min_tl = 70),
  'BCR'  = list(a = -5.618, b = 3.345, min_tl = 100),
  'WCR'  = list(a = -5.642, b = 3.332, min_tl = 100),
  'MK'   = list(a = -6.066, b = 3.325, min_tl = 380),
  'RE'   = list(a = -4.968, b = 3.119, min_tl = 70),
  'RB'   = list(a = -4.827, b = 3.074, min_tl = 80),
  'BG'   = list(a = -5.374, b = 3.316, min_tl = 80),
  'WM'   = list(a = -5.180, b = 3.241, min_tl = 80),
  'GS'   = list(a = -4.915, b = 3.101, min_tl = 60),
  'BKBH' = list(a = -4.974, b = 3.085, min_tl = 130),
  'BRBH' = list(a = -5.076, b = 3.105, min_tl = 130)
)

#sumFunLabels and sumFunShortLabels are vectors of the names of some descriptative statistics.
#These are often used with sumFun() and sumFunShort()
sumFunLabels <- c("N_Obs","N_Zero","N_NA","Sum","Mean","SD","SE","L95CI","U95CI","Median","Var","CV","Min","Max")
sumFunShortLabels <- c("N_Obs","N_NA","Mean","SD","SE")

save(sumFunLabels, sumFunShortLabels, wsLookup, ordinalLookup, file='~/Dropbox/R/packages/wheelR/R/sysdata.rda')
