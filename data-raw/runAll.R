dataGeneratingScripts <- list.files('~/git/wheelR/data-raw', full.names = TRUE)

dataGeneratingScripts <- dataGeneratingScripts[dataGeneratingScripts != "/home/powell/git/wheelR/data-raw/runAll.R" ]

for (i in dataGeneratingScripts){
  print(i)
  source(i)
  }
