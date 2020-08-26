## Downloading files and checking whether they are loaded:

if (!file.exists("exdata-data-NEI_data/summarySCC_PM25.rds") | !file.exists("exdata-data-NEI_data/Source_Classification_Code.rds")){
  if (!file.exists("exdata-data-NEI_data.zip")){
    temp = tempfile()
    download.file("exdata-data-NEI_data.zip", 
                  "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", temp)
    unzip(temp)
    unlink(temp)
  }
  unzip("exdata-data-NEI_data.zip")
  if (!exists(NEI) | !exists(SCC)){
    NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
    SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
  }
}

