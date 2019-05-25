# Load necessary dictionaries

# setwd("/home/sergiy/Documents/Work/Nutricia/Rework/Dictionaries")
setwd("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot")

dictPriceSegments = fread("PriceSegments.csv")
dictAcidified = read_xlsx("Acidified.xlsx")
dictScent = fread("dictScent.csv")
dictCereals = fread("dictCereals.csv")
dictCDC = fread("dictCDC.csv")
dictEC = fread("dictEC.csv")