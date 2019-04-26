# Perhaps Pharmacy needs to be deleted

library(data.table)
library(readxl)

# Source
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot/dictionaries.R")

setwd("/home/sergiy/Documents/Work/Nutricia/1/Data")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot/cleanDF.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot/addSegments.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot/add_CDC_EC_AC.R")


# Read raw data and join
df = fread("BF_Y13-15.csv")
df1 = fread("BF_Y16.csv")
df = rbindlist(list(df, df1), use.names = TRUE)

df1 = fread("BF_Y17-18.csv")
df = rbindlist(list(df, df1), use.names = TRUE)
rm(df1)

# Verification
str(df)
df[, .(.N, sum(Volume)), by = Ynb] # all is correct

# Check share of blank lines
df[, df[Brand == "", sum(Value)]*100/sum(Value), by = Ynb]
# sales of empty cells is big

df[Brand == "", .(Ynb, Comments), by = SKU] # coders should check empty cells

# select data to work with
df = df[Brand != "" & Value > 0 & Volume > 0]

# Clean the dataset, Pharmacy is deleted
df = cleanDF(df)

# Select data to save
df = df[, .(Pieces = sum(PIECES),
            Value = sum(Value),
            Volume = sum(Volume)),
        by = .(SKU, Ynb, Mnb, 
               Brand, SubBrand,
               Size, Age, Scent,
               PS0, PS2, PS3, PS,
               Company, Form, Additives,
               Channel, Region)]

# Add segments (Scent, Acidified, etc.)
df = addSegments(df)

# Add coefficients (calendar days correction, extrapolation, additionl correction)
df = add_CDC_EC_AC(df)

df[, `:=`(PiecesC = Pieces*CDC*EC*AC,
          ValueC = Value*CDC*EC*AC,
          VolumeC = Volume*CDC*EC*AC)]
# Delete unnecessary columns
#[, c("CDC", "EC") := NULL]

fwrite(df, "df.csv")
