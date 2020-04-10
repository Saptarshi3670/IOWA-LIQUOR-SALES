file <- '/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/Iowa_Liquor_Sales.csv'

master_file <- '/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/Master_Data_Stores_final_version.csv'


df <- read.csv(file, sep = ",", header = TRUE)


df_master <- read.csv(master_file, sep = ",", header = TRUE)
str(df)

class(df)
class(df_master)
df_master <- df_master[,-1]

df1 <- merge(x = df, y = df_master, by = 'Store.Number', all.y = TRUE)
colnames(df1)

summary(df1)

df1 <- df1[,-c(5:10)]

colnames(df1)

names(df1)[names(df1)=="Address.y"] <- "Address"
names(df1)[names(df1)=="Zip.Code.y"] <- "Zip.Code"
names(df1)[names(df1)=="City.y"] <- "City"
names(df1)[names(df1)=="County.y"] <- "County"

colnames(df1)
#Saving merged file to csv
merged_file <- "/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/IOWA LIQUOR SALES/IOWA_LIQUOR_CLEANED_MERGED_DATA.csv"

write.csv(x = df1, file = merged_file, quote = FALSE, row.names = FALSE)





