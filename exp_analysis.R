library(tools)
library(dplyr)
library(stringr)

file <- "/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/IOWA LIQUOR SALES/IOWA_LIQUOR_CLEANED_MERGED_DATA.csv"


df <- read.csv(file = file, sep = ",", header = TRUE)

summary(df)

caps <- function(x){
  string <- strsplit(as.character(x), " ")[[1]]
  paste0(toupper(substring(string, 1, 1)), tolower(substring(string, 2)), sep = " ", collapse = " ")
}


class(df)
str(df)

unique(df$Category.Name)

sum(is.na(df$Category.Name) | df$Category.Name == "")

sum(is.na(df$Category) | df$Category.Name == "")

head(df[df$Category.Name == "",])


df$Category.Name <- as.factor(sapply(df$Category.Name, caps))

df$Item.Description <- as.factor(sapply(df$Item.Description, caps))

df$Category.Name <- trimws(df$Category.Name)
df$Item.Description <- trimws(df$Item.Description)
table(df$Category.Name)
table(df$Item.Description)


head(df[,c('Category.Name', 'Item.Description')])

head(df[(df[,'Item.Description'] == "Stolichnaya  Premium  Vodka  80  Proof  (lv)"),c('Category.Name', 'Item.Description')])
unique(df[(df[,'Item.Description'] == "Ha  1792  Full  Proof"),c('Category.Name', 'Item.Description')])

df_map <- unique(df[df$Category.Name != "", c('Category.Name','Item.Description')])

#df[df[,'Category.Name'] == '','Category.Name'] <- df_map[df_map[,'Item.Description'] == df[,'Item.Description'],'Category.Name']

df_map$AchoholTypes <- ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Whisky|Whiskies*'), 'Whisky',
                              ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Vodka*'), 'Vodka',
                                     ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Brandies*'), 'Brandy',
                                            ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Tequila*'), 'Tequila',
                                                   ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Liqueurs*'), 'Liqueurs',
                                                          ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Rum*'), 'Rum',
                                                                 ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Schnapps*'), 'Schnapps',
                                                                        ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Cocktails*'), 'Cocktails',
                                                                               ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Scotch*'), 'Whisky',
                                                                                      ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Gins*'), 'Gin',
                                                                                             ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Sec*'), 'Liqueurs',
                                                                                                    ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Amaretto*'), 'Liqueurs',
                                                                                                           ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Bourbon*'), 'Whisky',NA)))))))))))))



df_map$AchoholTypes <- ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Whisky|Whiskies*'), 'Whisky', 
                              ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Tequila*'), 'Tequila', 
                                     ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Rum*'), 'Rum', 
                                            ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Scotch*'), 'Whisky', 
                                                   ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Gin*'), 'Gin', 
                                                          ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Vodka*'), 'Vodka', 
                                                                 ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Bourbon*'), 'Whisky', 
                                                                        ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Captain  Morgan*'), 'Rum', 
                                                                               ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Whiskey*'), 'Whisky', 
                                                                                      ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Brandy*'), 'Brandy', 
                                                                                             ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Tarantula  Azul*'), 'Tequila', 
                                                                                                    ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Schnapps*'), 'Schnapps', 
                                                                                                           ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Liqueur*'), 'Liqueur', 
                                                                                                                  ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Johnnie  Walker*'), 'Whisky', 
                                                                                                                         ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Everclear  Alcohol*'), 'Other', 
                                                                                                                                ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Smirnoff*'), 'Vodka', 
                                                                                                                                       ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Jack  Daniels*'), 'Whisky', 
                                                                                                                                              ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Baileys*'), 'Liqueur', 
                                                                                                                                                     ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Bacardi  8  W/2  Glasses*'), 'Rum', 
                                                                                                                                                            ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Moonshine*'), 'Whisky', 
                                                                                                                                                                   ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Barbancourt  5*'), 'Rum', 
                                                                                                                                                                          ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Mezcal*'), 'Mezcal', 
                                                                                                                                                                                 ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Jagermeister*'), 'Liqueur', 
                                                                                                                                                                                        ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Margarita*'), 'Cocktail', 
                                                                                                                                                                                               ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Midnight  Moon*'), 'Whisky', 
                                                                                                                                                                                                      ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Bacardi  Multi-flavor  Mini  6  Pack*'), 'Rum', 
                                                                                                                                                                                                             ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Bacardi  8yr  W/glass  &  Ice  Mold*'), 'Rum', 
                                                                                                                                                                                                                    ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Rye*'), 'Whisky', 
                                                                                                                                                                                                                           ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Rhum*'), 'Rum', 
                                                                                                                                                                                                                                  ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*Tortilla  Gold  Dss*'), 'Liqueur', 
                                                                                                                                                                                                                                         ifelse(str_detect(as.character(df_map[,'Item.Description']), '.*martini*'), 'Cocktail', df_map$AchoholTypes)))))))))))))))))))))))))))))))


#df_map[is.na(df_map$AchoholTypes), 'AchoholTypes'] <-'Others'



sum(str_detect(as.character(df$Category.Name), 'Special'))

#vector <- unlist(df_map[df_map$AchoholTypes == 'Others','Item.Description'], use.names=FALSE)
vector <- unlist(df_map[is.na(df_map$AchoholTypes),'Item.Description'], use.names=FALSE)
sum(df[,'Item.Description'] %in% vector)

#---------------need to change below code-------------# 
df_category_map <- df_map %>% 
  inner_join(df_map,  by = "Item.Description") %>% 
  mutate(category = .$Category.Name.y) %>% 
  select(category, Item.Description, AlchoholTypes)

df[df[,'Item.Description'] == 'Bombay Sapphire w/Glass',]

nrow(df %>% 
       inner_join(df_map,  by = "Item.Description") %>% 
       mutate(category = ifelse(.$Category.Name.x == "", .$Category.Name.y, df$Category.Name)))

#View(df)
