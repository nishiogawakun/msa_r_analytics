install.packages("tidyr")
library(tidyr)

ds <- read_csv("wide.csv")
ds <- as.data.frame(ds)

df_pivoted <- pivot_longer(ds, cols = -c("Function","Level"), names_to = "Month")
df <- as.data.frame(df_pivoted)


library(xlsx)
write.xlsx(df, "/Volumes/GoogleDrive/My Drive/Exec HR Report.xlsx")

write(df, file = "/Volumes/GoogleDrive/My Drive/Exec HR Report", sep=",")
