library(dplyr)

ms <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_aug/MS_R_aug.csv')

# Determine number of unique lure-bait combinations
combos_july <- unique(ms[, c("JULY9_LURE", "JULY9_BAIT")])
combos_may21 <- unique(ms[, c("MAY21_LURE", "May21_BAIT")])
combos_may9 <- unique(ms[, c("MAY9_LURE", "MAY9_BAIT")])

# Determine the count of each combo
table(ms$JULY9_LURE, ms$JULY9_BAIT)


# Determine frequencies of each lure-bait combo
freq <- ms %>% count(JULY9_LURE, JULY9_BAIT, sort = TRUE)
freq_may21 <- ms %>% count(MAY21_LURE, May21_BAIT, sort = TRUE)
freq_may9 <- ms %>% count(MAY9_LURE, MAY9_BAIT, sort = TRUE)

# Remove blank row 
freq <- freq[-2,]
write.csv(freq, file = '/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_aug/combo_freq_july.csv')
