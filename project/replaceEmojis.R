library(stringr)
library(dplyr)
library(stringi)

# scrap the latest version of emojis
readLines("https://www.unicode.org/Public/emoji/latest/emoji-test.txt",
          encoding="UTF-8") %>%
  stri_subset_regex(pattern = "^[^#]") %>%
  stri_subset_regex(pattern = ".+") -> emoji

# extract the emoji character
emoji %>%
  stri_extract_all_regex(pattern = "# *.{1,2} *") %>%
  stri_replace_all_fixed(pattern = c("*", "#"),
                         replacement = "",
                         vectorize_all=FALSE) %>%
  stri_trim_both() -> emoji.chars


# extract the emoji description
emoji %>%
  stri_extract_all_regex(pattern = "#.*$") %>%
  stri_replace_all_regex(pattern = "^#.*?E\\d+\\.\\d+\\s+",
                         replacement = " ") -> emoji.descriptions

# I added a white space to deal with people using emojis right after a word
# without spacing. It can cause double white spaces when people properly put a
# space between a word an emoji. but it's not a big deal because those 
# additional spaces can be removed easily in the text cleaning part.
# tm_map(corpus, stripWhitespace) can be used for that matter from tm pkg


yourData = read.csv('') # whatever works for you. If it has more than 1 column
# for instance numerical variables and so, just be careful in targeting the 
# right column in the very next function


# replace emoji characters with their descriptions
cNOe = stri_replace_all_regex(yourData$textColumn, #replace with yours case
                              pattern = emoji.chars,
                              replacement = emoji.descriptions,
                              vectorize_all=FALSE)

yourData$textNoEmoji = cNOe

#now you have one more column that contains the text version of emoji

