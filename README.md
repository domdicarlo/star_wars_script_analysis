# Star Wars Script Analysis

## About
In this fun analysis, I take Star Wars movie screenplays and look at word frequencies and sentiment across films in the original and prequel trilogies. I also look briefly at the one of the newer movies and make a comparison to the originals. I scrape the texts from the [the Internet Movie Script Database](https://www.imsdb.com/) in the R-Markdown. I have also added backup copies of these texts in the "./data" directory. 

## Navigation 
The R markdown file can be found in the same directory as this README, along with a pre-rendered markdown file. The scripts are found in [the data folder](./data).

### Links
**Analysis**

[R-Markdown of Script Analysis](star_wars_text_analysis.Rmd)

[Markdown of Script Analysis](star_wars_text_analysis.md) 

**Screenplays**

[Star Wars Episode 1](./data/sw_1_script.txt)

[Star Wars Episode 2](./data/sw_1_script.txt)

[Star Wars Episode 3](./data/sw_1_script.txt)

[Star Wars Episode 4](./data/sw_4_script.txt)

[Star Wars Episode 5](./data/sw_5_script.txt)

[Star Wars Episode 6](./data/sw_6_script.txt)

[Star Wars Episode 7](./data/sw_7_script.txt)

## Required Packages
* library(tidyverse) - tidy data functions

* library(rvest) - for working with html data on websites

* library(tidytext) - tidy text data functions

