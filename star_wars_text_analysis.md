Star Wars Script Analysis
================
Dominic DiCarlo
June 2, 2019

![Star Wars](./data/logo.png)

In this R Markdown script, I analyze the prequel and original trilogies in the Star Wars Saga. These were the trilogies created by George Lucas, with (in release order) the original trilogy (Episodes 4, 5, and 6) consisting of *A New Hope*, *Empire Strikes Back*, and *Return of the Jedi*, and the prequel trilogy of *The Phantom Menace*, *Attack of the Clones*, and *Revenge of the Sith* (Episodes 1, 2, and 3). Many fans who grew up with the original trilogy did not care much for the prequels, while fans of my generation are either blinded by nostalgia or see something redeeming in them. I adore all of the movies, but the prequels will always hold a special place in my heart as my introduction into the fandom.

I thought some textual analysis would be interesting to do here, to see if there are any patterns that can be seen in word frequency and sentiment throughout the trilogies. In the end, I also do some analysis on one of the new films not directed by Lucas, *The Force Awakens*.

To begin, I extract the Star Wars scripts from the Internet Movie Script Database (IMSDb). Each film webpage holds the script text in a single html cell. Doing this for Episode I looks like this:

``` r
# first, read the html from the website
sw_1_script <- read_html("https://www.imsdb.com/scripts/Star-Wars-The-Phantom-Menace.html") %>%
  # then, extract out the node with the script
  html_nodes(".scrtext pre") %>%
  # take just the text from the node
  html_text() 
```

After extracting the rest of the scripts, I can combine them all together into a tidy text data frame, sorting by movie title.

``` r
# assign some names to a char vector
sw_names <- c("The Phantom Menace", "Attack of the Clones",
              "Revenge of the Sith", "A New Hope", 
              "Empire Strikes Back", "Return of the Jedi") 

sw_scripts <- c(sw_1_script, sw_2_script, sw_3_script, sw_4_script,
                sw_5_script, sw_6_script) %>%
  # name each list element
  set_names(sw_names) %>%
  # convert each film to a data frame and merge into unify data frame
  map_df(as_tibble, .id = "film") %>%
  # convert film to a factor (this sorts/names the films)
  mutate(film = factor(film, levels = sw_names)) %>% 
  # here, we unnest every combination of two words
  unnest_tokens(word, value)
```

I am almost ready to do some analysis on the scripts. To start, I trim out stop words and then do a word frequency plot for each film.

``` r
sw_scripts %>%
 # delete stopwords
  anti_join(stop_words) %>%
  # these words are in the scripts a lot and dont say
  # anything interesting
  filter(word != "int", word != "ext", word != "cont'd") %>%
  # summarize count per word per film
  count(film, word) %>%
  # highest freq on top
  arrange(desc(n)) %>% 
  # work by film 
  group_by(film) %>% # 
  mutate(top = seq_along(word)) %>%
  # use top 20 frequent words
  filter(top <= 15) %>%
  # barplot step
  ggplot(aes(x = -top, y = n, fill = film)) + 
    geom_col(color = "black") +
    # print words in plot instead of as axis labels
    geom_text(aes(label = word), hjust = "left", nudge_y = 10, 
              size = 3) +
    labs(title = "Most frequent words in Star Wars",
         subtitle = "Lucas Era",
         x = NULL,
         y = "Word count") +
    facet_wrap( ~ film) +
    coord_flip() +
    theme(legend.position = "none",
          # rotate x text
          axis.text.x = element_text(angle = 45, hjust = 1),
          # remove tick marks and text on y-axis
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) 
```

![](star_wars_text_analysis_files/figure-markdown_github/word%20frequency%20plot-1.png)

While there are some problems here (words like Jar, Obi, and Wan are all parts of names and used together rather than alone), the word frequencies here are actually very revealing about the nature of these films. The key characters for each are the most obvious feature with Anakin, Obi-Wan, and Padme all in the top 15 in the prequels, and Luke, Han, and Leia in the original trilogy. Really cool is the fall of the usage of "Jar" in the prequels. The character Jar Jar Binks has a lot of screen time in the first prequel (*The Phantom Menace*), but his presence as a character diminishes in each additional film - we see "Jar" as the number one word in *The Phantom Menace*, number twelve word in *Attack of the Clones*, and missing from the top 15 in *Revenge of the Sith*. "Jedi" and "Droids" are also popular in each of the prequel films; which makes sense because there are indeed a lot of jedi and droids in the story. Each film has a few defining words that identify key places or features of the film, such as "Hoth" for *Empire Strikes Back* and "ewoks" for *Return of the Jedi*.

Let's do some sentiment analysis now and look at the emotional arcs of these films, to see if there are any interesting patterns to be seen. To do sentiment analysis, I will use the AFINN dictionary, which rates words between -5 and 5 in terms of their negativity and positivity in sentiment.

``` r
# divide up the scripts into 500 word chunks, we will 
# perform a sentiment analysis on each chunk
sw_scripts <- sw_scripts %>%
  # this adds an index to each column "rowid"
  rowid_to_column() %>%
  # here we are indexing on 500
  mutate(index_500 = floor(rowid/500))

# now, we can make the plot
sw_scripts %>%
  # match up words to the afinn dict 
  inner_join(get_sentiments("afinn")) %>%
  # group by the sets of 500
  group_by(film, index_500) %>%
  # compute the sum sentimental score for each set
  summarize(score = sum(score)) %>%
  # plot
  ggplot(aes(index_500, score, fill = film)) +
    geom_col() +
    facet_wrap(~ film, scales = "free_x") +
    labs(title = "Sentimental arcs of Star Wars Films",
         subtitle = "Lucas Era",
         x = "Point in script",
         y = "Emotional score") +
    theme(legend.position = "none",
          # remove tick marks and text on x-axis
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) 
```

![](star_wars_text_analysis_files/figure-markdown_github/sentiment%20analysis-1.png)

``` r
# score in table format
sw_scripts %>%
  # match up words to the afinn dict 
  inner_join(get_sentiments("afinn")) %>%
  # group by film and the sets of 500
  group_by(film) %>%
  summarize(net_sentiment = sum(score)) %>%
  ggplot(aes(x = film, y = net_sentiment, fill = film, 
             label = net_sentiment)) +
    geom_col() +
    # offset the text to the bottom of the graph 
    # this gives the score at the botoom of the graph
    geom_text(aes(y = 400)) +
    labs(title = "Net sentiment in Star Wars Films",
         subtitle = "Lucas Era",
         y = "Net sentiment",
         x = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
```

![](star_wars_text_analysis_files/figure-markdown_github/sentiment%20analysis-2.png)

The first two prequels are characterized by more positive emotional sentiment than the other films, and this is characteristic of their content. *The Phantom Menace* is mostly a more upbeat film, with lots of attempted jokes and gags with young Anakin and the floppy, goofy, (un?)lovable Jar Jar Binks comedic relief character. *Attack of the Clones* features a wider variety of settings, but also includes many positive scenes and situations, such as the protagonist Anakin's courting of Padme Amidala. *Revenge of the Sith* is definitely a darker film, with plenty of death and dismay, which is likely why we see more negative sentiment than the previous two prequels.

*A New Hope*'s significant amount of negative sentiment is interesting, because the movie isn't really a downer per se. However, most of the time the protagonists are on the run and in danger, and there isn't the same amount of blatant tom foolery and comic relief attempts we see in the prequels. The humor is more edgy when it's there, with lines such as "Will someone get this big walking carpet out of my way?". *Empire Strikes Back* and *Return of the Jedi* are fairly varied films, so their plots' sentiments also seem characteristic.

There are some caveats to this sort of sentiment analysis. These are screenplays, and all of the text is being evaluated, not just dialogues. Scene descriptions are probably roughly equal to the emotional effect of the actual light and colors of the scene, but this likely isn't always true. Likewise, this disregards the important effects of music, camera angles, and even body language in setting the emotional sentiment of the film. The 500 word binning is also an approximation, given we can't know whether 500 words is a consistent amount of passing screen time for each film (some sets of 500 words may be 2 minutes, while others might be 10). Lastly, the screenplays also contain deleted scenes not found in the film.

Now I go ahead and scrape the Episode 7 Script (*The Force Awakens*), one of the newer films not created by George Lucas. That movie has a plot that is very similar to *A New Hope*, and it would be interesting to see if the sentiment analysis is similar.

``` r
# first, read the html from the website
sw_7_script <- read_html("https://www.imsdb.com/scripts/Star-Wars-The-Force-Awakens.html") %>%
  # then, extract out the node with the script
  html_nodes(".scrtext pre") %>%
  # take just the text from the node
  html_text() 

# assign some names to a char vector
sw_names_4_7 <- c("A New Hope", "The Force Awakens")

sw_scripts_4_7 <- c(sw_4_script, sw_7_script) %>%
  # name each list element
  set_names(sw_names_4_7) %>%
  # convert each film to a data frame and merge into unify data frame
  map_df(as_tibble, .id = "film") %>%
  # convert film to a factor (this sorts/names the films)
  mutate(film = factor(film, levels = sw_names_4_7)) %>% 
  # here, we unnest every combination of two words
  unnest_tokens(word, value) %>%
  # this adds an index to each column "rowid"
  rowid_to_column() %>%
  # here we are indexing on 500
  mutate(index_500 = floor(rowid/500))

sw_scripts_4_7 %>%
  # match up words to the afinn dict 
  inner_join(get_sentiments("afinn")) %>%
  # group by film and the sets of 500
  group_by(film, index_500) %>%
  # compute the sum sentimental score for each set
  summarize(score = sum(score)) %>%
  ggplot(aes(index_500, score, fill = film)) +
    geom_col() +
    facet_wrap(~ film, scales = "free_x") +
    labs(title = "Sentimental arcs Star Wars Films",
         subtitle = "Cross-comparison",
         x = "Point in script",
         y = "Emotional score") +
    # add in some custom coloring to maintain color scheme we've been
    # using`
    scale_fill_manual(values = c("#05BDC5", "#a833ff")) +
    theme(legend.position = "none",
          # remove tick marks and text on x-axis
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) 
```

![](star_wars_text_analysis_files/figure-markdown_github/extract%20force%20awakens%20and%20analyze-1.png)

``` r
sw_scripts_4_7 %>%
  # match up words to the afinn dict 
  inner_join(get_sentiments("afinn")) %>%
  # group by film and the sets of 500
  group_by(film) %>%
  summarize(net_sentiment = sum(score)) %>%
  ggplot(aes(x = film, y = net_sentiment, fill = film, 
             label = net_sentiment)) +
    geom_col() +
    # offset the text to the bottom of the graph 
    # this gives the score at the botoom of the graph
    geom_text(aes(y = 400)) +
    labs(title = "Net sentiment in Star Wars Films",
         subtitle = "Cross-comparison",
         y = "Net sentiment",
         x = "") +
    # add in some custom coloring to maintain color scheme we've been
    # using`
    scale_fill_manual(values = c("#05BDC5", "#a833ff")) +
    theme(legend.position = "none")
```

![](star_wars_text_analysis_files/figure-markdown_github/extract%20force%20awakens%20and%20analyze-2.png)

While it's not perfect, these actually look somewhat similar! Both feature the most negative sentiment of all of the films. It'd be great to check if *The Last Jedi* (the sequel to *The Force Awakens*) also had similar negative sentiment levels, but an official screenplay has not been released as of now. This would help determine if *The Force Awakens*' negative sentiment is a result of similarity to *A New Hope* or if it is only part of a trend in the new trilogy.

This was a fun little first step into text analysis. It would be awesome to see what more powerful methods might show, such as some classical machine learning or Neural Network for NLP. I will have to set that on the to-do hopefully/eventually list!
