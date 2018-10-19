Gapminder-Exploration
================

``` r
options(warn = -1) # supresses warnings
suppressPackageStartupMessages(library(tidyverse))
```

    ## Note: the specification for S3 class "difftime" in package 'lubridate' seems equivalent to one from package 'hms': not turning on duplicate class definitions for this class.

``` r
suppressPackageStartupMessages(library(gapminder))
```

R Markdown
----------

``` r
library(gapminder)
library(tidyverse)
```

Factor Management
-----------------

We first check the number of levels for the country and continent variables before filtering and dropping unused factor levels. We also check the number of rows the dataset has.

``` r
gapminder %>%
  str() 
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

Now apply filtering and dropping of levels.

``` r
gapminder %>%
  filter(continent != "Oceania") %>% # remove rows with continent equal to Oceania
  droplevels() %>%
  str()
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

The country factor went from 142 levels to 140 levels, the continent factor went from 5 levels to 4 levels (since we drop Oceania), and the number of rows went from 1704 to 1680.

File I/O
--------

``` r
gapminderExpMax <- gapminder %>%
  group_by(continent) %>%
  summarize(maxLifeExp = max(lifeExp))

write_csv(gapminderExpMax, "ModifiedDataset")
modifiedDataset = read_csv("ModifiedDataset")
```

    ## Parsed with column specification:
    ## cols(
    ##   continent = col_character(),
    ##   maxLifeExp = col_double()
    ## )

``` r
modifiedDataset %>%
  str()
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    5 obs. of  2 variables:
    ##  $ continent : chr  "Africa" "Americas" "Asia" "Europe" ...
    ##  $ maxLifeExp: num  76.4 80.7 82.6 81.8 81.2
    ##  - attr(*, "spec")=List of 2
    ##   ..$ cols   :List of 2
    ##   .. ..$ continent : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ maxLifeExp: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr  "collector_guess" "collector"
    ##   ..- attr(*, "class")= chr "col_spec"

Visualization Design
--------------------

Writing Figures to a File
-------------------------

``` r
df <- gapminder %>%
  filter(year == 2007) # keep only the rows with year == 2007
plot <- ggplot(df, aes(lifeExp, gdpPercap)) +
  geom_point() +
  xlab("Life Expectancy") +
  ylab("GDP per capita") +
  ggtitle("GDP per capita versus Life Expectancy")
ggsave("GDP-LifeExp-plot.png", plot, width = 11, height = 8, dpi = "retina") # save plot to file
```

![Plot](https://github.com/STAT545-UBC-students/hw05-curtis77/blob/master/GDP-LifeExp-plot.png)
