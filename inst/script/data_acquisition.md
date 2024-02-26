

# urls.rda

Here, I will describe how I created the file `urls.rda`, which contains
URLs to files on FigShare with all duplicate pairs and genes. Data are
available in the following pages:

1.  [Ensembl
    Fungi](https://figshare.com/articles/dataset/doubletroubledb_data_-_Ensembl_Fungi/25222052)
2.  [Ensembl
    Metazoa](https://figshare.com/articles/dataset/doubletroubledb_data_-_Ensembl_Metazoa/25222061)
3.  [Ensembl
    Plants](https://figshare.com/articles/dataset/doubletroubledb_data_-_Ensembl_Plants/25222067)
4.  [Ensembl
    Protists](https://figshare.com/articles/dataset/doubletroubledb_data_-_Ensembl_Protists/25222070)
5.  [Ensembl](https://figshare.com/articles/dataset/doubletroubledb_data_-_Ensembl/25222076)

``` r
library(here)
library(tidyverse)
library(rvest)
library(httr)

set.seed(123) # for reproducibility
options(timeout = 1e10)
```

I opened each page on Google Chrome and did the following:

1.  Click on the “Next page” arrow button to see all the files (14
    pages, thumbnail view).

2.  Once all pages have been shown, the Javascript-created HTML file
    will contain data on all files. Then, we open Developer Tools, go to
    the Console tab, and run the following code:

<!-- -->

    copy(document.documentElement.outerHTML);

1.  This code will copy the content of the HTML page in the clipboard.
    Then, we can paste the text in the clipboard to an empty file with
    an `.html` extension (e.g., `figshare_fungi.html`).

Then, we can read the file and scrape the relevant information.

``` r
# Define function to get a data frame of URL, filename, and species
scrape_figshare <- function(html_path) {
    
    page <- read_html(html_path)
    file_divs <- page |>
        html_elements("div.Oom-6") 
    
    # Create a a vector of file names
    file_names <- unlist(lapply(file_divs, function(x) {
        
        fn <- x |>
            html_elements("span") |>
            html_attr("title")
        fn <- fn[!is.na(fn)]
        
        return(fn)
    }))
    
    # Create a vector of URLs
    file_urls <- unlist(lapply(file_divs, function(x) {
        
        furl <- x |>
            html_elements("a") |>
            html_attr("href")
        furl <- furl[!is.na(furl)]
        
        return(furl)
    }))
    
    # Combine results in a data frame
    url_df <- data.frame(
        url = file_urls,
        filename = file_names
    ) |>
        dplyr::filter(endsWith(filename, ".zip")) |>
        mutate(species = str_replace_all(filename, ".zip", ""))
    
    return(url_df)
}



# Read files
fungi <- scrape_figshare("~/Downloads/figshare_fungi.html") |> 
    mutate(ensembl = "Fungi")

plants <- scrape_figshare("~/Downloads/figshare_plants.html") |> 
    mutate(ensembl = "Plants")

metazoa <- scrape_figshare("~/Downloads/figshare_metazoa.html") |> 
    mutate(ensembl = "Metazoa")

vertebrates <- scrape_figshare("~/Downloads/figshare_vertebrates.html") |> 
    mutate(ensembl = "Vertebrates")

protists <- scrape_figshare("~/Downloads/figshare_protists.html") |> 
    mutate(ensembl = "Protists")

# Create object
urls <- rbind(fungi, plants, protists, metazoa, vertebrates)

save(
    urls, compress = "xz",
    file = here::here("data", "urls.rda")
)
```

## Session info

This document was created under the following conditions:

    ─ Session info ───────────────────────────────────────────────────────────────
     setting  value
     version  R version 4.3.2 (2023-10-31)
     os       Ubuntu 22.04.3 LTS
     system   x86_64, linux-gnu
     ui       X11
     language (EN)
     collate  en_US.UTF-8
     ctype    en_US.UTF-8
     tz       Europe/Brussels
     date     2024-02-26
     pandoc   3.1.1 @ /usr/lib/rstudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

    ─ Packages ───────────────────────────────────────────────────────────────────
     package     * version date (UTC) lib source
     cli           3.6.2   2023-12-11 [1] CRAN (R 4.3.2)
     colorspace    2.1-0   2023-01-23 [1] CRAN (R 4.3.2)
     digest        0.6.34  2024-01-11 [1] CRAN (R 4.3.2)
     dplyr       * 1.1.4   2023-11-17 [1] CRAN (R 4.3.2)
     evaluate      0.23    2023-11-01 [1] CRAN (R 4.3.2)
     fansi         1.0.6   2023-12-08 [1] CRAN (R 4.3.2)
     fastmap       1.1.1   2023-02-24 [1] CRAN (R 4.3.2)
     forcats     * 1.0.0   2023-01-29 [1] CRAN (R 4.3.2)
     generics      0.1.3   2022-07-05 [1] CRAN (R 4.3.2)
     ggplot2     * 3.4.4   2023-10-12 [1] CRAN (R 4.3.2)
     glue          1.7.0   2024-01-09 [1] CRAN (R 4.3.2)
     gtable        0.3.4   2023-08-21 [1] CRAN (R 4.3.2)
     here        * 1.0.1   2020-12-13 [1] CRAN (R 4.3.2)
     hms           1.1.3   2023-03-21 [1] CRAN (R 4.3.2)
     htmltools     0.5.7   2023-11-03 [1] CRAN (R 4.3.2)
     httr        * 1.4.7   2023-08-15 [1] CRAN (R 4.3.2)
     jsonlite      1.8.8   2023-12-04 [1] CRAN (R 4.3.2)
     knitr         1.45    2023-10-30 [1] CRAN (R 4.3.2)
     lifecycle     1.0.4   2023-11-07 [1] CRAN (R 4.3.2)
     lubridate   * 1.9.3   2023-09-27 [1] CRAN (R 4.3.2)
     magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.3.2)
     munsell       0.5.0   2018-06-12 [1] CRAN (R 4.3.2)
     pillar        1.9.0   2023-03-22 [1] CRAN (R 4.3.2)
     pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.3.2)
     purrr       * 1.0.2   2023-08-10 [1] CRAN (R 4.3.2)
     R6            2.5.1   2021-08-19 [1] CRAN (R 4.3.2)
     readr       * 2.1.5   2024-01-10 [1] CRAN (R 4.3.2)
     rlang         1.1.3   2024-01-10 [1] CRAN (R 4.3.2)
     rmarkdown     2.25    2023-09-18 [1] CRAN (R 4.3.2)
     rprojroot     2.0.4   2023-11-05 [1] CRAN (R 4.3.2)
     rstudioapi    0.15.0  2023-07-07 [1] CRAN (R 4.3.2)
     rvest       * 1.0.4   2024-02-12 [1] CRAN (R 4.3.2)
     scales        1.3.0   2023-11-28 [1] CRAN (R 4.3.2)
     sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.3.2)
     stringi       1.8.3   2023-12-11 [1] CRAN (R 4.3.2)
     stringr     * 1.5.1   2023-11-14 [1] CRAN (R 4.3.2)
     tibble      * 3.2.1   2023-03-20 [1] CRAN (R 4.3.2)
     tidyr       * 1.3.1   2024-01-24 [1] CRAN (R 4.3.2)
     tidyselect    1.2.0   2022-10-10 [1] CRAN (R 4.3.2)
     tidyverse   * 2.0.0   2023-02-22 [1] CRAN (R 4.3.2)
     timechange    0.3.0   2024-01-18 [1] CRAN (R 4.3.2)
     tzdb          0.4.0   2023-05-12 [1] CRAN (R 4.3.2)
     utf8          1.2.4   2023-10-22 [1] CRAN (R 4.3.2)
     vctrs         0.6.5   2023-12-01 [1] CRAN (R 4.3.2)
     withr         3.0.0   2024-01-16 [1] CRAN (R 4.3.2)
     xfun          0.42    2024-02-08 [1] CRAN (R 4.3.2)
     xml2          1.3.6   2023-12-04 [1] CRAN (R 4.3.2)
     yaml          2.3.8   2023-12-11 [1] CRAN (R 4.3.2)

     [1] /home/faalm/R/x86_64-pc-linux-gnu-library/4.3
     [2] /usr/local/lib/R/site-library
     [3] /usr/lib/R/site-library
     [4] /usr/lib/R/library

    ──────────────────────────────────────────────────────────────────────────────

## References
