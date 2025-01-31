# This is a script to generate sankey plots showing trajectories of mental health 
# diseases.

library(tidyverse)
library(networkD3)


# Load and prepare data -----------------

# this is the data for all
data <- readxl::read_excel("data/df.xlsx", col_names = c("id", "group", "tra1","tra2", "n"))

# data stratified by sex ------
data_man <- readxl::read_excel("data/df_sex.xls", sheet = "man", 
                           col_names = c("id", "group", "tra1","tra2", "n"))

data_woman <- readxl::read_excel("data/df_sex.xls", sheet = "vrouw", 
                                col_names = c("id", "group", "tra1","tra2", "n"))


# remove the redundant info from the trajectory definition
data[101:700, 'tra2'] <- lapply(data[101:700, "tra2"], function(x) trimws(substring(x, 5)))
data_man[101:700, 'tra2'] <- lapply(data_man[101:700, "tra2"], function(x) trimws(substring(x, 5)))
data_woman[101:700, 'tra2'] <- lapply(data_woman[101:700, "tra2"], function(x) trimws(substring(x, 5)))


data_man$sex <- "men"
data_woman$sex <- "women"


# data <- bind_rows(data_man, data_woman)


# separate into column ---------
data <- data |>
    tidyr::separate(tra2, c("A", "B", "C", "D")) |>
    dplyr::mutate(A = paste0(A, "_A"),
                  B = paste0(B, "_B"), 
                  C = paste0(C, "_C"), 
                  D = paste0(D, "_D"))

# a function to convert the data into a structure ready fot NetworkD3
get_sankey_data <- function(df = data, gr = 1, baseline, 
                            by_sex = FALSE, by_age = FALSE) {
    
    columns_to_select <- c("id", "from", "to", "n")
    if (by_sex) columns_to_select <- c(columns_to_select, "sex")
    if (by_age) columns_to_select <- c(columns_to_select, "age")
    
    d1 <- df |>
        filter(group == gr) |>
        rename(from = A,
               to = B) |>
        dplyr::select(any_of(columns_to_select)) 
    
    
    d2 <- df |>
        filter(group == gr) |>
        rename(from = B,
               to = C) |>
        dplyr::select(any_of(columns_to_select)) 
    
    
    d3 <- df |>
        filter(group == gr) |>
        rename(from = C,
               to = D) |>
        dplyr::select(any_of(columns_to_select)) 
    
    columns_to_group <- c("from", "to")
    if (by_sex) columns_to_group <- c(columns_to_group, "sex")
    if (by_age) columns_to_group <- c(columns_to_group, "age")
    
    dat <- bind_rows(d1, d2, d3) |>
        group_by_at(columns_to_group) |>
        summarise(n = sum(n))
    
    
    nodes <- data.frame(name = c(as.character(dat$from), as.character(dat$to))) %>% 
        unique
    
    

    list(nodes = nodes, edges = dat)
}


# Plotting ---------

# Group 1 with no history of depression or anxiety -----------
none <- get_sankey_data(df = data, gr = 1)


# order nodes so that none is always on top and missing are on the bottom
none$nodes$order[grepl("none", none$nodes$name)] <- c(100, 200, 300, 400) 
none$nodes$order[grepl("DD",   none$nodes$name)] <- c(250, 350, 450)
none$nodes$order[grepl("AD",   none$nodes$name)] <- c(260, 360, 460)
none$nodes$order[grepl("Com",  none$nodes$name)] <- c(270, 370, 470)
none$nodes$order[grepl("M_",   none$nodes$name)] <- c(380, 480)



none$nodes <- arrange(none$nodes, order)

# ad add to the nodes
none$edges$IDsource <- match(none$edges$from, none$nodes$name) - 1 
none$edges$IDtarget <- match(none$edges$to,   none$nodes$name) - 1

# add descriptive labels
none$nodes <- none$nodes |>
    dplyr::mutate(name = case_when(
        grepl("none", name) ~ "No depressive or anxiety disorder",
        grepl("AD", name) ~ "Anxiety disorder",
        grepl("DD", name) ~ "Depressive disorder",
        grepl("Com", name) ~ "Both depressive and anxiety disorder",
        grepl("M_", name) ~ "Lost to follow-up"
    ))


# Group 2 with history of depression  -----------

ever_dd <- get_sankey_data(df = data, gr = 2)

# order nodes so that the index disease (DD) is always on top and missing are on
# the bottom
ever_dd$nodes$order[grepl("DD", ever_dd$nodes$name)] <- c(100, 200, 300, 400) 
ever_dd$nodes$order[grepl("AD",   ever_dd$nodes$name)] <- c(250, 350, 450)
ever_dd$nodes$order[grepl("Com",   ever_dd$nodes$name)] <- c(260, 360, 460)
ever_dd$nodes$order[grepl("none",  ever_dd$nodes$name)] <- c(270, 370, 470)
ever_dd$nodes$order[grepl("M_",   ever_dd$nodes$name)] <- c(380, 480)

ever_dd$nodes <- arrange(ever_dd$nodes, order)

# ad add to the nodes
ever_dd$edges$IDsource <- match(ever_dd$edges$from, ever_dd$nodes$name) - 1 
ever_dd$edges$IDtarget <- match(ever_dd$edges$to,   ever_dd$nodes$name) - 1

# add descriptive labels
ever_dd$nodes <- ever_dd$nodes |>
    dplyr::mutate(name = case_when(
        grepl("none", name) ~ "No depressive or anxiety disorder",
        grepl("AD", name) ~ "Anxiety disorder",
        grepl("DD_A", name) ~ "History of depressive disorder",
        grepl("DD_B|DD_C|DD_D", name) ~ "Depressive disorder",
        grepl("Com", name) ~ "Both depressive and anxiety disorder",
        grepl("M_", name) ~ "Lost to follow-up"
    ))




# Group 3 with history of anxiety  -----------

ever_ad <- get_sankey_data(df = data, gr = 3)

# order nodes so that none is always on top and missing are on the bottom
ever_ad$nodes$order[grepl("AD", ever_ad$nodes$name)] <- c(100, 200, 300, 400) 
ever_ad$nodes$order[grepl("DD",   ever_ad$nodes$name)] <- c(250, 350, 450)
ever_ad$nodes$order[grepl("Com",   ever_ad$nodes$name)] <- c(260, 360, 460)
ever_ad$nodes$order[grepl("none",  ever_ad$nodes$name)] <- c(270, 370, 470)
ever_ad$nodes$order[grepl("M_",   ever_ad$nodes$name)] <- c(380, 480)

ever_ad$nodes <- arrange(ever_ad$nodes, order)

# ad add to the nodes
ever_ad$edges$IDsource <- match(ever_ad$edges$from, ever_ad$nodes$name) - 1 
ever_ad$edges$IDtarget <- match(ever_ad$edges$to,   ever_ad$nodes$name) - 1

# add descriptive labels
ever_ad$nodes <- ever_ad$nodes |>
    dplyr::mutate(name = case_when(
        grepl("none", name) ~ "No depressive or anxiety disorder",
        grepl("AD_A", name) ~ "History of anxiety disorder",
        grepl("AD_B|AD_C|AD_D", name) ~ "Anxiety disorder",
        grepl("DD", name) ~ "Depressive disorder",
        grepl("Com", name) ~ "Both depressive and anxiety disorder",
        grepl("M_", name) ~ "Lost to follow-up"
    ))




# Group 4 with history of depression and anxiety  -----------

ever_ddad <- get_sankey_data(df = data, gr = 4)

# order nodes so that none is always on top and missing are on the bottom
ever_ddad$nodes$order[grepl("Com", ever_ddad$nodes$name)] <- c(100, 200, 300, 400) 
ever_ddad$nodes$order[grepl("DD",   ever_ddad$nodes$name)] <- c(250, 350, 450)
ever_ddad$nodes$order[grepl("AD",   ever_ddad$nodes$name)] <- c(260, 360, 460)
ever_ddad$nodes$order[grepl("none",  ever_ddad$nodes$name)] <- c(270, 370, 470)
ever_ddad$nodes$order[grepl("M_",   ever_ddad$nodes$name)] <- c(380, 480)

ever_ddad$nodes <- arrange(ever_ddad$nodes, order)

# ad add to the nodes
ever_ddad$edges$IDsource <- match(ever_ddad$edges$from, ever_ddad$nodes$name) - 1 
ever_ddad$edges$IDtarget <- match(ever_ddad$edges$to,   ever_ddad$nodes$name) - 1

# add descriptive labels
ever_ddad$nodes <- ever_ddad$nodes |>
    dplyr::mutate(name = case_when(
        grepl("none", name) ~ "No depressive or anxiety disorder",
        grepl("AD", name) ~ "Anxiety disorder",
        grepl("DD", name) ~ "Depressive disorder",
        grepl("Com_B|Com_C|Com_D", name) ~ "Both depressive and anxiety disorder",
        grepl("Com_A", name) ~ "History of both depressive and anxiety disorder",
        grepl("M_", name) ~ "Lost to follow-up"
    ))




# Group 5 with current anxiety  -----------

curr_ad <- get_sankey_data(df = data, gr = 6)

# order nodes so that none is always on top and missing are on the bottom
curr_ad$nodes$order[grepl("AD", curr_ad$nodes$name)] <- c(100, 200, 300, 400) 
curr_ad$nodes$order[grepl("DD",   curr_ad$nodes$name)] <- c(250, 350, 450)
curr_ad$nodes$order[grepl("Com",   curr_ad$nodes$name)] <- c(260, 360, 460)
curr_ad$nodes$order[grepl("none",  curr_ad$nodes$name)] <- c(270, 370, 470)
curr_ad$nodes$order[grepl("M_",   curr_ad$nodes$name)] <- c(380, 480)

curr_ad$nodes <- arrange(curr_ad$nodes, order)

# ad add to the nodes
curr_ad$edges$IDsource <- match(curr_ad$edges$from, curr_ad$nodes$name) - 1 
curr_ad$edges$IDtarget <- match(curr_ad$edges$to,   curr_ad$nodes$name) - 1

# add descriptive labels
curr_ad$nodes <- curr_ad$nodes |>
    dplyr::mutate(name = case_when(
        grepl("none", name) ~ "No depressive or anxiety disorder",
        grepl("AD", name) ~ "Anxiety disorder",
        grepl("DD", name) ~ "Depressive disorder",
        grepl("Com", name) ~ "Both depressive and anxiety disorder",
        grepl("M_", name) ~ "Lost to follow-up"
    ))


# Group 6 with current depression  -----------

curr_dd <- get_sankey_data(df = data, gr = 5)

# order nodes so that none is always on top and missing are on the bottom
curr_dd$nodes$order[grepl("DD", curr_dd$nodes$name)] <- c(100, 200, 300, 400) 
curr_dd$nodes$order[grepl("AD",   curr_dd$nodes$name)] <- c(250, 350, 450)
curr_dd$nodes$order[grepl("Com",   curr_dd$nodes$name)] <- c(260, 360, 460)
curr_dd$nodes$order[grepl("none",  curr_dd$nodes$name)] <- c(270, 370, 470)
curr_dd$nodes$order[grepl("M_",   curr_dd$nodes$name)] <- c(380, 480)

curr_dd$nodes <- arrange(curr_dd$nodes, order)

# ad add to the nodes
curr_dd$edges$IDsource <- match(curr_dd$edges$from, curr_dd$nodes$name) - 1 
curr_dd$edges$IDtarget <- match(curr_dd$edges$to,   curr_dd$nodes$name) - 1

# add descriptive labels
curr_dd$nodes <- curr_dd$nodes |>
    dplyr::mutate(name = case_when(
        grepl("none", name) ~ "No depressive or anxiety disorder",
        grepl("AD", name) ~ "Anxiety disorder",
        grepl("DD", name) ~ "Depressive disorder",
        grepl("Com", name) ~ "Both depressive and anxiety disorder",
        grepl("M_", name) ~ "Lost to follow-up"
    ))



# Group 7 with current depression and anxiety  -----------

curr_ddad <- get_sankey_data(df = data, gr = 7)

# order nodes so that none is always on top and missing are on the bottom
curr_ddad$nodes$order[grepl("Com", curr_ddad$nodes$name)] <- c(100, 200, 300, 400) 
curr_ddad$nodes$order[grepl("DD",   curr_ddad$nodes$name)] <- c(250, 350, 450)
curr_ddad$nodes$order[grepl("AD",   curr_ddad$nodes$name)] <- c(260, 360, 460)
curr_ddad$nodes$order[grepl("none",  curr_ddad$nodes$name)] <- c(270, 370, 470)
curr_ddad$nodes$order[grepl("M_",   curr_ddad$nodes$name)] <- c(380, 480)

curr_ddad$nodes <- arrange(curr_ddad$nodes, order)

# ad add to the nodes
curr_ddad$edges$IDsource <- match(curr_ddad$edges$from, curr_ddad$nodes$name) - 1 
curr_ddad$edges$IDtarget <- match(curr_ddad$edges$to,   curr_ddad$nodes$name) - 1

# add descriptive labels
curr_ddad$nodes <- curr_ddad$nodes |>
    dplyr::mutate(name = case_when(
        grepl("none", name) ~ "No depressive or anxiety disorder",
        grepl("AD", name) ~ "Anxiety disorder",
        grepl("DD", name) ~ "Depressive disorder",
        grepl("Com", name) ~ "Both depressive and anxiety disorder",
        grepl("M_", name) ~ "Lost to follow-up"
    ))


# Prepare plots ---------


# define colours ---------

# colours:
# none: #44BB99

# dd: #77AADD
# history of dd: #99DDFF

# ad: #BBCC33
# history of ad: #EEDD88 (#E3EBAD)

# both depressive and anxiety disorder: #CC6677
# history of both depressive and anxiety disorder: #EE99AA

# lost to follow-up: #DDDDDD

clr_none <- 'd3.scaleOrdinal() .domain(["none", "dd", "ad", "ddad", "lfu"]) 
.range(["#44BB99", "#77AADD", "#BBCC33", "#CC6677", "#DDDDDD"])'

clr_ever_dd <- 'd3.scaleOrdinal() .domain(["hdd", "dd", "ad", "ddad", "none", "lfu"]) 
.range(["#99DDFF", "#77AADD", "#BBCC33", "#CC6677", "#44BB99", "#DDDDDD"])'

clr_ever_ad <- 'd3.scaleOrdinal() .domain(["had", "ad", "dd", "ddad", "none", "lfu"]) 
.range(["#E3EBAD", "#BBCC33", "#77AADD", "#CC6677", "#44BB99", "#DDDDDD"])'

clr_ever_ddad <- 'd3.scaleOrdinal() .domain(["hddad", "ddad", "dd", "ad", "none", "lfu"]) 
.range(["#EE99AA", "#CC6677", "#77AADD", "#BBCC33", "#44BB99", "#DDDDDD"])'

clr_curr_dd <- 'd3.scaleOrdinal() .domain(["dd", "ad", "ddad", "none", "lfu"]) 
.range(["#77AADD", "#BBCC33",  "#CC6677", "#44BB99", "#DDDDDD"])'

clr_curr_ad <- 'd3.scaleOrdinal() .domain(["ad", "dd", "ddad", "none", "lfu"]) 
.range(["#BBCC33", "#77AADD", "#CC6677", "#44BB99", "#DDDDDD"])'

clr_curr_ddad <- 'd3.scaleOrdinal() .domain(["ddad", "dd", "ad", "none", "lfu"]) 
.range(["#CC6677", "#77AADD", "#BBCC33", "#44BB99", "#DDDDDD"])'



#  Final plots ----

plot_sankey <- function(df, cl, filename){

    networkD3::sankeyNetwork(Links = df$edges, Nodes = df$nodes, 
                              Source = "IDsource", Target = "IDtarget",
                              NodeID = "name", Value = "n", 
                              nodePadding = 13, colourScale = cl, 
                              iteration = 0, fontSize = 15, fontFamily = "Arial", 
                              nodeWidth = 25, margin = list(left = 260))
}

p1 <- plot_sankey(df = none, cl = clr_none, filename =            "trajectories_none.html")
p2 <- plot_sankey(df = curr_dd, cl = clr_curr_dd, filename =      "trajectories_curr_dd.html")
p3 <- plot_sankey(df = ever_dd, cl = clr_ever_dd, filename =      "trajectories_ever_dd.html")
p4 <- plot_sankey(df = curr_ad, cl = clr_curr_ad, filename =      "trajectories_curr_ad.html")
p5 <- plot_sankey(df = curr_ddad, cl = clr_curr_ddad, filename =  "trajectories_curr_ddad.html")
p6 <- plot_sankey(df = ever_ad, cl = clr_ever_ad, filename =      "trajectories_ever_ad.html")
p7 <- plot_sankey(df = ever_ddad, cl = clr_ever_ddad, filename =  "trajectories_ever_ddad.html")

# fix formatting and save
js_string <- 
    '
    function(el, x) {
        d3.selectAll(".node text").attr("text-anchor", "begin").attr("x", 30);
    }
    '

p1 <- htmlwidgets::onRender(p1, js_string) 
htmlwidgets::saveWidget(p1, file = here::here("data", "trajectories_none.html"))

p2 <- htmlwidgets::onRender(p2, js_string) 
htmlwidgets::saveWidget(p2, file = here::here("data", "trajectories_curr_dd.html"))

p3 <- htmlwidgets::onRender(p3, js_string) 
htmlwidgets::saveWidget(p3, file = here::here("data", "trajectories_ever_dd.html"))

p4 <- htmlwidgets::onRender(p4, js_string) 
htmlwidgets::saveWidget(p4, file = here::here("data", "trajectories_curr_ad.html"))

p5 <- htmlwidgets::onRender(p5, js_string) 
htmlwidgets::saveWidget(p5, file = here::here("data", "trajectories_curr_ddad.html"))

p6 <- htmlwidgets::onRender(p6, js_string) 
htmlwidgets::saveWidget(p6, file = here::here("data", "trajectories_ever_ad.html"))

p7 <- htmlwidgets::onRender(p7, js_string) 
htmlwidgets::saveWidget(p7, file = here::here("data", "trajectories_ever_ddad.html"))

