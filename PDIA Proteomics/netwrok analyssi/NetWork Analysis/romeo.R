# install packages
#install.packages("flextable")
#install.packages("GGally")
#install.packages("ggraph")
#install.packages("igraph")
#install.packages("Matrix")
#install.packages("network")
#install.packages("quanteda")
#install.packages("sna")
#install.packages("tidygraph")
#install.packages("tidyverse")
#install.packages("tm")
#install.packages("tibble")
#install.packages("quanteda.textplots")
# install klippy for copy-to-clipboard button in code chunks
#install.packages("remotes")
remotes::install_github("rlesur/klippy")


# activate packages
library(flextable)
library(GGally)
library(ggraph)
library(igraph)
library(Matrix)
library(network)
library(quanteda)
library(sna)
library(tidygraph)
library(tidyverse)
library(tm)
library(tibble)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# load data
rom <- read.delim("https://slcladal.github.io/data/romeo_tidy.txt", sep = "\t")

#Transform data to matrix
rome <- crossprod(table(rom[1:2]))
diag(rome) <- 0
romeo <- as.data.frame(rome)

#Tidy Networks generation using multiple packages
va <- romeo %>%
  dplyr::mutate(Persona = rownames(.),
                Occurrences = rowSums(.)) %>%
  dplyr::select(Persona, Occurrences) %>%
  dplyr::filter(!str_detect(Persona, "SCENE"))

# Edges
ed <- romeo %>%
  dplyr::mutate(from = rownames(.)) %>%
  tidyr::gather(to, Frequency, BALTHASAR:TYBALT) %>%
  dplyr::mutate(Frequency = ifelse(Frequency == 0, NA, Frequency))

# Create Graph
ig <- igraph::graph_from_data_frame(d=ed, vertices=va, directed = FALSE)

#label Nodes
tg <- tidygraph::as_tbl_graph(ig) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=name)

#plot the network
# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                alpha = .1) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 colour="gray10") +
  theme_graph(background = "white") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

#size by occurances

v.size <- V(tg)$Occurrences
# inspect
v.size

#size integration to graph

# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1) +
  geom_node_point(size=log(v.size)*2) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

#weights in the edges

E(tg)$weight <- E(tg)$Frequency
# inspect weights
head(E(tg)$weight, 10)

#integrate to graph
# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                aes(edge_width = weight,
                    alpha = weight)) +
  geom_node_point(size=log(v.size)*2) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

# define colors (by family)
mon <- c("ABRAM", "BALTHASAR", "BENVOLIO", "LADY MONTAGUE", "MONTAGUE", "ROMEO")
cap <- c("CAPULET", "CAPULET’S COUSIN", "FIRST SERVANT", "GREGORY", "JULIET", "LADY CAPULET", "NURSE", "PETER", "SAMPSON", "TYBALT")
oth <- c("APOTHECARY", "CHORUS", "FIRST CITIZEN", "FIRST MUSICIAN", "FIRST WATCH", "FRIAR JOHN" , "FRIAR LAWRENCE", "MERCUTIO", "PAGE", "PARIS", "PRINCE", "SECOND MUSICIAN", "SECOND SERVANT", "SECOND WATCH", "SERVANT", "THIRD MUSICIAN")
# create color vectors
Family <- dplyr::case_when(sapply(tg, "[")$nodes$name %in% mon ~ "MONTAGUE",
                           sapply(tg, "[")$nodes$name %in% cap ~ "CAPULET",
                           TRUE ~ "Other")
# inspect colors
Family

#integrate
# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                aes(edge_width = weight,
                    alpha = weight)) +
  geom_node_point(size=log(v.size)*2, 
                  aes(color=Family)) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

#defining colors based on family
# define colors (by family)
mon <- c("ABRAM", "BALTHASAR", "BENVOLIO", "LADY MONTAGUE", "MONTAGUE", "ROMEO")
cap <- c("CAPULET", "CAPULET’S COUSIN", "FIRST SERVANT", "GREGORY", "JULIET", "LADY CAPULET", "NURSE", "PETER", "SAMPSON", "TYBALT")
oth <- c("APOTHECARY", "CHORUS", "FIRST CITIZEN", "FIRST MUSICIAN", "FIRST WATCH", "FRIAR JOHN" , "FRIAR LAWRENCE", "MERCUTIO", "PAGE", "PARIS", "PRINCE", "SECOND MUSICIAN", "SECOND SERVANT", "SECOND WATCH", "SERVANT", "THIRD MUSICIAN")
# create color vectors
Family <- dplyr::case_when(sapply(tg, "[")$nodes$name %in% mon ~ "MONTAGUE",
                           sapply(tg, "[")$nodes$name %in% cap ~ "CAPULET",
                           TRUE ~ "Other")
# inspect colors
Family

#include in network analysis graph
# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
   ggraph(layout = "fr") +
   geom_edge_arc(colour= "gray50",
                  lineend = "round",
                 strength = .1,
                 aes(edge_width = weight,
                     alpha = weight)) +
   geom_node_point(size=log(v.size)*2, 
                   aes(color=Family)) +
   geom_node_text(aes(label = name), 
                  repel = TRUE, 
                  point.padding = unit(0.2, "lines"), 
                  size=sqrt(v.size), 
                  colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)