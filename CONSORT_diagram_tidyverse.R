## CONSORT diagram, using the tidyverse package {ggflowchart}


# packages ----------------------------------------------------------------

library(tidyverse)
library(ggflowchart)
library(patchwork)
library(glue)


# load data ---------------------------------------------------------------

## Load data
consort_data <- 
  read_csv(file = "X:/R1369/CSO FULL grant/Safe Haven Exports/CONSORT_diagram_data.csv") %>%
  mutate(
    note = case_when(
      note == "Excluded: cases with no hospital records prior to death before age 18" &
        N > 1000 ~ "Excluded: controls with no hospital records prior to death before age 18",
      TRUE ~ note
    )
  )  # correction: instead of 'cases' it should be 'controls' at the entry for exclusions with no records prior to death or age 18 with N>1000


# wrangle data ------------------------------------------------------------

## Using the same exclusion criterion for controls as cases, i.e. removing
## individuals with no hospital records prior to death, makes it sound like
## controls died prior to cases (which is not the case, controls were selected
## to be alive at the time of the cases death. A small number died before
## reaching age 18, but for most analyses this does not matter)

consort_data <-
  consort_data %>%
  mutate(
    note = if_else(
      note == "Excluded: Controls with no hospital records or only hospital record was related to death",
      true = "Excluded: Controls with no hospital records prior to matched case's death",
      false = note
    ),
    note = if_else(
      note == "Controls with hospital records prior to death",
      true = "Controls with hospital records prior to matched case's death",
      false = note
    )
  )

# construct flowchart -----------------------------------------------------

## The flowchart starts from cases/controls born in 1981. The cases & controls
## flowcharts don't link to each other so they'll go in parallel. 


## The data needs converting to a table of two columns, 'from' and 'to',
## describing the nodes and edges in the flowchart

# TODO: shorten the text
flowchart_data <-
  consort_data %>%
  slice(1:10) %>%
  mutate(
    node_text = glue("{note},\nN={scales::comma(N)}")
  )

fd <- 
  flowchart_data %>%
  mutate(node_text = str_wrap(node_text, width = 24)) %>%
  pull(node_text)
  
flowchart_nodes_cases <-
  tibble(
    from = c(fd[1], fd[1], fd[5], fd[5]),
    to = c(fd[5], fd[3], fd[9], fd[7])
  )

flowchart_nodes_controls <-
  tibble(
    from = c(fd[2], fd[2], fd[6], fd[6]),
    to = c(fd[6], fd[4], fd[10], fd[8])
  )

textsize = 2.5

flowchart_cases <- 
  ggflowchart(
    data = flowchart_nodes_cases,
    node_data = tribble(
      ~name, ~x, ~y,
      fd[1], 1, 5,
      fd[3], 2, 4,
      fd[5], 1, 3,
      fd[7], 2, 2,
      fd[9], 1, 1
    ),
    layout = "custom", 
    text_size = rel(textsize)  # in current version can't change the size of the boxes
  ) +
    labs(title = "Cases")

flowchart_controls <- 
  ggflowchart(
    data = flowchart_nodes_controls,
    node_data = tribble(
      ~name, ~x, ~y,
      fd[2], 1, 5,
      fd[4], 2, 4,
      fd[6], 1, 3,
      fd[8], 2, 2,
      fd[10], 1, 1
    ),
    layout = "custom", 
    text_size = rel(textsize)  # in current version can't change the size of the boxes
  ) +
    labs(title = "Controls")


# assemble final plot -----------------------------------------------------

fig_consort <-
  wrap_plots(
    flowchart_cases,
    flowchart_controls
  )


# save --------------------------------------------------------------------

fig_consort %>%
  ggsave(
    filename = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/consort_diagram.pdf",
    width = 8,
    height = 8,
    dpi = 300,
    bg = "white"
  )

fig_consort %>%
  ggsave(
    filename = "C:/Users/40011625/OneDrive - Edinburgh Napier University/CHASE Publications/Results paper 1/BJPsych/BJPsych Open revision/consort_diagram.png",
    width = 8,
    height = 8,
    dpi = 300,
    bg = "white"
  )
