## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8
)

## ----setup--------------------------------------------------------------------
library(talkr)

## -----------------------------------------------------------------------------

data <- get_ifadv()
data <- init(data)

## ----report_stats-------------------------------------------------------------
report_stats(data)

## -----------------------------------------------------------------------------
plot_quality(data)


## -----------------------------------------------------------------------------
plot_quality(data, source = "/dutch2/DVA8K")

## ----geom_turn_demon1---------------------------------------------------------
library(ggplot2)
library(dplyr)

# we simplify participant names
conv <- data |>
  group_by(source) |>
  mutate(participant = as.character(factor(participant, labels=c("A","B"),ordered=T)))

# select first four conversations
these_sources <- unique(data$source)[1:4]

conv |>
  filter(end < 60000, # select first 60 seconds
                source %in% these_sources) |> # filter to keep only these conversations
  ggplot(aes(x = end, y = participant)) +
  geom_turn(aes(
    begin = begin,
    end = end)) +
  xlab("Time (ms)") +
  ylab("") +
  theme_turnPlot() +
  facet_wrap(~source) # let's facet to show the conversations side by side

## ----geom_turn_demo_3, fig.height=2.5-----------------------------------------

conv <- conv |> add_lines(line_duration = 60000)

conv |>
  filter(source == "/dutch2/DVA12S",
                line_id < 5) |> # limit to the first five lines
  ggplot(aes(x = line_end, y = line_participant)) +
  ggtitle("The first four minutes from DVA12S") +
  geom_turn(aes(
    begin = line_begin, # the begin and end aesthetics are now line-relative
    end = line_end)) +
  scale_y_reverse(breaks = seq(1, max(conv$line_id))) +  
  xlab("Time (ms)") +
  ylab("") +
  theme_turnPlot()

p <- last_plot()


## ----step10, fig.height=2.5---------------------------------------------------

p +
  ggtitle("Turns produced in overlap") +
  geom_turn(aes(
    begin = line_begin,
    end = line_end,
    fill=overlap,
    colour=overlap)) +
  scale_fill_discrete(na.translate=F) + # stop NA value from showing up in legend
  scale_colour_discrete(na.translate=F) # stop NA value from showing up in legend



## ----step11, fig.height=2.5---------------------------------------------------

p +
  ggtitle("Turns produced in overlap") +
  geom_turn(aes(
    begin = line_begin,
    end = line_end,
    fill=overlap,
    colour=overlap)) +
  scale_fill_discrete(na.translate=F) + # stop NA value from showing up in legend
  scale_colour_discrete(na.translate=F) # stop NA value from showing up in legend



## -----------------------------------------------------------------------------

conv_tokens <- conv |> tokenize() 



## -----------------------------------------------------------------------------

this_conv <- conv |>
  add_lines(line_duration=15000) |>
  filter(source == "/dutch2/DVA12S",
                line_id < 5) # let's look at the first three lines

these_tokens <- conv_tokens |>
  add_lines(line_duration=15000, time_columns = "relative_time") |>
  filter(source == "/dutch2/DVA12S",
                line_id < 5)

this_conv |>
  ggplot(aes(x = line_end, y = line_participant)) +
  ggtitle("Relative frequency of elements within turns") +
  scale_y_reverse() + # we reverse the axis because lines run top to bottom
  geom_turn(aes(
    begin = line_begin,
    end = line_end)) +
  geom_token(data=these_tokens,
             aes(x=line_relative_time,
                 size=frequency)) +
  xlab("Time (ms)") +
  ylab("") +
  theme_turnPlot()

p <- last_plot()


## -----------------------------------------------------------------------------

these_tokens_first <- these_tokens |>
  filter(order=="first",
                rank < 10)

p +
  ggtitle("Some frequent turn-initial elements") +
  geom_token(data=these_tokens_first,
             aes(x=line_relative_time),
             color="red") +
  ggrepel::geom_label_repel(data=these_tokens_first,
                            aes(x=line_relative_time,
                                label=token),
                            direction="y")


