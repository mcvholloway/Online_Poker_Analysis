library(shinydashboard)
library(tidyverse)
library(tools)
library(plotly)
library(DT)

players <- readRDS('../data/players.RDS')
positions <- readRDS('../data/position.RDS')
player_positions <- readRDS('../data/player_position.RDS')
winning_hands <- readRDS('../data/showdown.RDS')

positions$Stakes <- factor(positions$Stakes, levels = c('Low', 'High'))
#positions$Position <- factor(positions$Position, levels = c('Small Blind', 'Big Blind',
#                                                            'Under the Gun', 'Hijack',
#                                                            'Cutoff', 'Button'))
