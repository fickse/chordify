library(readxl)
source('functions.R')

outfile = 'result.html'

edge.list <- read_excel('data.xlsx')
id.list <- read_excel('data.xlsx', sheet = 2)

ch <- reformat(id.list$group, id.list$id, edge.list)

insert_data(ch, outfile)
