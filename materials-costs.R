library(tidyverse)
library(ggthemes)

# create pie chart for textbook cost breakdowns - physical talk back board
price_range_1 = c('$0', '$1-$99', '$100-$199', '$200-$299', '$300-$399', '$400-$499', '$500-$599', '$600-$699', '$800+')
percentage_1 = c(22.35, 21.18, 27.06, 15.29, 7.06, 2.35, 0.59, 0.59, 3.53)
textbook_prices_physical = data.frame(price_range_1, percentage_1)
textbook_prices_physical
pie(textbook_prices_physical$percentage_1,
    labels=paste(textbook_prices_physical$price_range_1),
    col =rainbow(length(textbook_prices_physical$percentage_1)),
    main = "How much did UArizona students spend on textbooks for Fall 2021?")

# create pie chart for homework access code cost breakdowns - phsyical talk back board
price_range_2 = c('$0', '$1-$99', '$100-$199', '$200-$299', '$300-$399', '$400-$499', '$800+')
percentage_2 = c(36.23, 30.43, 20.09, 4.35, 0.72, 2.17, 0.72)
hw_access_code_prices_physical = data.frame(price_range_2, percentage_2)
hw_access_code_prices_physical
pie(hw_access_code_prices_physical$percentage_2,
    labels=paste(hw_access_code_prices_physical$price_range_2),
    col =rainbow(length(hw_access_code_prices_physical$percentage_2)),
    main = "How much did UArizona students spend on homework access codes for Fall 2021?")

# # create pie chart for textbook cost breakdowns - virtual talk back board
price_range_3 = c('$0', '$1-$99', '$100-$199', '$200-$299', '$300-$399')
percentage_3 = c(20, 35, 20, 20, 5)
textbook_prices_virtual = data.frame(price_range_3, percentage_3)
textbook_prices_virtual
pie(textbook_prices_virtual$percentage_3,
    labels=paste(textbook_prices_virtual$price_range_3),
    col =rainbow(length(textbook_prices_virtual$percentage_3)),
    main = "How much did UArizona students spend on textbook for Fall 2021?")

# # create pie chart for homework access code cost breakdowns - virtual talk back board
price_range_4 = c('$0', '$1-$99', '$100-$199')
percentage_4 = c(75, 15, 10)
homework_access_code_prices_virtual = data.frame(price_range_4, percentage_4)
homework_access_code_prices_virtual
pie(homework_access_code_prices_virtual$percentage_4,
    labels=paste(homework_access_code_prices_virtual$price_range_4),
    col =rainbow(length(homework_access_code_prices_virtual$percentage_4)),
    main = "How much did UArizona students spend on homework access codes for Fall 2021?")

# create bar graph for textbook cost breakdowns between grade levels - physical and virtual talk back board
textbook_mixed = data.frame(matrix(ncol=3))
colnames(textbook_mixed) = c('price_range', 'grade', 'count')
textbook_mixed
textbook_mixed = data.frame(list('price_range'=c('$0', '$1-$99', '$100-$199', '$200-$299', '$300-$399', '$400-$499', '$500-$599', '$600-$699', '$700-$799', '$800+',
                                                 '$0', '$1-$99', '$100-$199', '$200-$299', '$300-$399', '$400-$499', '$500-$599', '$600-$699', '$700-$799', '$800+',
                                                 '$0', '$1-$99', '$100-$199', '$200-$299', '$300-$399', '$400-$499', '$500-$599', '$600-$699', '$700-$799', '$800+'),
                                 'grade' = c('undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate',
                                             'graduate', 'graduate', 'graduate', 'graduate', 'graduate', 'graduate', 'graduate', 'graduate', 'graduate', 'graduate',
                                             'other', 'other', 'other', 'other', 'other', 'other', 'other', 'other', 'other', 'other'),
                                 'count' = c(29, 31, 40, 25, 12, 2, 0, 0, 0, 4,
                                             11, 5, 7, 8, 1, 1, 1, 1, 0, 1,
                                             2, 7, 3, 1, 0, 1, 0, 0, 0, 1)))

textbook_mixed %>% 
  group_by(price_range, grade) %>% 
  ggplot(aes(y=price_range,
             x=count,
             fill=grade,
             label=count)) +
  geom_col() +
  facet_wrap(~grade) +
  geom_label(fill = 'white') +
  labs(y='Price range',
       x='Number of students',
       title='How much did UArizona students spend on textbooks in Fall 2021?')

# create bar graph for homework access code cost breakdowns between grade levels - physical and virtual talk back board
hw_access_code_mixed = data.frame(matrix(ncol=3))
colnames(hw_access_code_mixed) = c('price_range', 'grade', 'count')
hw_access_code_mixed
hw_access_code_mixed = data.frame(list('price_range'=c('$0', '$1-$99', '$100-$199', '$200-$299', '$300-$399', '$400-$499', '$500-$599', '$600-$699', '$700-$799', '$800+',
                                                 '$0', '$1-$99', '$100-$199', '$200-$299', '$300-$399', '$400-$499', '$500-$599', '$600-$699', '$700-$799', '$800+',
                                                 '$0', '$1-$99', '$100-$199', '$200-$299', '$300-$399', '$400-$499', '$500-$599', '$600-$699', '$700-$799', '$800+'),
                                 'grade' = c('undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate', 'undergraduate',
                                             'graduate', 'graduate', 'graduate', 'graduate', 'graduate', 'graduate', 'graduate', 'graduate', 'graduate', 'graduate',
                                             'other', 'other', 'other', 'other', 'other', 'other', 'other', 'other', 'other', 'other'),
                                 'count' = c(43, 40, 34, 6, 1, 3, 0, 0, 0, 0,
                                             16, 4, 2, 0, 0, 0, 0, 0, 0, 1,
                                             6, 1, 2, 0, 0, 0, 0, 0, 0, 0)))

hw_access_code_mixed %>% 
  group_by(price_range, grade) %>% 
  ggplot(aes(y=price_range,
             x=count,
             fill=grade,
             label=count)) +
  geom_col() +
  facet_wrap(~grade) +
  geom_label(fill = 'white') +
  labs(y='Price range',
       x='Number of students',
       title='How much did UArizona students spend on homework access codes in Fall 2021?')
