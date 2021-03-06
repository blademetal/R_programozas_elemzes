
library(tidyverse)
library(corrplot)
library(GGally)
library(plotly)
library(quantmod)
library(ggmap)
library(maps)
library(mapdata)
#devtools::install_github("renkun-ken/formattable")
library(formattable)
library(stringr)



tide_name <- function(df) {
  names(df) <- gsub(x = names(df),pattern = "\\ ",replacement = "_")
  return(df)
}



firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}



securities <- tide_name(read_csv("securities.csv"))
prices <- read_csv("prices-split-adjusted.csv")
fundamentals <- tide_name(read_csv("fundamentals.csv"))


sectors <- securities %>% 
            select(GICS_Sector) %>%
            count(GICS_Sector)

(sectors)



p_sectors <- ggplot(sectors) +
  geom_bar(mapping = aes(x = GICS_Sector,
                         y= n,
                         fill = GICS_Sector,
                         text = paste("Szektor: ", GICS_Sector, "\nC�gek sz�ma: ", n)),
           stat = "identity") +
  labs(title = "C�gek szektoronk�nti sz�ma", y = "A c�gek sz�ma az egyes szektorokban\n") +
  labs(fill='Szektorok neve (angolul)') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  theme(plot.margin=unit(c(1,1,1,1),"cm"))

ggplotly(p_sectors, tooltip = "text")



states <- map_data('state')
(states)



mainland_states <- unique(states[, 5])
mainland_states_up <- firstup(mainland_states)
(mainland_states_up)



states_securities <- securities %>%
                      mutate(states = strsplit(Address_of_Headquarters, ", ")) %>%
                      mutate(states = lapply(states, `[[`, 2)) %>%
                      mutate(states = str_to_lower(states)) %>%
                      filter(states %in% mainland_states)

(states_securities)



comp_per_states <- states_securities %>%
                      group_by(states) %>%
                      count(states)
colnames(comp_per_states) = c("region", "comp_n")

(comp_per_states)



states <- left_join(states, comp_per_states, by = c("region"))
states$comp_n[is.na(states$comp_n)] <- 0

states$region <- firstup(states$region)

(states)



ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


g <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = comp_n, group = group, text = paste("�llam neve: ", region,
                                              "<br>", "C�gek sz�ma: ", comp_n))) + 
  labs(title = 'C�gek sz�ma �llamonk�nt') +
  labs(fill = 'C�gek sz�ma') +
  coord_fixed(1.3) +
  scale_fill_gradient(low='#a99ffc', high='#9e1906') +
  ditch_the_axes

ggplotly(g, tooltip = "text")



first_appeared <- securities %>%
                    filter(!is.na(Date_first_added)) %>%
                    mutate(how_old_year = 2018 - as.numeric((Sys.Date() - as.Date(Date_first_added))/365)) %>%
                    mutate(how_old = as.numeric((Sys.Date() - as.Date(Date_first_added))/365)) %>%
                    mutate(how_old_year = formattable(how_old_year, digits = 0, format = "f")) %>%
                    mutate(how_old = formattable(how_old, digits = 2, format = "f")) %>%
                    arrange(how_old_year)



head(first_appeared, 20)



tail(first_appeared, 20)



first_appeared_compn <- first_appeared %>%
                          count(factor(how_old_year))


colnames(first_appeared_compn) = c("how_old_year", "comp_n")


apperances <- first_appeared_compn$comp_n


ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank()
)


g_how_old <- ggplot() +
                geom_bar(data = first_appeared_compn, aes(how_old_year, comp_n, 
                            text = paste('�vsz�m: ', how_old_year, '\nC�gek sz�ma: ', comp_n),
                            fill = comp_n), stat = 'identity') +
                ditch_the_axes +
                scale_y_continuous(name = "A c�gek sz�ma") +
                ggtitle("Frequency histogram of mean ozone") +
                scale_fill_gradient("A c�gek sz�ma \naz adott �vben", low = "blue", high = "red") +
                labs(title = 'C�gek tozsd�re vonul�sa �vente', x = '�vek',y = 'C�gek sz�ma')

ggplotly(g_how_old, tooltip = 'text')



bluechips <- fundamentals %>%
              group_by(Ticker_Symbol) %>%
              summarize(Total_Revenue_Mean = mean(Total_Revenue)) %>%
              filter(Total_Revenue_Mean>130000000000) %>%
              arrange(desc(Total_Revenue_Mean))

(bluechips)


bluechips_symbols <- c(bluechips$Ticker_Symbol)

(bluechips_symbols)



bluechip_names <- securities %>%
                  filter(Ticker_symbol %in% bluechips_symbols) %>%
                  select(Ticker_symbol, Security)

(bluechip_names)



get_Companies_by_Tickers <- function(ticker_list, comp_names) {
   return (prices %>% 
                    group_by(symbol) %>%
                    arrange(symbol) %>%
                    filter(match(symbol,ticker_list)>0) %>%
                    filter(as.Date(date) > as.Date("2012-01-01") & as.Date(date) <= as.Date("2015-12-31")) %>%
                    mutate(name = comp_names$Security[match(symbol, comp_names$Ticker_symbol)]) %>%
                    mutate(change = (close-close[1])) %>%
                    mutate(open = as.double(format(round(open, 2), nsmall = 2)),
                           close = as.double(format(round(close, 2), nsmall = 2)),
                           low = as.double(format(round(low, 2), nsmall = 2)),
                           high = as.double(format(round(high, 2), nsmall = 2)),
                           change = as.double(format(round(change, 2), nsmall = 2))))
}




bluechips_prices <- get_Companies_by_Tickers(bluechips_symbols, bluechip_names)
  

(bluechips_prices)



tooltip_text <- function(x) {
  return (paste('C�g neve: \n', x$name,
                                       '\n�rfolyam v�ltoz�sa: ', x$change, ' %',
                                       '\nNyit�si �r: ', x$open, ' $',
                                       '\nZ�r�si �r: ', x$close, ' $',
                                       '\nNapi cs�cs: ', x$high, ' $',
                                       '\nNapi m�lys�g: ',x$low, ' $',
                                       '\nForgalom: ', x$volume, ' db',
                                       '\nD�tum: ', x$date))
}


ditch_the_axes <- theme(
  panel.border = element_blank()
)


p <- ggplot(bluechips_prices, aes(date, change,
                                  text = tooltip_text(bluechips_prices))) +
  geom_line(mapping = aes(color = factor(name), group = 1)) + 
  ditch_the_axes + 
  labs(title = 'A Blue-Chippek �rfolyam�nak alakul�sa 2012. jan. 1.-e �s 2015. dec. 31. k�z�tt',
       x = '�vek',
       y = '2012. jan. 1.-tol sz�m�tott v�ltoz�s [ % ]',
       color = 'C�gek') +
  theme(plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))

ggplotly(p, tooltip = 'text')




largest_caps <- fundamentals %>%
                    group_by(Ticker_Symbol) %>%
                    summarise(Total_Assets = mean(Total_Assets)) %>%
                    arrange(desc(Total_Assets))
largest_caps <- largest_caps[1:10,]

(largest_caps)



largest_caps_symbols <- c(largest_caps$Ticker_Symbol)
(largest_caps_symbols)




largest_caps_names <- securities %>%
                  filter(Ticker_symbol %in% largest_caps_symbols) %>%
                  select(Ticker_symbol, Security)

(largest_caps_names)



largest_caps <- get_Companies_by_Tickers(largest_caps_symbols, largest_caps_names)

p_largest_cap <- ggplot(largest_caps, aes(date, change, text = tooltip_text(largest_caps))) +
          geom_line(mapping = aes(color = factor(name), group = 1)) +
          ditch_the_axes + 
          labs(title = 'A Large Cap-ek �rfolyam�nak alakul�sa 2012. jan. 1.-e �s 2015. dec. 31. k�z�tt',
               x = '�vek',
               y = '2012. jan. 1.-tol sz�m�tott v�ltoz�s [ % ]',
               color = 'C�gek') +
          theme(plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))

ggplotly(p_largest_cap, tooltip ='text')



smallest_caps <- fundamentals %>%
                  group_by(Ticker_Symbol) %>%
                  summarise(Total_Assets = mean(Total_Assets)) %>%
                  arrange(Total_Assets)
smallest_caps <- smallest_caps[1:12,]

(smallest_caps)



smallest_caps_symbols <- c(smallest_caps$Ticker_Symbol)

(smallest_caps_symbols)


smallest_caps_names <- securities %>%
                  filter(Ticker_symbol %in% smallest_caps_symbols) %>%
                  select(Ticker_symbol, Security)

(smallest_caps_names)



smallest_caps_symbols <- smallest_caps_symbols[!(smallest_caps_symbols %in% c("UA","UAA"))]
smallest_caps_names <- smallest_caps_names %>%
                          filter(Ticker_symbol %in% smallest_caps_symbols)

smallest_caps <- get_Companies_by_Tickers(smallest_caps_symbols, smallest_caps_names)

p_smallest_cap <- ggplot(smallest_caps, aes(date, change, text = tooltip_text(smallest_caps))) +
        geom_line(mapping = aes(color = factor(smallest_caps$name), group = 1)) +
        ditch_the_axes + 
        labs(title = 'A Small Cap-ek �rfolyam�nak alakul�sa 2012. jan. 1.-e �s 2015. dec. 31. k�z�tt',
               x = '�vek',
               y = '2012. jan. 1.-tol sz�m�tott v�ltoz�s [ % ]',
               color = 'C�gek') +
        theme(plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))
                    

ggplotly(p_smallest_cap, tooltip = 'text')



largest_caps_mean <- largest_caps %>%
                      group_by(date) %>%
                      summarize(change = mean(change))


smallest_caps_mean <- smallest_caps %>%
                        group_by(date) %>%
                        summarize(change = mean(change))


S_and_P500 <- prices %>% 
                    group_by(symbol) %>%
                    arrange(symbol) %>%
                    filter(as.Date(date) > as.Date("2012-01-01") & as.Date(date) <= as.Date("2015-12-31")) %>%
                    mutate(change = (close-close[1])) %>%
                    mutate(open = as.double(format(round(open, 2), nsmall = 2)),
                           close = as.double(format(round(close, 2), nsmall = 2)),
                           low = as.double(format(round(low, 2), nsmall = 2)),
                           high = as.double(format(round(high, 2), nsmall = 2)),
                           change = as.double(format(round(change, 2), nsmall = 2)))

S_and_P500_mean <- S_and_P500 %>%
                        group_by(date) %>%
                        summarize(change = mean(as.double(format(round(change, 2), nsmall = 2))))



tooltip_text_indices <- function(ind_Name, x) {
  return (paste('Index neve: ', ind_Name,
                              '\n�rfolyam v�ltoz�sa: ', as.double(format(round(x$change, 2), nsmall = 2)), ' %',
                              '\nD�tum: ', x$date))
}


indices <- ggplot() +
  geom_line(data = largest_caps_mean, aes(date,
                                  change,
                                  colour = "#f90a0a",
                                  text = tooltip_text_indices("Largest Caps", largest_caps_mean),
                                  group = 1), show.legend = FALSE) +
  geom_line(data = smallest_caps_mean, aes(date,
                                  change,
                                  colour = "#22d012",
                                  text = tooltip_text_indices("Smallest Caps", smallest_caps_mean),
                                  group = 1),show.legend = FALSE) +
  geom_line(data = S_and_P500_mean, aes(date,
                                  change,
                                  colour = "#1d2bec",
                                  text = tooltip_text_indices("S&P500", S_and_P500_mean), 
                                  group = 1), show.legend = FALSE) +
  theme(legend.position="none") +
  labs(title = 'R�szv�nycsomagok �sszehasonl�t�sa',
       x = '�vek',
       y = '2012. jan. 1.-tol sz�m�tott v�ltoz�s [ % ]') +
  theme(plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))

ggplotly(indices, tooltip = 'text')



















