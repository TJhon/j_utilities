librarian::shelf(gganimate, tidyverse, janitor)

tribble(
  ~anio, ~paises, ~medallas, ~n,
  2004, 'usa', 'oro', 12,
  2004, 'usa', 'bronce', 1,
  2004, 'usa', 'plata', 34,
  2004, 'uk', 'oro', 11,
  2004, 'uk', 'bronce', 8,
  2004, 'uk', 'plata', 24,
  # ----
  2005, 'uk', 'oro', 22,
  2005, 'uk', 'bronce', 32,
  2005, 'uk', 'plata', 36,
  2005, 'usa', 'oro', 31,
  2005, 'usa', 'bronce', 18,
  2005, 'usa', 'plata', 29,
) %>% 
  #filter(anio == 2005) %>% 
  mutate(
    anio = lubridate::as_date(anio) #%>% lubridate::year()
    , paises = fct_reorder(paises, n)
    , medallas = fct_relevel(medallas, 'oro', 'plata', 'bronce')
  ) %>% 
  ggplot() +
  aes(x = paises, y = n, fill = medallas) +
  geom_tile(aes())
  geom_col() +
  coord_flip() +
  facet_wrap(~medallas, ncol = 1) +
  scale_fill_manual(values = c('gold2', 'grey', 'red'))  +
  transition_time(anio) +
  labs(title = "{frame_time}")  +
  ease_aes('linear')

z <- bd_oli %>% 
  drop_na(year, team, medal) %>% 
  count(year, noc, medal) %>% 
  #separate_rows(noc, sep = "-") %>% 
  #separate_rows(noc, sep = "") %>% 
  #filter()
  #filter(team %in% t_me) %>% 
  mutate(
    year1 = paste0(year, "/01/01")
    , year = lubridate::year(year1)
    , noc = fct_reorder(noc, n)
  ) %>% 
  select(!year1) %>% 
  group_by(year, noc) %>% 
  mutate(total = sum(n)) %>% 
  select(!c(n, medal)) %>% 
  distinct() %>% 
  arrange(year, desc(total)) %>% 
  group_by(year) %>% 
  mutate(ranking = row_number()) %>% 
  filter(ranking < 10) %>% 
  ungroup() %>% 
  #filter(year == 2016) %>% 
  mutate(noc= fct_reorder(noc, total)) %>% 
  ggplot() +
  aes(noc, total) +
  geom_col(alpha = .9) +
  #geom_text(aes(y = 0,  label = noc), hjust = 1.4) +
  coord_flip() +
  labs(title = "{frame_time}") +
  transition_time(year) +
  ease_aes('cubic-in-out')
animate(z, nframes = 100, fps = 5, end_pause = 20)
