pacman::p_load(tidyverse, rvest)
lula <- read_html("https://search.folha.uol.com.br/search?q=%22lula+erra%22&periodo=personalizado&sd=01%2F01%2F2023&ed=01%2F01%2F2024&site=todos")

Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

dateslula <- lula %>%  html_elements(".c-headline__dateline") %>%
  html_text() %>% 
  str_remove_all(.,"\n") %>% str_trim() %>% 
  str_remove_all(.,"º") %>% 
  str_split(pattern = "às", simplify = T) %>%
  data.frame() %>%  
  mutate(date_published = dmy(X1),
         yearmonth = dmy(paste(day(date_published), month(date_published), "2000", sep = "-")),
         politico = rep("Lula", NROW(.))) 
bolsonaro <- read_html("https://search.folha.uol.com.br/search?q=%22bolsonaro+erra%22&periodo=personalizado&sd=01%2F01%2F2019&ed=01%2F01%2F2020&site=todos")
datesbolsonaro <- bolsonaro %>%  html_elements(".c-headline__dateline") %>%
  html_text() %>% 
  str_remove_all(.,"\n") %>% str_trim() %>% 
  str_remove_all(.,"º") %>% 
  str_split(pattern = "às", simplify = T) %>%
  data.frame() %>%  
  mutate(date_published = dmy(X1),
         yearmonth = dmy(paste(day(date_published), month(date_published), "2000", sep = "-")),
         politico = rep("Bolsonaro", NROW(.))) 
# dateslula$yearmonth <- factor(dateslula$yearmonth, levels = unique(dateslula$yearmonth))


fakedates <- data.frame(dates = as.Date(c("2000-01-01", "2000-02-01", "2000-03-01", "2000-04-01", 
                                           "2000-05-01", "2000-06-01", "2000-07-01", "2000-08-01", 
                                           "2000-09-01", "2000-10-01", "2000-11-01", "2000-12-01")),
                        labels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", 
                                    "Junho", "Julho", "Agosto", "Setembro", "Outubro", 
                                    "Novembro", "Dezembro"))
geom_label(date=fakedates, aes(x=dates, text = labels))
rbind(dateslula,datesbolsonaro) %>% 
  mutate(y = case_when(politico == "Lula" ~ 1,TRUE ~1.1)) %>% 
  ggplot(aes(x = yearmonth, y = y))+
  geom_point(aes(color = politico, shape = politico),size = 4, position = position_dodge(.6))+
  # geom_line()+
  ylim(0.8,1.5)+
  scale_x_date(breaks = c(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2000-12-01"), by = "month")))+
  labs(x = "Mês", y = NULL,
       title = "Reportagens da Folha com menção a 'Nome do político + erra' no primeiro ano de mandato") +
  geom_vline(xintercept = as.numeric(c(as.Date("2000-01-01"),
                                       as.Date("2000-02-01"),
                                       as.Date("2000-03-01"),
                                       as.Date("2000-04-01"),
                                       as.Date("2000-05-01"),
                                       as.Date("2000-06-01"),
                                       as.Date("2000-07-01"),
                                       as.Date("2000-08-01"),
                                       as.Date("2000-09-01"),
                                       as.Date("2000-10-01"),
                                       as.Date("2000-11-01"),
                                       as.Date("2000-12-01"))),
             linetype = "dashed")+
  geom_label(data = fakedates, aes(x = dates, y = 1.4, label = labels),
             angle = 90, vjust = -0.5, fill = NA, label.size = NA)+ 
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank())
 