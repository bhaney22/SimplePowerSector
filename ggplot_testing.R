
load("DF.results.Rda")

df <- DF.results %>% 
  # filter(#Eta.2==Eta.2.f[1], 
  #   # Eta.1==Eta.1.f[1] & 
  #     Eta.3==Eta.3.f[1], 
  #     Eta.4==Eta.4.f[1], 
  #     Res.1.price==Res.1.price.f[1], 
  #     Res.2.price==Res.2.price.f[1], 
  #        # Fin.1.price==Fin.1.price.f[1] &
  #     Fin.2.price==Fin.2.price.f[1], 
  #     #   Fin.1.Mkt.share==Fin.1.Mkt.share.f[1] &
  #     Fin.1.I.1.share==Fin.1.I.1.share.f[1], 
  #     Fin.1.I.2.share==Fin.1.I.2.share.f[1], 
  #     Fin.1.I.3.share==Fin.1.I.3.share.f[1], 
  #     Fin.2.I.1.share==Fin.2.I.1.share.f[1], 
  #     Fin.2.I.2.share==Fin.2.I.2.share.f[1], 
  #     Fin.2.I.3.share==Fin.2.I.3.share.f[1]) %>% 
  select(Fin.1.I.3.share, 
         Fin.1.I.4.share,
         Fin.1.I.5.share,
         Fin.1.I.6.share,
         Fin.2.I.3.share,
         Fin.2.I.4.share,
         Fin.2.I.5.share,
         Fin.2.I.6.share,
         Fin.1.Mkt.share, Fin.1.price, Eta.3, 
         alpha.phys, alpha.curr, GDP.curr, PRR.phys, PRR.curr) %>% 
  mutate(
    Fin.1.Mkt.share = Fin.1.Mkt.share %>% as.numeric, 
    Fin.1.price = Fin.1.price %>% as.numeric,
    Eta.31 = Eta.3 %>% as.numeric, 
    alpha.phys = alpha.phys %>% as.numeric,
    alpha.curr = alpha.curr %>% as.numeric, 
    GDP.curr = GDP.curr %>% as.numeric, 
    PRR.phys = PRR.phys %>% as.numeric, 
    PRR.curr = PRR.curr %>% as.numeric,
    # Set up some factors.
    Fin.1.price_factor = case_when(
      .data$Fin.1.price < 1 ~ "Low (0.876 $/W-yr)", 
      .data$Fin.1.price >= 1 ~ "High (1.752 $/W-yr)", 
      TRUE ~ NA_character_
    ),
    Eta.3_factor = case_when(
      .data$Eta.3 < 0.5 ~ "Low (1/3)",
      .data$Eta.3 >= 0.5 ~ "High (2/3)",
      TRUE ~ NA_character_
    ),
    Eta.3_factor = factor(Eta.3_factor, levels = c("Low (1/3)", "High (2/3)")),
    Resources=ifelse(Fin.1.I.3.share==0 & Fin.2.I.3.share==0 & 
                     Fin.1.I.4.share==0 & Fin.2.I.4.share==0, "NG Only",
              ifelse(Fin.1.I.5.share==0 & Fin.2.I.5.share==0 & 
                     Fin.1.I.6.share==0 & Fin.2.I.6.share==0,"Coal Only","Coal & NG")))   %>%  
  gather(key = "y.var", value = "y.val", 
         alpha.phys, alpha.curr, GDP.curr, PRR.phys, PRR.curr)

# Faceted graph
df %>% 
  ggplot(mapping = aes_string(x = "Fin.1.Mkt.share", y = "y.val", linetype = "Fin.1.price_factor")) +
  geom_line() + 
  
  facet_grid(y.var ~ Eta.3_factor, scales = "free_y") +
             
  scale_x_continuous(limits = c(0, 1)) +
  labs(x = "Residential electricity [fraction of final market]", y = NULL) # +
  # xy_theme()
