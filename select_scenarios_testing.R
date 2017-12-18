load("DF.results.f.RDa") # Check the name. Loads an object named DF.results.f

names(DF.results.f)

three_cases <- DF.results.f %>% 
  filter(TFO == 100,
         Eta.1 < 0.5, Eta.2 == 0.4, Eta.3 == 0.4, Eta.4 == 0.5, 
         Res.1.price < 0.1, Res.2.price < 0.1,
         Fin.1.price < 1, Fin.2.price < 2, 
         Fin.1.Mkt.share == 0.4) %>% View
         # Fin.1.I.1.share == 0, Fin.2.I.1.share == 0,
         # Fin.1.I.2.share == 0, Fin.2.I.2.share == 0,
         # Fin.1.I.3.share == 0, Fin.2.I.3.share == 0) %>% View
