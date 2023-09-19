setwd('/media/steppe/hdd/Barneby_Lives-dev/scripts')

usda <- read.csv('../taxonomic_data_raw/USDA_plants.csv', na.strings = "") %>% 
  select(Symbol, Synonym.Symbol, Scientific.Name = Scientific.Name.with.Author)

synonyms <- drop_na(usda, Synonym.Symbol) %>% 
  select(-Symbol, Symbol = Synonym.Symbol)

clean <- filter(usda, is.na(Synonym.Symbol)) %>% 
  select(-Synonym.Symbol)

gen_clean <- clean %>% 
  filter(str_detect(Symbol, '[A-Z]{5}') & 
           str_detect(Scientific.Name, ' var. | ssp. ', negate = TRUE)) %>% 
  mutate(Scientific.Name = str_remove(Scientific.Name, " .*$"))

gen_syn <- synonyms %>% 
  filter(str_detect(Symbol, '[A-Z]{5}') & 
           str_detect(Scientific.Name, ' var. | ssp. ', negate = TRUE)) %>% 
  mutate(Scientific.Name = str_remove(Scientific.Name, " .*$"))

genera_codes <- bind_rows(gen_clean, gen_syn)

species <- bind_rows(clean, synonyms) %>% 
  filter(! Symbol %in% genera_codes$Symbol) %>% 
  mutate(
    Genus = str_split_i(Scientific.Name, pattern = " ", 1),
    Species = str_split_i(Scientific.Name, pattern = " ", 2),
    Infrarank = str_extract(Scientific.Name, 'var[.]|ssp[.]'),
    Infra = str_split_i(Scientific.Name, pattern = " var[.] | ssp[.] ", 2),
    Infra = str_split_i(Infra, pattern = " ", 1),
    Infrarank = if_else(is.na(Infra), NA, Infrarank) # orthographic variants abbreviated as 'var.', drop it
    ) %>% 
  unite(Scientific.Name, Genus:Infra, sep = " ", na.rm = T)

out <- bind_rows(genera_codes, species) %>% 
  distinct(Symbol, Scientific.Name) %>% 
  arrange(Symbol)

out %>% 
  group_by(Scientific.Name) %>% 
  filter(n() > 1)

write.csv(out, '../taxonomic_data/USDA_PLANTS.csv', row.names = F)

rm(gen_clean, gen_syn, usda, clean, synonyms, genera_codes, species, out)
