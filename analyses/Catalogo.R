library(plantR) # used foi reading and cleaning occurrence data
if(!require(integraFlora)) devtools::load_all()

# Read list from Catalogo das Plantas das UCs do Brasil
cl0 <- read.csv(("data/raw-data/Dados_Catalogo_UCs_Brasil.csv"))
names(cl0)

# Choose the units we are interested in
cl0$UC <- standardize_uc_name(cl0$Unidade.Conservação)
tab(cl0$UC)
ucs <- read.csv(file.path(Sys.getenv("RESULTS_DIR"), "summary_multilist.csv"))
used <- cl0$UC %in% ucs$Nome.da.UC
tab(cl0$UC[used])
cl0 <- cl0[used,]

# Format taxons with the same logic as we are using for the rest
cl0$scientificName <- substr(cl0$Táxon, nchar(cl0$Família) + 2, nchar(cl0$Táxon))
cl0 <- getTaxonId(cl0)
table(cl0$tax.notes)
tab(cl0$Família[cl0$tax.notes=='not found'])
tab(cl0$UC[cl0$tax.notes=='not found'])
dim(cl0)
familiesRef <- unique(cl0$family.new)
length(familiesRef)
speciesRef <- unique(cl0$scientificName.new)
length(speciesRef)
str(cl0)


save(catalogoCompleto, file="data/raw-data/catalogoCompleto.rda")
