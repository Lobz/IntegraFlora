SHELL := /bin/bash
R ?= Rscript

# Temporary objects folder
export DATATMP=data-tmp
# Results folder
export RESULTS_DIR=results

GBIF_URL ?= https://api.gbif.org/v1/occurrence/download/request/0000452-260623161305970.zip
GBIF_FILE ?= "data-input/Occurrences/GBIF/GBIF_Brazil.zip"


all: treat-occs

install-deps: install_deps_linux.sh
	$(SHELL) install_deps_linux.sh

install: install-deps DESCRIPTION
	$(R) -e "devtools::install()"

create-uc-summary: data-input/Locations/info/Summary.csv

data-input/Locations/info/Summary.csv: config.R
	$(R) "analyses/createUCsummary.R"

$(DATATMP)/gbif.rda: data-input/Occurrences/GBIF/*
	$(R) "analyses/formatData/GBIF.R"

$(DATATMP)/jabot.rda: data-input/Occurrences/JABOT/*
	$(R) "analyses/formatData/JABOT.R"

$(DATATMP)/reflora.rda: data-input/Occurrences/REFLORA/*
	$(R) "analyses/formatData/Reflora.R"

$(DATATMP)/splink.rda: data-input/Occurrences/splink/*
	$(R) "analyses/formatData/splink.R"

$(DATATMP)/other.rda: data-input/Occurrences/OtherSources/*
	$(R) "analyses/formatData/other.R"

$(DATATMP)/all_data.rda: $(DATATMP)/gbif.rda $(DATATMP)/jabot.rda $(DATATMP)/reflora.rda $(DATATMP)/splink.rda $(DATATMP)/other.rda
	$(R) "analyses/joinData.R"

$(DATATMP)/treated_data_all.rda: $(DATATMP)/all_data.rda
	$(R) "analyses/treatData_part1.R"

$(DATATMP)/corpus-full.rda: $(DATATMP)/treated_data_all.rda
	$(R) "analyses/treatData.R"

$(DATATMP)/corpus.rda: $(DATATMP)/corpus-full.rda
	$(R) "analyses/deduplicate.R"

$(RESULTS_DIR)/summary_getOccs.csv: clean $(DATATMP)/corpus.rda data-input/Locations/extraTables/checkedLocations.csv data-input/Locations/info/Summary.csv data-input/Locations/extraTables/uc_locstrings.csv analyses/getOccs.R
	$(R) "analyses/getOccs.R"

treat-occs: $(RESULTS_DIR)/summary_getOccs.csv analyses/treatOccs.R
	$(R) "analyses/treatOccs.R"

clean:
	$(R) "analyses/cleanResults.R"

purge: clean
	rm -rf $(DATATMP)/*.rda $(DATATMP)/*.rda plantR_input/*

downloadIPTs:
	$(R) -e "devtools::load_all(); downloadReflora(); downloadJabot()"

downloadGBIF:
	wget -v $(GBIF_URL) -O $(GBIF_FILE)

openGBIF: $(GBIF_FILE)
	$(R) -e "devtools::load_all(); openZip($(GBIF_FILE))"
	rm $(GBIF_FILE)
