SHELL := /bin/bash
R ?= Rscript

# StateProvince (overrides config)
export STATE
# Resulsts mega-folder for many states
export RESULTS_MANY=../ChecklistsBrazil
# Temporary objects folder
export DATATMP=data-tmp
# Temporary state-specific folder
export DATASTATE=$(DATATMP)
# Results folder
export RESULTS_DIR=results

GBIF_URL ?= https://api.gbif.org/v1/occurrence/download/request/0000452-260623161305970.zip
GBIF_FILE ?= "data-input/Occurrences/GBIF/GBIF_Brazil.zip"

all: treat-occs

install-deps: install_deps_linux.sh
	$(SHELL) install_deps_linux.sh

install: install-deps DESCRIPTION
	$(R) -e "if(!require(devtools)) install.packages('devtools')"
	$(R) -e "devtools::document(); devtools::install()"

create-uc-summary: $(RESULTS_DIR)/UCsummary.csv

get-occurrences: $(RESULTS_DIR)/summary_getOccs.csv

treat-data: $(DATASTATE)/corpus.rda

Locations/extraTables/municipalityGazetteer.csv:
	$(R) "analyses/createMunicipalityGazetteer.R"

Locations/extraTables/uniqueMunicipalities.csv: Locations/extraTables/municipalityGazetteer.csv

Locations/extraTables/locGazetteer.csv:

$(RESULTS_DIR)/UCsummary.csv: config.R data-input/Locations/info/* #analyses/createUCsummary.R R/search.R
	$(R) "analyses/createUCsummary.R"

$(DATATMP)/gbif.rda: analyses/formatData/GBIF.R  data-input/Occurrences/GBIF/*
	$(R) "analyses/formatData/GBIF.R"

$(DATATMP)/jabot.rda: analyses/formatData/JABOT.R data-input/Occurrences/JABOT/*
	$(R) "analyses/formatData/JABOT.R"

$(DATATMP)/reflora.rda: analyses/formatData/Reflora.R data-input/Occurrences/REFLORA/*
	$(R) "analyses/formatData/Reflora.R"

$(DATATMP)/splink.rda: analyses/formatData/splink.R data-input/Occurrences/splink/*
	$(R) "analyses/formatData/splink.R"

$(DATATMP)/other.rda: analyses/formatData/other.R data-input/Occurrences/OtherSources/*
	$(R) "analyses/formatData/other.R"

$(DATATMP)/all_data.rda: analyses/joinData.R $(DATATMP)/gbif.rda $(DATATMP)/jabot.rda $(DATATMP)/reflora.rda $(DATATMP)/splink.rda $(DATATMP)/other.rda
	$(R) "analyses/joinData.R"

$(DATATMP)/treated_data_all.rda: analyses/treatData_part1.R $(DATATMP)/all_data.rda Locations/extraTables/uniqueMunicipalities.csv Locations/extraTables/municipalityGazetteer.csv Locations/extraTables/locGazetteer.csv analyses/fixLocation.R
	$(R) "analyses/treatData_part1.R"

$(DATASTATE)/corpus-full.rda: analyses/treatData.R config.R $(DATATMP)/treated_data_all.rda R/plantRWorkflow.R R/getTaxonId.R
	$(R) "analyses/treatData.R"

treated-data/noStateInfo.rds: analyses/treatData.R config.R $(DATATMP)/treated_data_all.rda R/plantRWorkflow.R R/getTaxonId.R
	bash changeConf.sh "none"
	$(R) "analyses/treatData.R"
	$(R) -e "load('corpus-full.rda'); saveRDS(corpus, 'treated-data/noStateInfo.rds')"
	bash changeConf.sh $(STATE)

$(DATASTATE)/corpus.rda: analyses/deduplicate.R $(DATASTATE)/corpus-full.rda treated-data/noStateInfo.rds #analyses/deduplicate.R
	$(R) "analyses/deduplicate.R"

$(RESULTS_DIR)/summary_getOccs.csv: analyses/getOccs.R config.R $(DATASTATE)/corpus.rda data-input/Locations/extraTables/checkedLocations.csv $(RESULTS_DIR)/UCsummary.csv data-input/Locations/extraTables/uc_locstrings.csv
	$(R) "analyses/getOccs.R"

treat-occs: $(RESULTS_DIR)/summary_treatOccs.csv

$(RESULTS_DIR)/summary_treatOccs.csv: $(RESULTS_DIR)/summary_getOccs.csv analyses/treatOccs.R R/create_list_file.R
	$(R) "analyses/treatOccs.R"

clean: analyses/cleanResults.R
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
