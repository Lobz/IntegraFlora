SHELL := /bin/bash
R ?= Rscript

all: treat-occs

install-deps: install_deps_linux.sh
	$(SHELL) install_deps_linux.sh

install: install-deps DESCRIPTION
	$(R) -e "devtools::install_deps()"

create-uc-summary: data-input/Locations/info/Summary.csv

data-input/Locations/info/Summary.csv: config.R
	$(R) "analyses/createUCsummary.R"

data-tmp/gbif.RData: data-input/Occurrences/GBIF/*
	$(R) "analyses/formatData/GBIF.R"

data-tmp/jabot.RData: data-input/Occurrences/JABOT/*
	$(R) "analyses/formatData/JABOT.R"

data-tmp/reflora.RData: data-input/Occurrences/REFLORA/*
	$(R) "analyses/formatData/Reflora.R"

data-tmp/splink.RData: data-input/Occurrences/splink/*
	$(R) "analyses/formatData/splink.R"

data-tmp/other.RData: data-input/Occurrences/OtherSources/*
	$(R) "analyses/formatData/other.R"

data-tmp/treated_data_all.RData: data-tmp/gbif.RData data-tmp/jabot.RData data-tmp/reflora.RData data-tmp/splink.RData data-tmp/other.RData
	$(R) "analyses/joinData.R"

data-tmp/corpus-full.rda: data-tmp/treated_data_all.RData config.R
	$(R) "analyses/treatData.R"

data-tmp/corpus.rda: data-tmp/corpus-full.rda
	$(R) "analyses/detectDuplicates.R"

results/summary_getOccs.csv: data-tmp/corpus.rda data-input/Locations/extraTables/checkedLocations.csv data-input/Locations/info/Summary.csv data-input/Locations/extraTables/uc_locstrings.csv analyses/getOccs.R
	$(R) "analyses/getOccs.R"

treat-occs: results/summary_getOccs.csv analyses/treatOccs.R
	$(R) "analyses/treatOccs.R"

clean:
	$(R) "analyses/cleanResults.R"

purge: clean
	rm -rf data-tmp/*.rda data-tmp/*.RData plantR_input/*