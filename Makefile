SHELL := /bin/bash
R ?= Rscript

all:

create-uc-summary: config
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

detect-duplicates: data-tmp/corpus-full.rda
	$(R) "analyses/detectDuplicates.R"

results/summary_getOccs.csv: data-tmp/corpus-full.rda
	$(R) "analyses/getOccs.R"

treat-occs: results/summary_getOccs.csv
	$(R) "analyses/treatOccs.R"

clean:
	rm -f results/**/*.csv results/**/*.rda

purge: clean
	rm -f data-tmp/*