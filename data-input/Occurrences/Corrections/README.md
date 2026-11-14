## Corrections

Add .csv files to this folder to correct mistakes in the source data.
Use file_descriptions.csv to specify which columns will be used as identifiers and which will be used as corrections.
Example:

file_descriptions.csv:
```csv
arquivo,identificadores,dado_corrigido
itapeva_bananal.csv,barcode,locality
itapeva_bananal_alternativo.csv,"collectionCode,catalogNumber","locality"
```

itapeva_bananal.csv:
```csv
barcode,locality
SPSF00030716,"EEC BANANAL"
```

The correction script will identify any records with barcode "SPSF00030716" and change the locality field to "EEC BANANAL".

itapeva_bananal_alternativo.csv:
```csv
collectionCode,catalogNumber,locality
SPSF,30716,"EEC BANANAL"
```

Using this alternative file will affect the same record, but it will use the fields "collectionCode" and "catalogNumber" to select the records instead.