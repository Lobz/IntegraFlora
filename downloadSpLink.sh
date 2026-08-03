#1/bin/bash
export SP_URL="https://specieslink.net/search/download-gzip/20260801142304-0031534"
export N_PARTS=4

cd data-input/Occurrences/splink/
if [ ! -d parts ]; then
    mkdir parts
fi
cd parts
for i in $(seq $N_PARTS)
do
    wget -O splink-part-$i.txt.gz $SP_URL-$i
    gzip --decompress splink-part-$i.txt.gz
done
cd ..
cat parts/*.txt > large-file.txt

cd ../../../
