export SP_URL="https://specieslink.net/search/download-gzip/20260801142304-0031534"
export N_PARTS=4

cd data-input/Occurrences/splink/
mkdir parts
cd parts
for i in $(seq $N_PARTS)
do
    wget $SP_URL-$i
done
gzip --decompress *
cd ..
cat parts/* > splink_large_file.txt
