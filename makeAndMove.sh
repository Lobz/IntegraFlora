# Source - https://stackoverflow.com/a/6501343
# Posted by geekosaur, modified by community. See post 'Timeline' for change history
# Retrieved 2026-08-14, License - CC BY-SA 3.0

if [ -z "$1" ]; then
    echo "no parameter"
else
    echo "$1"
    cat config.R | sed 's/STATEPROVINCE = \".*\"/STATEPROVINCE = \"$1\"/' | cat
fi
