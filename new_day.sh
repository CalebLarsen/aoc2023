dircount=$(find . -maxdepth 1 -type d | grep -c -x '.\{4\}' | xargs printf "%.2d")
mkdir "$dircount"
cp -r template/* ./"$dircount"
echo "$dircount" "created"