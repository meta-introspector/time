#find -name \*.yml -
grep action -r */.github/* |grep uses | cut -d: -f3- | cut -d@ -f1 | grep -v .github| sort -u  #> todo4.txt 
