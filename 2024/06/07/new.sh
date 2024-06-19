

#grep -f repos.txt -F /time/.gitmodules | cut -d/ -f4- > found.txt

for x in `grep -f found.txt -v repos.txt `;
do
    NAME=`echo $x | tr -d '\/' `
    git submodule add  https://github.com/$x $NAME
    
done
#for x in
