

for x in `cat modules.txt`;
do
    git submodule add $x;
done
