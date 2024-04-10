for x in `cat todo5.txt `; do echo $x; git submodule add https://github.com/$x; done
