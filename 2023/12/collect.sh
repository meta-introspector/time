
find -name \*.git > gitdirs.txt

for x in `cat gitdirs.txt`;
do 
   foo=`grep github $x/config| grep url | grep https |tail -1|cut -d= -f2`;
   target=`dirname $x`
   echo git submodule add $foo $target
done

# use like bash ./collect.sh |grep https > add.sh
