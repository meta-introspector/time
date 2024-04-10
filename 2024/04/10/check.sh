for x in `cat todo4.txt`;
do echo $x;
   export NAME=`echo $x | cut -d/ -f2`
   export NAME2=meta-introspector/${NAME}

   pushd $NAME;
   #git commit -m 'auto cleanup' -a
   #git remote show origin
   git push origin
   popd;
done
