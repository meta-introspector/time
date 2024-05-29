for x in *.tar.gz;
do
    name=`echo $x| sed -e's/.tar.gz//g'`
    echo $x $name
    mkdir $name
    
    tar -C $name -xzf $x;
done
