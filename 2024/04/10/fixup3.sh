
for x in `ls -b */.git | cut -d/ -f1`;
do
    pushd $x
    export NAME2=meta-introspector/${x}

    #find 	.github/workflows/ -name \*.back -exec rm {} \;
    #git add github-actions-summary.md
    #git commit -m 'cleanup' -a
    #git checkout -b 'newversions'
    git status ;
    #git checkout main;
    #git merge newversions;
    #git push origin main

    popd;
   
done
