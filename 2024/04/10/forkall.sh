
for x in `ls -b */.git | cut -d/ -f1`;
do
    pushd $x
    for y in `git remote show origin |grep github | grep -v meta |grep Fetch | cut "-d " -f3`;
    do echo $y
       echo gh repo fork --org meta-introspector --remote
    done

    git status
    git commit -m 'closure of the gh actions to my org' -a
    git push origin
    popd
done
