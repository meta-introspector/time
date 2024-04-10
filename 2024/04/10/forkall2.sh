
for x in `ls -b */.git | cut -d/ -f1`;
do
    pushd $x
    gh repo fork --org meta-introspector --remote
    popd
done
