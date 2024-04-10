for x in `cat todo.txt`;
do echo $x;
   export NAME=`echo $x | cut -d/ -f2`

   export NAME2=meta-introspector/${NAME}
   pushd $NAME;
   PYTHONPATH=~/2024/04/10/github-actions-version-updater/ GITHUB_STEP_SUMMARY=github-actions-summary.md GITHUB_WORKSPACE=${NAME2} GITHUB_EVENT_NAME=fdsfsdf GITHUB_REF=main GITHUB_REPOSITORY=${NAME2} INPUT_TOKEN=`cat ~/.github` PULL_REQUEST_BRANCH=fff python3 -m src.main
   popd $x;
   #for y in `grep -r $x */.github/ |cut -d: -f1 | sort -u`
   #do
   #    echo $y
   #    sed -i.back  -e "s!${x}!${NAME2}!g" $y;
   #done
   
done
