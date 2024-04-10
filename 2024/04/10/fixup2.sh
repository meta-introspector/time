
for x in `ls -b */.git | cut -d/ -f1`;
do
    pushd $x
   export NAME2=meta-introspector/${x}
   PYTHONPATH=~/2024/04/10/github-actions-version-updater/ GITHUB_STEP_SUMMARY=github-actions-summary.md GITHUB_WORKSPACE=${NAME2} GITHUB_EVENT_NAME=fdsfsdf GITHUB_REF=main GITHUB_REPOSITORY=${NAME2} INPUT_TOKEN=`cat ~/.github` PULL_REQUEST_BRANCH=fff python3 -m src.main
   popd;
   
done
