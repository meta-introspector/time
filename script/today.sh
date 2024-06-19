
today() {
    TODAY=$HOME/`date +'%Y/%m/%d'`
    if [ ! -d ${TODAY} ];
    then
	mkdir -p ${TODAY}
    fi
    cd ${TODAY}
    echo $TODAY
    pushd $TODAY
}

today
