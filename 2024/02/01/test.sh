#!/bin/bash
file_name=trace_log
current_time=$(date "+%Y.%m.%d-%H.%M.%S")
batch=batch3
outdir=~/experiments/lang_agent/huggingface/$batch
mkdir -p $outdir
new_fileName=$outdir/$file_name.$1.$current_time."org"
echo building $new_fileName

for y in {1..4}
do
    for x in {1..2}
    do
	echo  ${new_fileName}.a${y}${x}a.org
	~/experiments/lang_agent/_build/default/bin/worker.exe prompt.txt separator.txt $1 > ${new_fileName}.a${y}${x}a.org &
	~/experiments/lang_agent/_build/default/bin/worker.exe prompt.txt separator.txt $1 > ${new_fileName}.a${y}${x}b.org 
    done
done

