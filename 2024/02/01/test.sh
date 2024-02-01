#!/bin/bash
file_name=trace_log
current_time=$(date "+%Y.%m.%d-%H.%M.%S")
batch=batch3
outdir=~/experiments/lang_agent/huggingface/$batch
mkdir -p $outdir
new_fileName=$outdir/$file_name.$1.$current_time."org"
echo building $new_fileName

~/experiments/lang_agent/_build/default/bin/worker.exe prompt.txt separator.txt task.txt > ${new_fileName}.a.org &
~/experiments/lang_agent/_build/default/bin/worker.exe prompt.txt separator.txt task.txt > ${new_fileName}.b.org 

