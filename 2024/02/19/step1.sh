DS=$(date -Iseconds)
mkdir -p data/
~/.opam/4.14.1/bin/simple.exe \
     --openai -m "mixtral" -u "https://dev-hub.agentartificial.com" \
     -f notes.org \
    -s "data/out_${DS}"   \
    -x ".txt" \
    -n 10
