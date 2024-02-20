DS=$(date -Iseconds)
mkdir -p data/
~/.opam/4.14.1/bin/simple.exe \
    --ollama -m "mixtral" -u "https://mixtral-agentartificial.ngrok.app" \
     -f notes.org \
    -s "data/out_${DS}"   \
    -x ".txt" \
    -n 10
