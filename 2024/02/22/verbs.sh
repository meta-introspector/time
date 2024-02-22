DS=$(date -Iseconds)
mkdir -p data/
echo "extract a list of verbs from the following text:" > prompt_verbs.txt
cat notes.org >> prompt_verbs.txt
mkdir data
#    --ollama -m "mixtral" -u "https://mixtral-agentartificial.ngrok.app" \
~/.opam/4.13.1/bin/simple.exe \
      --ollama -m "mistral:instruct" -u "http://localhost:11434" \
     -f prompt_verbs.txt \
    -s "data/out_${DS}"   \
    -x ".txt" \
    -n 10
