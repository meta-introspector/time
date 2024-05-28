mkdir -p network
mkdir -p git/foundation/
mkdir -p git/ops/
mkdir -p go
mkdir -p cpp
mkdir -p backup
mkdir -p specs
mkdir -p python
mkdir -p forge

ln -s ../../2024/03/06/git.limo forge/
ln -s ../../2024/03/12/gnunet network/
ln -s -f ../../../../2024/03/13/libgit2 git/foundation/
rm llm
mkdir llm/
ln -s ../../2024/04/05/p2pllm llm/
ln -s -f ../../../../2024/04/06/rules_gitops  git/ops/adobe-rules_gitops
ln -s ../../2024/04/06/specs specs/libp2p-specs 
ln -s ../../2024/04/27/cpp-libp2p cpp/
ln -s ../../2024/04/27/go-libp2p-relay-daemon go/
ln -s ../../2024/05/26/PyGithub python/
ln -f -s ../../../2024/05/26/degitx git/
ln -s ../../2024/05/26/devel backup/bitdust-io-devel
ln -s ../../2024/05/26/forgefed forge/
ln -s ../../2024/05/26/gitverse forge/
ln -s ../../2024/05/26/hbs2
ln -s ../../2024/05/26/peerforge forge/
ln -s ../../2024/05/26/redwood
#ln -s ../../2024/05/28/*
