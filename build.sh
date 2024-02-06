#/bin/bash

clj -T:build build
tar cvzf riker.tgz config config.template.edn deps.edn README.md resources run.sh src version.edn
