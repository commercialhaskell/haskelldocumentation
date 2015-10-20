#!/usr/bin/env bash

set -eoux pipefail

# Intended usage is from the root of the repo:
# ./src/soh-upload/upload.sh

# You must have soh-upload installed:
# cabal install src/soh-upload

# You must set SIG_USER and SIG_TOKEN

echo Uploading files to $SIG_USER

for FOLDER in content outline; do

echo ======= begin $FOLDER directory =======

cat > $FOLDER/soh-upload.yaml <<- EOF
user: $SIG_USER
security-token: $SIG_TOKEN
folder: $FOLDER/
EOF

soh-upload $FOLDER

rm $FOLDER/soh-upload.yaml

echo ======= end $FOLDER directory =======

done
