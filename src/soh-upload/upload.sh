# Intended usage is from the root of the repo:
# ./src/soh-upload/upload.sh

# You must have soh-upload installed:
# cabal install src/soh-upload

# You must set SIG_USER and SIG_TOKEN
# You may set SIG_FOLDER
#   (but make sure it has a trailing slash)

for FOLDER in content outline; do

echo ======= begin $FOLDER directory =======

cat > $FOLDER/soh-upload.yaml <<- EOF
user: $SIG_USER
security-token: $SIG_TOKEN
folder: $SIG_FOLDER$FOLDER/
EOF

soh-upload $FOLDER

rm $FOLDER/soh-upload.yaml

echo ======= end $FOLDER directory =======

done
