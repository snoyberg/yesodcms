#!/bin/bash -ex
cabal clean
cabal configure -fproduction
cabal build
rm -rf static/tmp
strip dist/build/yesodcms/yesodcms
bzip2 dist/build/yesodcms/yesodcms
scp -r dist/build/yesodcms/yesodcms.bz2 ubuntu@beta.yesodweb.com:/home/ubuntu/yesodcms
scp -r static/custom ubuntu@beta.yesodweb.com:/home/ubuntu/yesodcms/static
ssh ubuntu@www.yesodweb.com 'cd yesodcms && sh update.sh'
