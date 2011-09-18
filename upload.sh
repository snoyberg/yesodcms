#!/bin/bash -ex
cabal clean
cabal configure -fproduction
cabal build
rm -rf static/tmp
strip dist/build/yesodcms/yesodcms
bzip2 dist/build/yesodcms/yesodcms
scp -r dist/build/yesodcms/yesodcms.bz2 ubuntu@socialkb.suite-sol.com:/home/ubuntu
scp -r static/custom ubuntu@socialkb.suite-sol.com:/home/ubuntu/static
ssh ubuntu@socialkb.suite-sol.com 'mv yesodcms yesodcms.old && bunzip2 yesodcms.bz2 && sudo restart yesodcms'
