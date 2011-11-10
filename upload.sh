#!/bin/bash -ex
cabal clean
cabal configure -fproduction
cabal build
rm -rf static/tmp
strip dist/build/yesodcms/yesodcms
bzip2 dist/build/yesodcms/yesodcms
scp -r dist/build/yesodcms/yesodcms.bz2 ubuntu@ditadocs.content-lifecycle.com:/home/ubuntu
scp -r static/custom ubuntu@ditadocs.content-lifecycle.com:/home/ubuntu/static
ssh ubuntu@ditadocs.content-lifecycle.com 'mv yesodcms yesodcms.old && bunzip2 yesodcms.bz2 && sudo restart yesodcms'
