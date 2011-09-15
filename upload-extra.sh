#!/bin/bash -ex
rm -rf static/tmp
scp -r data dita static ubuntu@beta.yesodweb.com:/home/ubuntu/yesodcms
