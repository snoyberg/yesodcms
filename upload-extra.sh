#!/bin/bash -ex
rm -rf static/tmp
scp -r config data dita static ubuntu@beta.yesodweb.com:/home/ubuntu/yesodcms
