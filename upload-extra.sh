#!/bin/bash -ex
rm -rf static/tmp
scp -r config dita static ubuntu@ditadocs.content-lifecycle.com:/home/ubuntu
