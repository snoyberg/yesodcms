#!/bin/bash -ex
rm -rf static/tmp
scp -r config dita static ubuntu@socialkb.suite-sol.com:/home/ubuntu
