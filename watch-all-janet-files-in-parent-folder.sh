#!/bin/env sh

find ../ -type f -name "*.janet" | sort | uniq | entr jpm test
