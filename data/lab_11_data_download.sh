#!/bin/bash

wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1JtuR21hQef1MeiIWYCiolX_tMTAjWvP4' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1JtuR21hQef1MeiIWYCiolX_tMTAjWvP4" -O censoc_175_2023.csv && rm -rf /tmp/cookies.txt
