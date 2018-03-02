#!/bin/bash

cat writer*.log | grep DATASPACES > tmp.txt ; awk '{print $8}' < tmp.txt
echo
cat reader*.log | grep DATASPACES > tmp.txt ; awk '{print $8}' < tmp.txt