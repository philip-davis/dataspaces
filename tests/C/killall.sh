#!/bin/bash

#kill all previous dataspaces process
ps -aux | grep dataspaces| cut -c 9-15|xargs kill -9
ps -aux | grep test| cut -c 9-15|xargs kill -9