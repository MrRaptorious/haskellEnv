#! /bin/bash

# install random update
sudo apt-get update -y &&
sudo apt-get install libgmp-dev -y && 

# update cache so random can be found
stack update --allow-different-user &&

# install random
stack install random --allow-different-user