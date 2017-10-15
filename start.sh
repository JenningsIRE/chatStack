#!/bin/bash
#Run this in terminal
stack build
stack exec stackChat $1
exit 0
