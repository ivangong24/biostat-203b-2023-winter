#!/bin/bash
# Select lines from the middle of a file.
head -n "$2" "$1" | tail -n "$3"