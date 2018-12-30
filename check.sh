#!/bin/bash

diff <(sort output.txt) <(sort simplex-lock-combos.txt)
