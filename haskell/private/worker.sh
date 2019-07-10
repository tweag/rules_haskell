#!/bin/bash
>&2 echo "Hello Worker!"
#exec strace -f -e trace=%file ~/worker $@
exec ~/worker $@
