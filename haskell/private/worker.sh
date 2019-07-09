#!/bin/bash
>&2 echo "Hello Worker!"
exec /home/artem/worker $@
