#!/bin/sh
TEMPERATURE=`sensors|awk '/^Core0.*/ {print $3}'`
echo Temp: ${TEMPERATURE}
