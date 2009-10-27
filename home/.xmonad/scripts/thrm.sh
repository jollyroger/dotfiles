#!/bin/sh
TEMPERATURE=`awk '{print $2}' /proc/acpi/thermal_zone/THRM/temperature`
echo Temp: ${TEMPERATURE}°C
