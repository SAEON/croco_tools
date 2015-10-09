#!/bin/bash

echo "Compute the RVTK for analytical case"
./Scripts/testromsagrif_noagrif.sh
echo " "
echo "Compute the RVTK Vortex + Regional"
./Scripts/testromsagrif.sh
echo " "
