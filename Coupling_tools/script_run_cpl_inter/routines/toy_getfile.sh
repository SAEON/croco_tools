#!/bin/bash
#-------------------------------------------------------------------------------
#                                                          Configuration files
#-------------------------------------------------------------------------------

# Get grid file

cp -f ${CPL_FILES_DIR}/*.sh ./ # Copy scripts to create the mask and the toy_file

echo 'create input files for TOY'
for k in `seq 0 $(( ${nbtoy} - 1))` ; do
    echo "./create_oasis_toy_files.sh ${toyfile[$k]} toy_${toytype[$k]}.nc ${model_to_toy[$k]} ${timerange[$k]}"
    module load $ncomod
    ./create_oasis_toy_files.sh ${toyfile[$k]} toy_${toytype[$k]}.nc ${model_to_toy[$k]} ${timerange[$k]}
    module unload $ncomod
done



