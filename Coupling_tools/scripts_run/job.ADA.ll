#!/bin/bash

#=========== Global directives ===========
# @ environment = COPY_ALL
# @ job_name = run_cpl
# @ output = $(step_name)_DATO_$(jobid).out
# @ error  = $(step_name)_DATO_$(jobid).err

#=========== Step 1 directives ===========
#============== run oasis ================
# # requirements = (Feature =="prepost")
# @ step_name = run_cpl
# @ job_type = parallel
# @ total_tasks = 8
# @ notification = error
# mem up to 20 in serial 7 in parallels
# # as_limit = 7.0gb
# @ wall_clock_limit = 01:00:00
# @ queue

./run_cpl 0 4 4

