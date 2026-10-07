version 17.0
clear all
set more off
set linesize 255
set varabbrev off
adopath ++ "/home/tpcopeland/R-Dev/_notes/tabtools-parity-2026-10-06/wp-stata-post251-port-v1/native-256-source/fvgen"
adopath ++ "/home/tpcopeland/R-Dev/_notes/tabtools-parity-2026-10-06/wp-stata-post251-port-v1/native-256-source/tabtools"
adopath
which puttab
which table1_tc
which desctab
do "/home/tpcopeland/R-Dev/_notes/tabtools-parity-2026-10-06/wp-stata-post251-port-v1/puttab-current-geometry-candidate-v1/capture_puttab_current_geometry.do" "/tmp/tabtools-native-256-ldv3y3pm/out"
display "ROOT_NATIVE_256_CAPTURE_COMPLETED"
exit, clear
