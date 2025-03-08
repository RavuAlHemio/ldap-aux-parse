#!/bin/sh
python3 contrib/gen_stringprep_map.py -o src/stringprep/case_fold_map.rs contrib/rfc4518_2_2.txt contrib/rfc3454_b2.txt
python3 contrib/gen_stringprep_map.py -o src/stringprep/case_sensitive_map.rs contrib/rfc4518_2_2.txt
