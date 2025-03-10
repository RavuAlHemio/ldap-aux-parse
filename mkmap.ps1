& python contrib\gen_stringprep_map.py -o src\stringprep\case_fold_map.rs contrib\rfc4518_2_2.txt contrib\rfc3454_b2.txt
& python contrib\gen_stringprep_map.py -o src\stringprep\case_sensitive_map.rs contrib\rfc4518_2_2.txt
& python contrib\gen_stringprep_map.py -o src\stringprep\prohibit_map.rs contrib\rfc3454_a1.txt contrib\rfc3454_c8.txt contrib\rfc3454_c3.txt contrib\rfc3454_c4.txt contrib\rfc4518_2_4.txt
