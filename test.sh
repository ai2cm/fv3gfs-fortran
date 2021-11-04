make -C FV3 && rm -rf /run/user/1001/pytest-of-noahb/ && pytest --native tests/pytest/test_regression.py::test_emulation_two_steps | tee logs | grep REGRESSION > msg 

python read_logs.py



echo "Differences in IPD_Control type:"
echo "--" > reg
grep -A 265  'Control REGRESSION' logs >> reg


split -l 267 reg reg.part.
diff -u reg.part.ab reg.part.ac
