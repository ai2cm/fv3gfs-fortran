make -C FV3 && rm -rf /run/user/1001/pytest-of-noahb/ && pytest --native tests/pytest/test_regression.py::test_emulation_two_steps | tee logs | grep REGRESSION > msg 
