#!/bin/bash

# show results
cat testsuite.out

# display first failing test log
set +e
grep RESULT testsuite.out | egrep 'FAIL|CRASH' > /dev/null
if [ $? -eq 0 ] ; then
  firstfail=`grep RESULT testsuite.out | egrep 'CRASH|FAIL' | /bin/sed 's/.* RESULT //g' | /bin/sed 's/:.*//g' | head -1`
  log=work/${firstfail}/exe.log
  if [ -f ${log} ] ; then
    echo "=== exe.log of first failing test BEGIN =="
    cat ${log}
    echo "=== exe.log of first failing test END =="
  fi
  exit 1
fi

exit 0
