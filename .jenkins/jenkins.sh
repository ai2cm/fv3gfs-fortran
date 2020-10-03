#!/bin/bash -f

# This is the master script used to trigger Jenkins actions.
# The idea of this script is to keep the amount of code in the "Execute shell" field small
#
# Example syntax:
# .jenkins/jenkins.sh test
#
# Other actions such as test/build/deploy can be defined.

### Some environment variables available from Jenkins
### Note: for a complete list see https://jenkins.ginko.ch/env-vars.html
# slave              The name of the build slave (daint, kesch, ...).
# BUILD_NUMBER       The current build number, such as "153".
# BUILD_ID           The current build id, such as "2005-08-22_23-59-59" (YYYY-MM-DD_hh-mm-ss).
# BUILD_DISPLAY_NAME The display name of the current build, something like "#153" by default.
# NODE_NAME          Name of the slave if the build is on a slave, or "master" if run on master.
# NODE_LABELS        Whitespace-separated list of labels that the node is assigned.
# JENKINS_HOME       The absolute path of the data storage directory assigned on the master node.
# JENKINS_URL        Full URL of Jenkins, like http://server:port/jenkins/
# BUILD_URL          Full URL of this build, like http://server:port/jenkins/job/foo/15/
# JOB_URL            Full URL of this job, like http://server:port/jenkins/job/foo/

# stop on all errors
set +e

# get root directory of where jenkins.sh is sitting
root=`dirname $0`
envloc=`dirname $0`

# some global variables
action="$1"
optarg="$2"

# get latest version of buildenv
git submodule update --init --recursive

# setup module environment and default queue
. ${envloc}/env/machineEnvironment.sh

# load machine dependent environment
. ${envloc}/env/env.${host}.sh

# check if action script exists
script="${root}/actions/${action}.sh"
test -f "${script}" || exitError 1301 ${LINENO} "cannot find script ${script}"

# run the action script
echo "### Running ${script} ${optarg}"
${script} ${optarg}
if [ $? -ne 0 ] ; then
  exitError 1510 ${LINENO} "problem while executing script ${script}"
fi

echo "### ACTION ${action} SUCCESSFUL"

exit 0
