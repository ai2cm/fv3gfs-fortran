docker stop phys_standalone 1>/dev/null 2>/dev/null
docker rm phys_standalone 1>/dev/null 2>/dev/null
docker run -i -t --rm \
	--mount type=bind,source=`pwd`/turb,target=/work/turb \
	--mount type=bind,source=`pwd`/seaice,target=/work/seaice \
	--mount type=bind,source=`pwd`/microph,target=/work/microph \
	--mount type=bind,source=`pwd`/shalconv,target=/work/shalconv \
	--name=phys_standalone phys_standalone
