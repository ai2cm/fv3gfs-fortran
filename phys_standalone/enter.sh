docker stop phys_standalone 1>/dev/null 2>/dev/null
docker rm phys_standalone 1>/dev/null 2>/dev/null
docker run -i -t --rm \
	--mount type=bind,source=`pwd`/sfc_sice,target=/work/sfc_sice \
	--mount type=bind,source=`pwd`/cloud_mp,target=/work/cloud_mp \
	--name=phys_standalone phys_standalone
