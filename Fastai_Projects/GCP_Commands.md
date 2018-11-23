By Default the project_ID is set to the logged in project ID but to ssh we need to specify it.

## Create 250GB Disk and V100 or P4 Machines in project project_ID at asia-east1-a
	
	gcloud beta compute instances create fastai-boot --zone=asia-east1-a --machine-type=n1-standard-1 --subnet=fastai --maintenance-policy=TERMINATE --scopes=https://www.googleapis.com/auth/devstorage.read_only,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write,https://www.googleapis.com/auth/servicecontrol,https://www.googleapis.com/auth/service.management.readonly,https://www.googleapis.com/auth/trace.append --accelerator=type=nvidia-tesla-k80,count=1 --image-family=pytorch-1-0-cu92-experimental --image-project=deeplearning-platform-release --boot-disk-size=250GB --metadata="install-nvidia-driver=True" --no-boot-disk-auto-delete --boot-disk-type=pd-standard --boot-disk-device-name=fastai-boot-disk


	gcloud compute --project=$Project_ID ssh --zone=asia-east1-a jupyter@fastai-boot

	gcloud compute instances create fastai-p100 --zone=asia-east1-a --machine-type=n1-standard-8 --subnet=fastai --no-restart-on-failure --maintenance-policy=TERMINATE --preemptible --accelerator=type=nvidia-tesla-p100,count=1 --disk=name=fastai-boot,device-name=fastai-boot,mode=rw,boot=yes

	gcloud compute instances create fastai-k80 --zone=asia-east1-a --machine-type=n1-standard-8 --subnet=fastai --no-restart-on-failure --maintenance-policy=TERMINATE --preemptible --accelerator=type=nvidia-tesla-k80,count=1 --disk=name=fastai-boot,device-name=fastai-boot,mode=rw,boot=yes


## Create 250GB Disk and V100 or P4 Machines in project project_ID at europe-west4-c
	
	gcloud beta compute instances create fastai-boot-eu --zone=europe-west4-c --machine-type=n1-standard-1 --subnet=fastai --maintenance-policy=TERMINATE --scopes=https://www.googleapis.com/auth/devstorage.read_only,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write,https://www.googleapis.com/auth/servicecontrol,https://www.googleapis.com/auth/service.management.readonly,https://www.googleapis.com/auth/trace.append --accelerator=type=nvidia-tesla-p4,count=1 --image-family=pytorch-1-0-cu92-experimental --image-project=deeplearning-platform-release --boot-disk-size=250GB --metadata="install-nvidia-driver=True" --no-boot-disk-auto-delete --boot-disk-type=pd-standard --boot-disk-device-name=fastai-boot-disk

	gcloud compute --project=$Project_ID ssh --zone=europe-west4-c jupyter@fastai-boot-eu

	gcloud compute instances create fastai-v100 --zone=europe-west4-c --machine-type=n1-standard-8 --subnet=fastai --no-restart-on-failure --maintenance-policy=TERMINATE --preemptible --accelerator=type=nvidia-tesla-v100,count=1 --disk=name=fastai-boot-eu,device-name=fastai-boot,mode=rw,boot=yes

	gcloud compute instances create fastai-p4 --zone=europe-west4-c --machine-type=n1-standard-8 --subnet=fastai --no-restart-on-failure --maintenance-policy=TERMINATE --preemptible --accelerator=type=nvidia-tesla-p4,count=1 --disk=name=fastai-boot-eu,device-name=fastai-boot-eu,mode=rw,boot=yes
