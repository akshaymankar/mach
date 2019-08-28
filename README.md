# Project Mach

## What is it?

It is an **experiment** to make BOSH deployments and especially scaling up faster.

## How does it work?

1. Adds all the compiled packages to the stemcell and creates a new stemcell
1. Re-package releases wihtout any packages and remove any package dependencies from job manifests and re-upload the releases with a new name
1. Change deployment manifest to use the new releases

## How to use it

**NOTE:** These steps only works on a Linux machine and has only been tested with xenial stemcell. The code is hard-coded for google stemcell, so if you want some other stemcell you'd need to change the code.

1. Compile this project (binaries available soon!)
   1. Clone this repository
   1. Install [bosh-cli](https://bosh.io/docs/cli-v2-install/), [stack](https://docs.haskellstack.org/en/stable/README/)
   1. Compile the project with `stack install mach`
   1. Make sure `~/.local/bin` is in `$PATH` or move `~/.local/bin/mach` to anywhere in `$PATH`
1. Prep Dependencies:
   1. Docker
   1. Git
1. Prep:
   1. Clone [Fork of Stemcell builder](https://github.com/akshaymankar/bosh-linux-stemcell-builder)
   1. Run `cd bosh-linux-stemcell-builder/ci/docker/`
   1. Run `./run os-image-stemcell-builder`, you should find yourself in a docker container shell in a directory called `/opt/bosh`
   1. Run `bundle`
   1. Run `mkdir tmp`
   1. Run `bundle exec rake stemcell:build_os_image[ubuntu,xenial,$PWD/tmp/ubuntu_base_image.tgz]`
   1. In a separate shell, run `docker ps` to get the container id of the running container. Make sure this container keeps running until end of these steps.
1. Run:
   1. Deploy your BOSH deployment like you do normally: upload releases and stemcells then apply the manifest
   1. Run 
      ```bash
      mach -f /path/to/deployment-manifest.yml \
           -b /path/to/bosh-linux-stemcell-builder/tmp/ubuntu_base_image.tgz
           -c <container-id>
           --stemcell-version <whatever version number you want new stemcell to be>
      ```
      Make sure the stemcell version is not already uploaded to the director.
      This command will take some time, if you think it is stuck at "Building Stemcell", you can tail stemcell building logs at `/tmp/build-stemcell.std*`
      
   1. You should see `patched-manifest.yml` in the whatever directory you ran this command from.
   1. You should also see `bosh-stemcell-<version>-google-kvm-ubuntu-xenial-go_agent.tgz` in the `bosh-linux-stemcell-builder/tmp/` directory.
   1. Upload the stemcell
   1. Apply the patched manifest
   1. See it running fast
