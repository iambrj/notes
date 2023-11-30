# Foundations
* Container: "lightweight" VM.
* Docker: enables containerization.
* Hypervisor: layer between os and application to provide virtualization layer.
  VMs run on top of hypervisor.
  - Type-1: runs on bare metal, mediates between hardware and OS. E.g.
    Citrix/Xen server, Microsoft Hyper-V.
  - Type-2: runs on top of OS, mediates between OS below it and OS running on
    top of it. E.g. Oracle Virtual Box.
  Docker is thus *similar* to a type-2 hypervisor.
* A container does not run a full-blown OS, tries to reuse OS running under
  the docker engine.
* Container gets hardware resources on-demand, only when it needs to use them.
  Whenever it does not need resources they are freed back into the OS under the
  docker engine.
* Docker Hub: world's largest library and community for container images.
* All dependencies of a container get installed from docker hub when docker
  engine runs the container.
* Docker image: a static file that contains everything needed to run an
  application, including the application code, libraries, dependencies, and the
  runtime environment. It's like a snapshot of a container that, when executed,
  creates a Docker container.
* Docker platform was designed natively for Linux.
* The container shares the Linux kernel with the underlying OS.
* When Docker platform is running on Windows and has to run a Linux container,
  it runs it using a LinuxKit based virtual machine running on Hyper-V.
* Advantages of Docker:
  - No pre-allocation of RAM.
  - Decreases costs by allowing sharing of resources between containers.
  - Re-usability by sharing of images. Note: images are immutable. An image is
    a "snapshot" of a container.
* Disadvantages of Docker:
  - Lack of cross-platform support.
  - Docker is limited to applications that do not have GUI.
  - Difficult to manage large amount of containers.
  - It is recommended to run images on the OS they were created in.
* Images are organized in layers: every change introduced is part of a layer.
  Each layer represents a specific modification to the file system (inside the
  container), such as adding a new file or modifying an existing one. Once a
  layer is created, it becomes immutable. An image can be gotten from Docker
  Hub, generated from a Docker file, and generated from a running container.
* Ecosystem:
  - Docker daemon/server/engine: A persistent background process that manages
    Docker images, containers, networks, and storage volumes. The Docker daemon
    constantly listens for Docker API requests and processes them.
  - Docker client: provides a command line interface (CLI) that allows you to
    issue build, run, and stop application commands to a Docker daemon.
  - Docker compose: a tool that helps you define and share multi-container
    applications.
  - Docker host: the physical hardware on which docker is being run.

# Practicals
* `docker images`: list images present in local system
* `docker search`: search images on Docker Hub
* `docker pull`: download images from Docker Hub
* `docker ps`: process status, lists info about running containers
* `docker run`: runs a container from an image, login's into it's shell
* `docker start`: starts a container from an image, doesn't login into it's shell
* `docker attach`: login into a running container's shell
* `docker stop`: stop a started container
* `docker rm`: delete a stopped container
