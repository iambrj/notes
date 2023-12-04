- Monolithic -> microservices: for load-balancing etc.
- Use microservices over containers instead of real machines to exploit
  benefits of containers.
- K8s provides automatic management of containers: it handles CRUD of
  containers and clustering them.
- Problems with scaling up containers that k8s helps with:
  * Containers cannot communicate with each other
  * Autoscaling and load balancing is not possible
  * Containers had to be manually managed, introducing scope for human errors
- Features of k8s:
  * Horizontal and vertical scaling
  * Auto healing
  * Load balancing
  * Platform (cloud/virtual/physical)
  * Fault tolerance
- Manifest: initial configuration.
- Cluster -> Node -> Pod -> Container -> Application/microservice.
- Pod: abstraction over containers to proivde a uniform communication interface
  irrespective of underlying container (may be docker or something else).
- Node: physical server/VM on which pods are running.
- Each pod is assigned an IP address, not each container in the pod.
- Although each pod may theoretically have more than one container, usually
  only one container is run per pod to avoid tight coupling of containers.
- Control plane = etcd + kube-API server + kube-scheduler + control manager
- 1 control plane / master
- kube-API server: interface to interact with master.
- etcd: highly available key-value store for shared configuration, service
  discovey, and scheduler coordination.
- kube-scheduler: handles pod creation and management, match/assign any node to
  create and run pods.
- Controller/manager: responsible for node detection, setting up network
  routes, setting up load balancers, volumne management
- Components of a node:
  * Kubelet: agent running on each node that communicates with master via API
    server. Runs on port 10255.
  * Container engine: docker etc. Exposing containers on port specified in
    manifest.
  * Kube-proxy: assigns an IP address to each pod.
