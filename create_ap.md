* To create a WiFi, use `sudo create_ap wlp2s0 enp3s0 lappy nowifi4u`
* To delete redundant virtual interfaces, use `sudo iw dev ap0 del`
* To up an interface, use `sudo ip link set wlp2s0 up`
* To deal with `RTNETLINK answers: Operation not possible due to RF-kill`, use `sudo rfkill unblock wlan`
