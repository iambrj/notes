## Who runs the Internet?
* Administrative + technical + political wings
### Manor players
* ICANN - Internet Corporation for Assigned Names and Numbers
* ISOC - the Internet Society
* IGF - the Internet Governance Forum
* RFCs - documents with protocol standards, proposed changes and information
	bulletins
# Road Map
TCP/IP is a protocol "suite" of network protocols designed to work together
* IP - Internet Protocol: routes data packets from one machine to another
* ICMP - Internet Control Message Protocol: provides several kinds of low level
	support for IP including error messages, routing assistance, and debugging
	help
* ARP - Address Resolution Protocol: translates IP addresses to hardware
	addresses
* UDP - User Datagram Protocol: provides unverified, one-way data delivery
* TCP - Transmission Control Protocol: implements reliable full duplex,
	flow-controlled, error-corrected conversions
* IPv4 - four-byte IP addresses
* IPv6 - 16 byte IP addresses, remove useless features, faster, easier to
	implement
## Packets and encapsulation
* TCP/IP supports variety of physical networks and transport layers - Ethernet,
	token ring, Multiprotocol Label Switching, wireless Ethernet and
	serial-line-based systems. All hardware abstracts out at the link layer.
* Data travels as packets, with a maximum length imposed by the link layer. 
## Packet = Header + Payload
* Header = where to/from, checksums, handling instructions
* **Framing**- adding headers to packets and putting separators between them
* Link Layer = MAC + LLC
* MAC = Media Access Contro - deals with media and transmits packets onto the
	wire
* LLC = Link Layer Control Layer - deals with framing
* Fragmentation - subdividing packet onto small MTU network
* Set `do not fragment` flag to discover lowest-MTU link; router returns ICMP
	error message to the sender
## Packet Addressing
Several addressing schemes are used in combination
* MAC - Media Access Control: for use by hardware
	Each of a host's network interface has on link-layer MAC address that
	distinguishes it from other machines on the physical network, plus one or
	more IP addresses that identify the interface on the global Internet
	Ethernet uses 6-byte address = 3-byte manufacturer + 3-byte unique serial
* IPv4 and IPv6: for use by software
	- Globally unique and hardware independent
	- Link layer implements IP address to hardware address mapping
	- Use ARP protocol to discover link layer addresses
* Hostnames: for use by people
	- Port: extension to IP address, a 16-bit number that specifies a particular
		communication channel
	- `/etc/services` contains definitions of standard ports
	- UNIX systems restrict server programs from binding to port numbers under
		1024 unless run as root
	- Address types:
		- Unicast - addresses that refer to a single network interface
		- Multicast - addresses that simultaneously target a group of hosts
			(video conferences)
		- Broadcast - address that include all hosts on the local subnet
		- Anycast - addresses that resolve to any one of a group of hosts
# IP Addresses
* Network portion + Host portion
* Network portion - logical network
* Host portion - node on that network
* IPv4: addresses 4 bytes long, boundary set administratively
* IPv6: 16 bytes long, each 8 bytes
## IPv4
* IPv4 addresses are written as decimal numbers, one for each byte, separated by
	periods (`209.85.171.147`)
* When `127` is the first byte of an address, it denotes a "loopback newtork" -
	a fictitious network that has no real hardware interface and only one host
* The loopback address `127.0.0.1` always refers to current host
