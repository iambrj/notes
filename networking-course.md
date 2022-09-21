# meta

- Course link https://www.youtube.com/playlist?list=PL6RdenZrxrw9inR-IJv-erlOKRHjymxMN
- Slides https://github.com/khanhnamle1994/computer-networking
- Assignments https://github.com/PrincetonUniversity/COS461-Public

# Unit 1

- Reliable, bidirectional, bytestream
- Basic network idea:
  + Alice waiting for connection
  + Bob wants to connnect
  + Connection established
  + Bob writes data to connection
  + Alice reads data from connection
  + Either Alice or Bob can close the connection
  + They can close connection in between, can refuse connection, etc
- Four layer model : different layer got different jobs to do, separation of
  concerns
  4. Application : Many applications e.g. HTTP, BitTorrent, Skype, etc.
     Bidirectional reliable stream between two applications using application
     specific semantics.
  3. Transport : to ensure data transferred by network layer is in order, not
     corrupted, etc. E.g. TCP, UDP, RTP, etc. Controls congestion.
  2. Network : packet (called datagram in this layer) end to end transport
     across internet. E.g. IP. Best effort delivery, no guarantees.
  1. Link layer : packet moved from link to link. E.g. Ethernet, WiFi, 4G.
     Moving data between two routers or router and end point.
- Internet Protocol
  + IP Datagram = Data + TL Hdr + IP Hdr (IP Frame)
  + Job = deliver data to end post>
  + Sends to link layer, which then puts it into Link frame.
  + IP Hdr = IP Source Address + IP Destination Address.
- IP Design
  + IP is unreliable : packets may be dropped, but only if necessary.
  + IP is "connectionless" : no per-flow state, packets may be missequenced.
  + End to End Principle : Where possible, implement features in end hosts
  + Allows variety of reliable or unreliable services to be built on top
  + Works over any link layer : v few assumptions made
- IP Details
  + Tries to prevent packets looping: using Time To Live (TTL).
  + Will fragment packets if too long.
  + Uses header checksum to reduce chances of delivering datagram to wrong
    destination
  + Allows for new versions of IP
  + Allows for new options to be added to header
- 3-way handshake for establishing a connection : syn, syn/ack, ack
- Network layer address = IP Address, Transport layer address = port (which
  application to send the data to)
- wireshark to inspect packets across network layer
- traceroute to inspect packets across link layer/hops
- Packet switch : device that forwards packets to the next hop, eventually
  reaching the destination
- Flow: collection of datagram belonging to the same end-to-end communication
  (e.g. a TCP connection)
- Packet switching has no per-flow state
- Packet switching uses statistical multiplexing : allows flows to use all
  available link capacity, allows flows to share link capacity
- Link is busy? Hold packet to be sent later.
- Layering principle outcomes:
  + Modularity
  + Separation of concerns
  + Continuous improvement
  + Well defined service
  + Reusability
- IPv4 : Network + host
  + Network to get to correct network (administrative domain)
  + Host to get to correct device in network (within administrative domain)
  + Originally 3 classes of address A, B, C
- Netmask : bitmask used to determine which address are in the same network,
  which need to go through a router. If first 24 bits are common in the network,
  then 255.255.255.0 is the netmask, if first 22 bits are common then
  255.255.252.0, etc.
- Longest Prefix Match : algo IP routers use to chose matching entry from
  forwarding table. Always use the most specific match to forward along.
- Address Resolution Protocol (ARP) : how network layer discovers the link
  layers it is connected to
  + Link layer and network layers are decoupled logically, but are coupled in
    practise --- each network interface has its own link layer address and
    network layer (ip) address.
  + To figure out IP address link address mapping, each device maintains a cache
    mapping table.
  + If there is an IP address I whose link address is not known to a device D, D
    broadcasts an ARP packet to everyone on the network asking what the link
    address of I is. The device with IP address I, D_I, responds to this packet
    with a ARP packet containing information about its link address. D_I also
    updates its cache table to update D's information using the packet it
    received.

# Unit 2

- Application bytestream -> TCP Segment -> IP Datagram -> Link frame
- Connection establishment = 3 way handshake : Syn + Ack, Syn + Ack
- Connection teardown = Fin, Data + Ack, Fin, Ack
- Services provided:
  + Reliable byte delivery service
    - Ack for correct delivery
    - Checksum to detect data corruption
    - Seq numbers detect missing data
    - Flow control prevents overrunning receiver : receiver tells sender how
      much more buffer space it has to receive stuff
  + In-sequence delivery
  + Control congestion
- TCP Header contains:
  + Source port
  + Destination port
  + Sequence # : position in the bytestream of the first byte in the TCP data
    field
  + Ack sequence # : tells what byte is expected next, also tells everything
    till this byte has been received
  + Checksum
  + Header length (how many optional fields are present)
  + Flags
    - Ack : all data up until now is ack'd
    - Syn : Syncronizing used in handshake
    - Fin : closing of one direction of connect
    - Psh : send data immediately (e.g. keystroke)
- Unique TCP ID = Soure port + Destination port + RNG Initial Sequence Number +
  Source address + Destination address
- UDP Header contains:
  + Source port
  + Destination port
  + Length
  + Checksum
- UDP properties
  + Connectionless
  + Packets may show up in any order
  + No acks
  + Unreliable
- Applications use UDP when they want simple and fast service, don't care about
  reliability (e.g. DNS)
- ICMP (Internet Control Message Protocol) : used to report error conditions and
  help diagnose problems
- ICMP runs in transport layer
- ICMP returns an unreliable simple datagram with error message
- End-to-End Principle : the function in question can completely and correctly
  be implemented only with the knowledge and help of the application standing at
  the end points of the communication system.
- Function [reliability, security, etc] may be provided, but end systems may no
  take it for granted.
- Strong end-to-end : do NOT implement anything in the middle, only end to end.
  All network has to do is transfer datagrams.
- Protocols as FSMs
- Stop and Wait protocol : at most one packet in flight at any given time
- Sliding Window : window/bounded number of packets in flight
- Sliding Window Sender (SWS) behavior:
  + Keeps track of variables : Send Window Size, Last Ack Received (LAR), Last
    Segment Sent (LSS)
  + Invariant to maintain : `(LSS - LSR) <= SWS`
  + Advance LAR on new ack
  + Buffer up SWS segments
- Sliding Window Receiver (SWR) behavior:
  + Variables: Receive Window Size, Last Acceptable Segment, Last Segment
    Received
  + Invariant to maintain : `LAS - LSR <= RWS`
  + Acks are cumulative
- Retrasmission : go back n vs selective repeat
