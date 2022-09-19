- Course link https://www.youtube.com/playlist?list=PL6RdenZrxrw9inR-IJv-erlOKRHjymxMN
- Slides https://github.com/khanhnamle1994/computer-networking
- Assignments https://github.com/PrincetonUniversity/COS461-Public

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
- IPv4 : A.B.C.D
- Netmask : bitmask used to determine which address are in the same network,
  which need to go through a router. If first 24 bits are common in the network,
  then 255.255.255.0 is the netmask, if first 22 bits are common then
  255.255.252.0, etc.
