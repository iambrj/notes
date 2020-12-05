* A record represents a collection of values stored together as one, where each
	component is identified by a different field name.
* Syntax
```
type <record-name> =
  {
  <field> : <type>;
  <field> : <type>;
  }
```
* Example
```
type host_info =
{
	hostname : string;
	os_name : string;
	cpu_arch : string;
};;
```
* Elements from the record field can be extracted using the dot notation
```
my_host.cpu_arch;;
- : string = "unknown"
```
* Types can be parameterized for polymorphism
```
type 'a with_line_num = { item: 'a; line_num : int};;
let parse_lines parse file_contents =
  let lines = String.split ~on:'\n' file_contents in
  List.mapi lines ~f:(fun line_num line ->
    { item = parse line;
      line_num = line_num + 1;
    })
;;
parse_lines service_info_of_string
  "rtmp              1/ddp     # Routing Table Maintenance Protocol
   tcpmux            1/udp     # TCP Port Service Multiplexer
   tcpmux            1/tcp     # TCP Port Service Multiplexer"
;;
parse_lines Int.of_string "1\n10\n100\n1000";;
```
* Pattern matching
```
let service_info_to_string { service_name = name; port = port; protocol = prot  } =
  sprintf "%s %i/%s" name port prot
```
- Patterns can mention only a subset of fields
# Functional update
```
{ <record> with <field> = <value>;
      <field> = <value>;
      ...
}
```
# Mutable fields
- Use the `mutable` keyword for declaring a field as mutable
- Use the `<-` for side-effecting update
```
let register_heartbeat t hb =
  t.last_heartbeat_time   <- hb.Heartbeat.time;
  t.last_heartbeat_status <- hb.Heartbeat.status_message
;;
```
