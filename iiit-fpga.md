---
author: Bharathi Ramana Joshi
title: 'Lectures notes for the FPGA based Accelerator Design course, IIIT Hyderbad Monsoon 2021'
---

# 18/08/2021

- Verilog (low level, like programming in x86) vs HLS/BSV/Chisel (high level,
    like programming in C/Python).
- Using the right abstractions for FPGA.
- Parameters for FPGA design
    1. Productivity
    2. Performance
    3. Portability
- Performance obstacles
    1. Memory wall: processor too fast for main memory to provide
       instructions/data. Cache helps sometimes.
    2. Parallelism Wall.
    (check out paper Limits of Instruction Level Parallelism http://www.eecs.harvard.edu/~dbrooks/cs146-spring2004/wall-ilp.pdf)
    3. Power Wall (cannot keep increasing clock frequency).
- ASICs: Application Specific Instruction Chip (e.g. Google's TPUs, Intel neural
    stick)
- Programmable Hardware: FPGA >> ASICs (except for performance)
- Three Ages of FPGAs: A Retrospective on the First Thirty Years of FPGA
    Technology

# 21/08/2021

- Configurable Logic Block (CLB)
- Heterogeneous architecture

# 25/08/2021

- Automata Processor: graph analysis, pattern matching, statistics
- Overlay: micro-architecture on top of FPGA
- Edge vs cloud: edge also collects data real time (e.g. cc camera), but cloud
  only does something with data (AWS Lambda)
- Fine grained/compute parallelism (parallel computing, divide chip into bins
  and assign thread to each bin) vs coarse grained parallelism (OS kinda
  parallelism).
- Choice of accelerator
    + Edge vs cloud
    + Cost
    + Profile of program/workload
    + Scalability
    + Development time
    + Portability
    + Reliability
- FaaS: FPGAs as a Service
- Measuring performance
    + Throughput: amount of work completed per unit time.
    + Bandwidth: theoretical bounds.
    + Latency: Time elapsed between initiation and completion of ta task.
    + Tradeoff between latency and throughput.
- Host (e.g. x86 CPU) <--- PCIe Link ---> Accelerator

# 28/08/2021

- Hardware vs Software programming paradigms.
- Framework for:
    + Setup host and accelerators
    + Define memory address spaces
    + Manage communication to and from host and accelerator
    + Define and schedule tasks
    + Manage queues
- Any complicated circuit can be broken down to combinational + sequential
    components.
- D-Flip Flop: value of D at falling edge of clock gets latched in the state of
  the D-flip flop.
- Register: Similar to D flip flop, except has flag for when input value must be
    latched into the state of the register.
- Bottleneck due to long paths in combinational circuits:
    + Cycle time must be > clock-to-q + longest path + T_su
- Pipelining a block
    + Clock?
    + Area?
    + Throghput?
- Optimization to reduce clock cycle length (latency may be same): Split up
  critical path by dividing combinational circuit into multiple components. E.g.
  64-bit ripple carry adder has delay of 64 * delay of single adder. Decrease
  this delay by using two 32-bit adders connected by a register (same latency,
  but clock cycle is halved).

# Coursera lectures
- History: PROM -> EPROM -> PLAs -> PALs
- PROM
    + Fixed AND plane
    + Programmable OR plane
- PAL (more efficient, since most logic functions have limited number of []
    terms)
    + Fixed OR plane
    + Programmable AND plane
- CPLD (Complex Programmable Logic Device)
- FPGA springs from PROM
- Altera first FPGA in 1984
- FPGA = wire, gate, register/flip-flop
- System on Chip (SoC)
- `FPGAs < PLDs < Logical devices`
- CPLD:
    + Reprogrammable array built from "macrocells", units that have all inputs
      available and can output to any line they want.
    + Suited for applications such as complex 16 state state machines with
        deterministic timing, address decoders, and glue logic.
    + Reinforced heirarchial design methods.
    + Drawback: not scalable for many flip-flops.
- FPGA:
    - Highly flexible.
    - Smaller than CPLD as it is based on memory.
    - Logic cell: 4-inpute LUT, Full Adder, D-type Flip-Flop.
    - Modes an FPGA can be used in (programmed via middle mux):
        1. Normal mode: Two 3-LUTs combined into 4-input LUT via left mux.
        2. Arithmetic mode: Outputs fed to FA.
    - Routing switches to connect various logic elements. These choices impact
        performance significantly.
    - Global networks: common routes (e.g. clock).
    - Memory choices:
        1. Antifuse: highly reliable, but OTP and expensive.
        2. FLASH: highly reliable, reprogrammable, but more expensive than SRAM
           FPGAs.
        3. SRAM: interconnects reporgrammable and highest density, lowest cost.
- When CPLD over FPGA?
    1. Save money in design requiring just a little logic.
    2. Add more I/O in a small space.
    3. Meet very tight and deterministic timing requirements.
- LUT design
    + SRAM bits to hold configuration memory + LUT mask + multiplexers to select
        SRAM to LUTS.
    + LUT vs routing tradeoff.
    + Larger LUT => more logic, less routing.
    + Smaller LUT => more routing, better efficiency.
    + Can be used to implement combinatorial logic functions.
- Adders can be efficiently implemented in adders, 4 * LUT delay
- Many ways to build FPGA
    1. Combinational circuits: fast but big
    2. Sequential shift & add (state machine approach)
    3. Speciality algoriths (Booth's, Dadda, Wallace tree)
    4. Memories (LUTs)
    5. Some combination of above.
    6. Hard multiplier blocks if available (best solution in most cases)
- Capabilities desirable in a FPGA
    1. Provide ample amounts of logic (measured by system gates, logic elements,
       slices, macrocells, etc)
    2. Cost per gate
    3. Speed (measured by clock frequency)
    4. Lower power consumption: static & dynamic (microwatts to hundreds of
       watts)
    5. Reprogrammability: anitfuse is not, flash & sram are
    6. Amount & type of IO
    7. Deterministic timing
    8. Reliability (FIT rate)
    9. Endurance (# of programming cycles & years of retention)
    10. Design & data security
- Xilinx: both reprogrammable with deterministic timing and rich I/O, but
    limited speed and logic density
    - Efficient comparator and decoder, but inefficient in adders and shift
        registers
    1. XC9500XL: 5 volent tolernat IO, important for industrial and automotive
       applications
    2. CoolRunner II: lower power CPLD family

# 01/09/2021

- Clock: `C < P ~ FP`
- Area: `FP < C < P`
- Throughput: `FP < C < P`

# 04/09/2021

# 11/09/2021

# 15/09/2021

- Links:
    + https://github.com/Xilinx/Vitis-Tutorials/tree/2021.1/Hardware_Acceleration/Introduction
    + https://www.xilinx.com/html_docs/xilinx2021_1/vitis_doc/kme1569523964461.html
    + https://www.khronos.org/registry/OpenCL/specs/opencl-1.2.pdf
- Amdahl's Law [HPC]
- Gustafson's law [Big data]
- Compute intensity
- PCIe bandwidth
- Checkout: BSP [MIT], Chisel shell [VCB]. Good project would be to see if
    something that can be done in BSP can also be done in Chisel.

# 18/09/2021

- Roofline analysis (see paper )
  + What is the peak possible performance?
  + What is stopping the program from achieving this?
- This tells us whether it is memory bound (i.e. memory transfer is too slow and
  processor is not being used as much as it can be) vs compute bound (processor
  is being used to the maximum of its capabilities, improving memory transfer
  won't improve performance).
- Multicore CPU performance model
  + Floating point operations per second
  + Memory transfer bandwidth
  + 
- Tricks to compute performance:
  * Instruction level parallelism
  * Floating-point balance
- Tricks to memory performance:
  * Improve spatial locality:
    + Regular (looping through an array) vs irregular (traversing a tree) memory
      access.
    + Unit stride vs > 1 stride
  * Prefetching

# 22/09/2021

- 

# Roofline analysis

## Wikipedia page

- Work (W): number of operations performed by given kernel/application.
- Memory traffic (Q): number of bytes of memory transfer during execution of
  kernel/application.
- Arithmetic intensity(I) = W / Q
- Naive roofline: peak performance (pi, calculated from architectural manuals)
  and peak bandwidth (beta, calculated from benchmarking), plot
  attainable performance P = min(pi, beta * I).
- Ridge point: point where diagonal roof (beta * I) and horizontal roof (pi)
  meet.
- Naive roofline tells us what the minimum compute intensity is required to
  achieve peak performance, but does not tell us what is currently stopping the
  kernel from achieving this performance.
- More ceilings specific to certain optimizations can be added to guide the
  programmer:
  1. Communication/bandwidth ceilings: absence of software prefetching.
  2. Computation/in-core ceilings: instruction level parallelism, task level
     parallelism.
  3. Locality/locality walls: realistically, intensity is not simply a function
     of the kernel --- if architectural aspects (caches) are taken into account

## Paper

- Operational intensity: operations per byte of DRAM traffic.
- Total bytes = between caches and memory, NOT processor and caches.
- 

# Questions

- Can explain what locality walls are?
