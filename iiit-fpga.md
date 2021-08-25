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
