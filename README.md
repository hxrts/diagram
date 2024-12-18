# diagram

![radar image of a butterfly swarm](./radar.jpg "Radar image of a butterfly swarm")

## Overview

A DAG-based evaluation system for distributed computation that handles dependency and atomicity.

Programs are modeled as a directed acyclic graph where nodes represent atomically evaluated units and edges are dependencies between them, expressed as a partial order. Subgraphs can themselves be nodes by declaring atomicity of that group.
Machines are analogous to chains or state machines. Each machine can lock and free resources, commit operations, and rescind on failure to free remote resource locks.

## Install and Run

Clone the repo and run the following command to create a development shell with all the necessary dependencies:

```bash
nix develop
```

Run the Application with a configuration file that includes the node setup and program definition:

```bash
nix run
```

### Train-Hotel Problem

This test simulates a scenario where a train booking to Amsterdam must be made atomically with a hotel in Amsterdam. The program ensures making exactly one successful atomic booking, i.e. there is a partial order between booking attempts. The system first attempts to book a train and hotel in Amsterdam. If this attempt fails due to machine failure, the system unwinds holds and attempts to book a train and hotel in Berlin.

Machines represent the entities responsible for performing actions such as placing holds, booking resources, and releasing holds. Each machine can lock resources, commit operations, rescind on failure, and send messages to free remote resources.
