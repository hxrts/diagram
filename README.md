# diagram
model environment for distributed programs

![radar image of a butterfly swarm](./radar.jpg)

## Program Structure

*Control Flow:*
- Programs are modelled as a directed acyclic graph (DAG) where a `Node` represents an atomically evaluated unit and an `Edge` is a dependency or partial order between Nodes.
- `Atomic` ensures the subgraph is executed as a single unit.
- `Concurrent` allows independent subgraphs to be executed in parallel.

*Node Distribution:*
- Each Node specifies the `MachineID` where it will execute its computation.
- `DistributedIO` encapsulates the logic for communicating with that machine.

*Evaluation:*
- Dependencies are evaluated first, ensuring the DAG's structure is respected.
- Computations are executed on the correct machines via `runDistributed`.
