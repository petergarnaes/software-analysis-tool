# software-analysis-tool
A basic framework software analysis and optmizer written in Haskell

## Functionality
This framework can perform basic control flow analysis of an AST representing an imperative programming language called "While".

## Implementation
Implemented with Monads. User must implement their custom lattice and transfer functions, like the illustration below shows.

# Example use
An example analysis and optimizer for live variable analysis can be found in `LiveVariable.hs`, just load it in `ghci` and call `test1`.
