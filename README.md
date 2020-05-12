# SDP Tree Extension

SDP Tree Extension is an generalized interface for common (and not very)
operation on trees.

## Reasons

* SDP provides linear structures that can't be used as trees.
* Trees and other SDP classes have limited integration potential.
* The number of operations on trees is very huge. Even the already existing 5
classes provide a small part of them.

## Functionality

SDP Tree provide some classes:
* Node - construction and deconstruction of nodes, child (branch) operations
* Tree - safe construction and deconstruction of trees, some higher order
functions, pattern synonyms
* ShiftTree - non cyclic shifts
* AppendTree - branch appending
* ParentTree - operations on trees with links to parents, Advanced xpath-style
data selection

## Versioning

sdp-trees follows of the [Haskell PVP](https://pvp.haskell.org) and SDP
extension rules.

