0.7
---
* Build warning-free on GHC 7.10 and GHC 8.0-rc1

0.6
---
* Fixed the `dfs` `enterVertex` and `exitVertex` order, they were wrong before.
* Factored out a common visitor model for both `bfs` and `dfs`.

0.5
---
* Added `enterEdge` to `bfs` and `dfs`.
* Exported `AdjacencyListGraph` and `AdjacencyMatrixGraph`.

0.4.1
-----
* Added CHANGELOG
* Removed my intra-package dependency upper bounds

