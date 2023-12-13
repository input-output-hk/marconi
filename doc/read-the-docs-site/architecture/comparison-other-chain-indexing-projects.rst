Comparison with other chain-index projects
==========================================

Marconi is a library to define custom indexers and synchronise them with the
node.
Its main objective is to allow users to store exactly what they need, in a
format that suits their logic.

Marconi provides few out-of-the-box indexing solutions and the existing
executables were design either as an example or to suit specific needs, not to
address a general indexing need.

https://github.com/input-output-hk/cardano-node[cardano-node]
-------------------------------------------------------------

``cardano-node`` doesn't offer any kind of indexing out-of-the-box.
However, through the mini-protocols, one can get access to a stream of blocks
that can be processed.
The mini-protocol is what Marconi uses internally to synchronise with the node.

https://github.com/input-output-hk/cardano-db-sync[cardano-db-sync]
-------------------------------------------------------------------

``cardano-db-sync`` is the reference indexing solution for Cardano.
It's exhaustive and is ready to use out of the box.
Compared to Marconi, ``cardano-db-sync`` doesn't offer any way to customise the
indexing (aside filtering out tables that you don't want to index) and
provide no filtering solution.

``cardano-db-sync`` used Postgres as a backend (must be installed and setup
indepedently), while Marconi use a SQLite database embedded in the solution
(and we expect to support other backends in the future).

``cardano-db-sync`` doesn't provide any kind of API on top of it, and many
people uses ``blockfrost`` on top of it to access it through an API.

https://cardanosolutions.github.io/kupo/[Kupo]
----------------------------------------------

Kupo is also a predefined indexing solution. As Marconi, it uses ``SQLite``
as a backend. It also provides facilities to monitor addresses or policy ids
for a short amount of time, but no further customisation.
It doesn't have the modularity of Marconi, but it's an out of the box solutions
that works.


https://github.com/txpipe/scrolls[Scrolls]
------------------------------------------

Aggregator of on-chain data, with a no-sql storage.
Use `node-to-node` protocol to synchronised, which is at the same time
more defensive (thus slower) and more resilient than the node-to-client
protocol of Marconi.
Scrolls offers some predefined aggregator and opts for distributed storage
rather than local storage.
Scrolls is one step further than Marconi in the "one aggregator to
answer one specific question" direction.


https://dcspark.github.io/carp/[Carp]
-------------------------------------

Carp is another modular indexing solution.
It uses postgres database for storage and provides a set of indexers that you
can choose from for indexing. They also provide a REST API to access the data.
