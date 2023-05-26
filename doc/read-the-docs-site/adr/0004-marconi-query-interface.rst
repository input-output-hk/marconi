.. _adr4:

ADR 4: Marconi Query Interface
==============================

Date: 2022-10-27

Author(s)
---------

Kayvan Kazeminejad <kayvan.kazeminejad@iohk.io>

Status
------

Draft

Context
-------

Marconi provides a general solution for indexing the blockchain data.
The query interface adds reporting capabilities on top of Marconi for both Haskell and non-Haskell applications.

Decision
--------

- We will build the interface on top of the Marconi indexer with minimal impact on the Marconi infrastructure

- The query interface may be used both as an executable or a Haskell library

- The query interface supports `JSON-RPC 2.0 <https://www.jsonrpc.org/>`_ on top of HTTP

- The query interface provides reporting of both memory and disk storage of indexers as described in :ref:`adr3`.

Implications
^^^^^^^^^^^^

- No changes to the marconi infrastructure

- we will remain with `SQLite <https://www.sqlite.org/index.html>`_ as our storage engine

