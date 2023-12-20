# marconi-core

`marconi-core` is a Haskell library to ease indexing of distributed ledger
that has a settlement time
(ie. rollback can happen and invalidates the last indexed blocks).

While `marconi` was designed to target the Cardano blockchain,
the content of `marconi-core` is totally chain agnostic and does not rely on
anything Cardano specific type.

If you want to develop your own indexing solution for Cardano using `marconi`,
you'll probably want to use other specific libraries aside `marconi-core` that
will ease the integration on Cardano
(like [marconi-caradano-core](../marconi-cardano-core) that provides content to
ease events extraction and connection to a node or
[marconi-cardano-indexers](../marconi-cardano-indexers)).

## Philosophy

`Marconi` eased the definition and composition of atomic indexers,
each of them tailored to answer efficiently specific questions.
The underlying idea is that a dApp usually requires specific information
and in the fastest possible way.
To address this, we need tailored indexers, extracting exactly what is needed to
provide this information and store most efficiently.

## What's in there?

A description of `marconi-core` key concepts is available in
[the documentation](../doc/read-the-docs-site/architecture/marconi-core-building-blocks.rst).
