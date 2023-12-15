.. _adr7:

ADR 7: Marconi observability
============================

Date: 2023-07-19

Authors
-------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Draft

Context
-------

Whenever we add new features in Marconi, we inevitably end up in a state where we introduce a significant regression in sync time, CPU/RAM/Disk usage and query performance.
Currently, developers who are adding new features will typically test those features by running ``marconi-cardano-chain-index`` or ``marconi-sidechain``, and manually visualize the CPU/RAM usage by running ``htop`` (or similar), checking syncing speed by reading the logs, and manually run queries after syncing is done.
However, that approach does not give accurate information for detecting regressions, because we only look at the logs at specific points in time and we might miss peaks in memory usage for example.
Moreover, this testing approach does not scale for delivering on time the different features.

Therefore, we need a solution for observing the runtime behavior of Marconi through different metrics such blocks processed, query time, CPU/RAM/Disk usage, etc.
Ideally, these observations would be done through graphical visualizations.
Eventually, they would be used for external reporting and as part of our QA practices.

Decision
--------

* We will expose Prometheus metrics for our chain-indexers, more specifically, ``marconi-sidechain``.
  The metrics we should provide are:

  * CPU, memory and disk usage
  * Total number of processed blocks
  * The number of processed queries
  * The time spent processing queries

  Except the CPU/RAM/Disk usage which are provided by the systemd process, the application-specific metrics will be exposed in an HTTP endpoint such as ``/metrics``.

* We will expose Grafana dashboards to visualize each of the metrics.

Argument
--------

Grafana and Prometheus seem to be the go-to framekwork for observing behavior of long-running applications, and it is used by other projects in IOG, most notably by the Sidechain Tribe.
As we're delivering ``marconi-sidechain`` for them, it makes sense to use the same framework.
