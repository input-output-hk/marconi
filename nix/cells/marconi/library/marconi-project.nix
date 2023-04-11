{ inputs, cell }:

cell.library.make-marconi-project {
  deferPluginErrors = false;
  enableHaskellProfiling = false;
}
