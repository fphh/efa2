

Loop.checkRangeIO sysParams optParams simParams]


drawTopologyWithStates = concurrentlyMany_ [
  Draw.xterm $ Draw.labeledTopology $ System.labeledTopology,
  Draw.xterm $
  Draw.flowTopologies $
  StateAnalysis.advanced System.topology ]