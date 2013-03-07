

module EFA.Hack.Draw where



topology2pdf :: (Node.C node) => Topo.Topology node -> IO (FilePath)
topology2pdf topo =
   runGraphvizCommand Dot (dotFromTopology topo) Pdf "result/topology.pdf"
