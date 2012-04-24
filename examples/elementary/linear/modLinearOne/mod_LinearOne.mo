model mod_LinearOne
  EFA_Blocks_simple.ETA eta1(eta = 0.9) annotation(Placement(visible = true, transformation(origin = {2.34899,15.1007}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  EFA_Blocks_simple.powerCon powercon1 annotation(Placement(visible = true, transformation(origin = {-22.4832,14.4295}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  EFA_Blocks_simple.powerCon powercon2 annotation(Placement(visible = true, transformation(origin = {29.1946,14.7651}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  EFA_Blocks_simple.signalSource signalsource1(Offset = -0.2, Amplitude = 1, Period = 0.5) annotation(Placement(visible = true, transformation(origin = {-52.6846,13.4228}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(eta1.y,powercon2.u) annotation(Line(points = {{12.2174,14.548},{18.7919,14.548},{18.7919,14.4888},{18.7736,14.4888}}));
  connect(powercon1.y,eta1.u) annotation(Line(points = {{-12.4174,14.2322},{-5.36913,14.2322},{-5.36913,14.9033},{-5.82206,14.9033}}));
  connect(signalsource1.y,powercon1.u) annotation(Line(points = {{-42.303,14.3702},{-32.2148,14.3702},{-32.2148,14.1532},{-32.9043,14.1532}}));
end mod_LinearOne;

