model control_DCTClutches
  Modelica.Blocks.Interfaces.RealInput torqueDemand annotation(Placement(visible = true, transformation(origin = {-91.3889,10}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-91.3889,10}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon());
  Modelica.Blocks.Math.Gain gain1 annotation(Placement(visible = true, transformation(origin = {34.1667,9.44444}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Interfaces.IntegerInput gearSelect annotation(Placement(visible = true, transformation(origin = {-84.7222,48.8889}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-91.3889,10}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.SlewRateLimiter slewratelimiter1 annotation(Placement(visible = true, transformation(origin = {-25.8333,-6.11111}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.StateGraph.Step step1 annotation(Placement(visible = true, transformation(origin = {-11.9444,60}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.StateGraph.StepWithSignal stepwithsignal1 annotation(Placement(visible = true, transformation(origin = {-42.7778,43.8889}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.StateGraph.TransitionWithSignal transitionwithsignal1 annotation(Placement(visible = true, transformation(origin = {27.5,43.6111}, extent = {{-12,-12},{12,12}}, rotation = 0)));
end control_DCTClutches;

