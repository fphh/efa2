model GasPedal
  Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-78.7356,9.1954}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-78.7356,9.1954}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon(graphics = {Line(points = {{-35.6322,-9.77011},{36.2069,39.0805},{37.931,35.6322},{-31.6092,-12.069}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{8.04598,14.3678},{12.069,-8.62069},{16.092,-5.74713},{10.9195,16.6667}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}));
  Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {80.4598,5.74713}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {80.4598,5.74713}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain MaximumTorque(k = 100) annotation(Placement(visible = true, transformation(origin = {6.32184,8.04598}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(MaximumTorque.y,y) annotation(Line(points = {{19.5218,8.04598},{71.8391,8.04598},{71.8391,5.74713},{80.4598,5.74713}}));
  connect(u,MaximumTorque.u) annotation(Line(points = {{-78.7356,9.1954},{-6.89655,9.1954},{-6.89655,8.04598},{-8.07816,8.04598}}));
end GasPedal;

