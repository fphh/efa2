model GasPedal
  Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-78.7356,9.1954}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-78.7356,9.1954}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon(graphics = {Line(points = {{-35.6322,-9.77011},{36.2069,39.0805},{37.931,35.6322},{-31.6092,-12.069}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{8.04598,14.3678},{12.069,-8.62069},{16.092,-5.74713},{10.9195,16.6667}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-71.3178,-53.3333},{68.5271,87.4419}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-58.9147,74.7287},{36.2791,51.7829}}, textString = "GasPedal")}));
  Modelica.Blocks.Math.Gain MaximumTorquePlusMinimumTorque(k = 100) annotation(Placement(visible = true, transformation(origin = {-33.908,8.62069}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {80.4598,5.17242}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {80.4598,5.17242}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(MaximumTorquePlusMinimumTorque.y,y) annotation(Line(points = {{-20.708,8.62069},{72.4138,8.62069},{72.4138,5.17242},{80.4598,5.17242}}));
  connect(u,MaximumTorquePlusMinimumTorque.u) annotation(Line(points = {{-78.7356,9.1954},{-57.4713,9.1954},{-57.4713,8.62069},{-48.308,8.62069}}));
end GasPedal;

