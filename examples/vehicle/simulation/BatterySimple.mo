model Battery
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-91.4729,-16.7442},{60.7752,66.0465}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-75.6589,-8.37209},{43.1008,32.2481}}, textString = "Battery"),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-147.597,35.3488},{32.5581,69.4574}}, textString = "+"),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-49.3023,28.8372},{94.5736,75.3488}}, textString = "-")}));
  Modelica.Electrical.Analog.Basic.Resistor resistor1(R = 0.5) annotation(Placement(visible = true, transformation(origin = {-60.9009,9.72973}, extent = {{12,-12},{-12,12}}, rotation = -270)));
  Modelica.Electrical.Analog.Sources.ConstantVoltage constantvoltage1(V = 200) annotation(Placement(visible = true, transformation(origin = {-4.68468,-45.045}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  Modelica.Electrical.Analog.Interfaces.NegativePin pin_n annotation(Placement(visible = true, transformation(origin = {30.1829,78.6585}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {30.1829,78.6585}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Interfaces.PositivePin pin_p annotation(Placement(visible = true, transformation(origin = {-58.8415,78.6585}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-58.8415,78.6585}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(constantvoltage1.n,pin_n) annotation(Line(points = {{7.31532,-45.045},{30.3876,-45.045},{30.3876,78.6585},{30.1829,78.6585}}));
  connect(constantvoltage1.p,resistor1.p) annotation(Line(points = {{-16.6847,-45.045},{-60.155,-45.045},{-60.155,-2.27027},{-60.9009,-2.27027}}));
  connect(resistor1.n,pin_p) annotation(Line(points = {{-60.9009,21.7297},{-58.8415,21.7297},{-58.8415,78.6585},{-58.8415,78.6585}}));
end Battery;

