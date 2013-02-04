model Battery
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-91.4729,-16.7442},{60.7752,66.0465}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-75.6589,-8.37209},{43.1008,32.2481}}, textString = "Battery"),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-147.597,35.3488},{32.5581,69.4574}}, textString = "+"),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-49.3023,28.8372},{94.5736,75.3488}}, textString = "-")}));
  parameter Real resistance(start = 0.02, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real voltage(start = 200, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Interfaces.PositivePin pin_p annotation(Placement(visible = true, transformation(origin = {-63.3333,79.4444}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-63.3333,79.4444}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Interfaces.NegativePin pin_n annotation(Placement(visible = true, transformation(origin = {25,79.7222}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {25,79.7222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Sources.ConstantVoltage constantvoltage1(V = voltage) annotation(Placement(visible = true, transformation(origin = {-6.38889,-54.7222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Basic.Resistor resistor1(R = resistance) annotation(Placement(visible = true, transformation(origin = {-61.6667,5}, extent = {{-12,12},{12,-12}}, rotation = -90)));
equation
  connect(constantvoltage1.n,pin_n) annotation(Line(points = {{5.61111,-54.7222},{38.8889,-54.7222},{38.8889,78.3333},{25,78.3333},{25,79.7222}}));
  connect(resistor1.n,constantvoltage1.p) annotation(Line(points = {{-61.6667,-7},{-61.1111,-7},{-61.1111,-54.7222},{-18.3889,-54.7222}}));
  connect(pin_p,resistor1.p) annotation(Line(points = {{-63.3333,79.4444},{-61.6667,79.4444},{-61.6667,17},{-61.6667,17}}));
end Battery;

