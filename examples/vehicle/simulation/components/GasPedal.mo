model GasPedal
  Modelica.Blocks.Interfaces.RealInput input_position annotation(Placement(visible = true, transformation(origin = {-78.7356,9.1954}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-78.7356,9.1954}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon(graphics = {Line(points = {{-35.6322,-9.77011},{36.2069,39.0805},{37.931,35.6322},{-31.6092,-12.069}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{8.04598,14.3678},{12.069,-8.62069},{16.092,-5.74713},{10.9195,16.6667}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-71.3178,-53.3333},{68.5271,87.4419}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-58.9147,74.7287},{36.2791,51.7829}}, textString = "GasPedal")}));
  Modelica.Blocks.Interfaces.RealOutput output_demand annotation(Placement(visible = true, transformation(origin = {80.4598,5.17242}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {80.4598,5.17242}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_maximumTorquePlusMinimumTorque(k = 100000) annotation(Placement(visible = true, transformation(origin = {-27.5191,8.89847}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _demand_log annotation(Placement(visible = true, transformation(origin = {75,77.1552}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _position_log annotation(Placement(visible = true, transformation(origin = {75.431,78.0172}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(input_position,gain_maximumTorquePlusMinimumTorque.u) annotation(Line(points = {{-78.7356,9.1954},{-57.4713,9.1954},{-57.4713,8.89847},{-41.9191,8.89847}}));
  connect(gain_maximumTorquePlusMinimumTorque.y,output_demand) annotation(Line(points = {{-14.3191,8.89847},{72.4138,8.89847},{72.4138,5.17242},{80.4598,5.17242}}));
  _position_log = input_position;
  _demand_log = output_demand;
end GasPedal;

