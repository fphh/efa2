model Driver
  annotation(Diagram(), Icon(graphics = {Ellipse(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 1.75, extent = {{-30.6977,43.4108},{-4.031,17.3643}}),Line(points = {{-18.6046,14.8837},{-11.7829,-45.5814},{28.8372,-35.969},{26.0465,-75.6589},{46.8217,-66.3566}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 1.75),Ellipse(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 1.75, extent = {{23.876,21.3953},{38.1396,-21.3953}}),Line(points = {{-16.1241,-7.44189},{1.86043,-23.2558},{24.8062,-15.1938}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 1.75),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-93.9535,85.2713},{111.318,-80.9302}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-51.1628,52.093},{45.8915,84.3411}}, textString = "Driver")}));
  Modelica.Blocks.Interfaces.RealInput Speed annotation(Placement(visible = true, transformation(origin = {-99.845,-20.155}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-99.845,-20.155}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback1 annotation(Placement(visible = true, transformation(origin = {-27.4694,32.8653}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = 1000000) annotation(Placement(visible = true, transformation(origin = {13.1507,31.625}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain kph_to_mps(k = 1 / 3.6) annotation(Placement(visible = true, transformation(origin = {-66.2204,33.3058}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput SpeedDemand annotation(Placement(visible = true, transformation(origin = {-103.247,35.0388}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-103.247,35.0388}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput GasPedalPosition annotation(Placement(visible = true, transformation(origin = {123.462,31.1789}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {123.462,31.1789}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain ChangeSign(k = -1) annotation(Placement(visible = true, transformation(origin = {134.381,71.4087}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput BrakePedalPosition annotation(Placement(visible = true, transformation(origin = {211.393,70.834}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {211.393,70.834}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.Limiter LimitGas(uMin = -1, uMax = 1) annotation(Placement(visible = true, transformation(origin = {64.841,30.0294}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback2 annotation(Placement(visible = true, transformation(origin = {87.2548,71.9835}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.Limiter LimitBrake(uMin = 0) annotation(Placement(visible = true, transformation(origin = {171.738,71.4087}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(ChangeSign.y,LimitBrake.u) annotation(Line(points = {{147.581,71.4087},{156.795,71.4087},{156.795,71.4087},{157.338,71.4087}}));
  connect(LimitBrake.y,BrakePedalPosition) annotation(Line(points = {{184.938,71.4087},{204.496,71.4087},{204.496,70.834},{211.393,70.834}}));
  connect(feedback2.y,ChangeSign.u) annotation(Line(points = {{98.0548,71.9835},{118.289,71.9835},{118.289,71.4087},{119.981,71.4087}}));
  connect(feedback2.u1,gain1.y) annotation(Line(points = {{77.6548,71.9835},{26.3352,71.9835},{26.3352,31.625},{26.3507,31.625}}));
  connect(feedback2.u2,LimitGas.y) annotation(Line(points = {{87.2548,62.3835},{87.2548,29.4547},{78.041,29.4547},{78.041,30.0294}}));
  connect(LimitGas.y,GasPedalPosition) annotation(Line(points = {{78.041,30.0294},{115.416,30.0294},{115.416,31.1789},{123.462,31.1789}}));
  connect(gain1.y,LimitGas.u) annotation(Line(points = {{26.3507,31.625},{49.8984,31.625},{49.8984,30.0294},{50.441,30.0294}}));
  connect(SpeedDemand,kph_to_mps.u) annotation(Line(points = {{-103.247,35.0388},{-81.7967,35.0388},{-81.7967,33.3058},{-80.6204,33.3058}}));
  connect(kph_to_mps.y,feedback1.u1) annotation(Line(points = {{-53.0204,33.3058},{-38.4945,33.3058},{-38.4945,32.8653},{-37.0694,32.8653}}));
  connect(feedback1.y,gain1.u) annotation(Line(points = {{-16.6694,32.8653},{-1.42291,32.8653},{-1.42291,31.625},{-1.24931,31.625}}));
  connect(Speed,feedback1.u2) annotation(Line(points = {{-99.845,-20.155},{-27.4694,-20.155},{-27.4694,23.8884},{-27.4694,23.2653}}));
end Driver;

