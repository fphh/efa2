model Driver
  annotation(Diagram(), Icon(graphics = {Ellipse(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-23.876,72.8682},{2.7907,46.8217}}),Line(points = {{-10.5426,43.1008},{-9.30233,-14.8837},{26.9767,-5.5814},{28.5271,-44.9612},{44.3411,-34.7287}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 1.25),Line(points = {{65.1163,-33.4884},{22.9457,-57.6744}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{51.4729,-41.2403},{53.6434,-59.2248}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Ellipse(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{26.3566,52.093},{40.6202,9.30233}}),Line(points = {{-9.92248,14.5736},{10.5426,16.4341},{27.5969,27.2868}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}));
  Modelica.Blocks.Interfaces.RealInput Speed annotation(Placement(visible = true, transformation(origin = {-71.0078,-26.0465}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-71.0078,-26.0465}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback1 annotation(Placement(visible = true, transformation(origin = {1.36783,26.9738}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = 1000) annotation(Placement(visible = true, transformation(origin = {41.9879,25.7335}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain kph_to_mps(k = 1 / 3.6) annotation(Placement(visible = true, transformation(origin = {-37.3832,27.4143}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput SpeedDemand annotation(Placement(visible = true, transformation(origin = {-74.4099,29.1473}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-74.4099,29.1473}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput GasPedalPosition annotation(Placement(visible = true, transformation(origin = {152.299,25.2874}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {80,24.8062}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.Limiter limiter1(uMin = 0) annotation(Placement(visible = true, transformation(origin = {94.2529,24.7126}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback2 annotation(Placement(visible = true, transformation(origin = {122.989,65.5172}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput BrakePedalPosition annotation(Placement(visible = true, transformation(origin = {199.425,64.3678}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {199.425,64.3678}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(feedback2.y,BrakePedalPosition) annotation(Line(points = {{133.789,65.5172},{188.506,65.5172},{188.506,64.3678},{199.425,64.3678}}));
  connect(feedback2.u1,gain1.y) annotation(Line(points = {{113.389,65.5172},{55.1724,65.5172},{55.1724,25.7335},{55.1879,25.7335}}));
  connect(feedback2.u2,limiter1.y) annotation(Line(points = {{122.989,55.9172},{122.989,24.7126},{107.453,24.7126},{107.453,24.7126}}));
  connect(limiter1.y,GasPedalPosition) annotation(Line(points = {{107.453,24.7126},{144.253,24.7126},{144.253,25.2874},{152.299,25.2874}}));
  connect(gain1.y,limiter1.u) annotation(Line(points = {{55.1879,25.7335},{78.7356,25.7335},{78.7356,24.7126},{79.8529,24.7126}}));
  connect(SpeedDemand,kph_to_mps.u) annotation(Line(points = {{-74.4099,29.1473},{-52.9595,29.1473},{-52.9595,27.4143},{-51.7832,27.4143}}));
  connect(kph_to_mps.y,feedback1.u1) annotation(Line(points = {{-24.1832,27.4143},{-9.65732,27.4143},{-9.65732,26.9738},{-8.23217,26.9738}}));
  connect(feedback1.y,gain1.u) annotation(Line(points = {{12.1678,26.9738},{27.4143,26.9738},{27.4143,25.7335},{27.5879,25.7335}}));
  connect(Speed,feedback1.u2) annotation(Line(points = {{-71.0078,-26.0465},{1.36783,-26.0465},{1.36783,17.9969},{1.36783,17.3738}}));
end Driver;

