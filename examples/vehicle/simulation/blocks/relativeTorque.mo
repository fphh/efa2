model relativeTorque
  Modelica.Blocks.Interfaces.RealInput torque annotation(Placement(visible = true, transformation(origin = {-76.8908,44.958}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-76.8908,44.958}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-55.4622,63.4454},{57.563,-56.3025}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-41.5966,27.7311},{58.4034,-6.30252}}, textString = "T_rel")}), Diagram(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-100.84,60.084},{-50.8403,65.5462}}, textString = "torque"),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-90.7563,25.2101},{-60.084,17.2269}}, textString = "max"),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-108.824,-17.6471},{-31.9328,-11.7647}}, textString = "min")}));
  Modelica.Blocks.Interfaces.RealInput minTorque annotation(Placement(visible = true, transformation(origin = {-73.9496,-31.9328}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-73.9496,-31.9328}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput maxTorque annotation(Placement(visible = true, transformation(origin = {-70.5882,7.56303}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-76.8908,44.958}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {78.5714,32.3529}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {78.5714,32.3529}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback1 annotation(Placement(visible = true, transformation(origin = {-51.2605,44.958}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback2 annotation(Placement(visible = true, transformation(origin = {-28.1512,7.56302}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Division division1 annotation(Placement(visible = true, transformation(origin = {-2.10082,40.7563}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.Limiter limiter1(uMax = 1, uMin = 0) annotation(Placement(visible = true, transformation(origin = {24.7899,40.7563}, extent = {{-5.59809,-5.59809},{5.59809,5.59809}}, rotation = 0)));
equation
  connect(division1.y,limiter1.u) annotation(Line(points = {{5.35024,40.7563},{45.7983,40.7563},{45.7983,40.7563},{18.0722,40.7563}}));
  connect(limiter1.y,y) annotation(Line(points = {{30.9478,40.7563},{70.5882,40.7563},{70.5882,32.3529},{78.5714,32.3529}}));
  connect(feedback1.y,division1.u1) annotation(Line(points = {{-43.1463,44.958},{-17.2269,44.958},{-17.2269,44.8205},{-10.2292,44.8205}}));
  connect(feedback2.y,division1.u2) annotation(Line(points = {{-20.037,7.56302},{-17.2269,7.56302},{-17.2269,36.692},{-10.2292,36.692}}));
  connect(minTorque,feedback2.u2) annotation(Line(points = {{-73.9496,-31.9328},{-27.731,-31.9328},{-28.1512,-1.61681},{-28.1512,0.3504}}));
  connect(maxTorque,feedback2.u1) annotation(Line(points = {{-70.5882,7.56303},{-45.7983,7.56303},{-45.7983,7.56302},{-35.3638,7.56302}}));
  connect(torque,feedback1.u1) annotation(Line(points = {{-76.8908,44.958},{-45.7983,44.958},{-45.7983,44.958},{-58.4731,44.958}}));
  connect(minTorque,feedback1.u2) annotation(Line(points = {{-73.9496,-31.9328},{-51.2605,-31.9328},{-51.2605,33.6773},{-51.2605,37.7454}}));
end relativeTorque;

