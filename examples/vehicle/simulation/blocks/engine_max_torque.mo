model engine_max_torque
  annotation(Diagram(), Icon(graphics = {Line(points = {{-51.9444,-24.7222},{58.8889,-24.7222},{58.8889,62.2222},{-55.2778,62.5},{-55,-24.1667},{-52.7778,-24.4444}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-37.5,-4.16667},{48.8889,-4.16667}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-41.3889,-4.16667},{-41.1111,58.3333},{-40.8333,58.8889}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-36.3889,27.5},{-23.0556,42.7778},{-6.94444,50.5556},{8.05556,52.5},{25.8333,50.8333},{43.6111,44.4444},{44.7222,43.6111}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-15.2778,26.6667},{34.1667,1.11111}}, textString = "T_max")}), experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  Modelica.Blocks.Nonlinear.DeadZone deadzone1(uMax = ignitionSpeed, uMin = 0) annotation(Placement(visible = true, transformation(origin = {-62.1667,9.72222}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {80.8333,16.1111}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {8,16.1111}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-76.9444,17.2222}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-9,17.2222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Add add2 annotation(Placement(visible = true, transformation(origin = {-1.66667,48.3333}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const(k = +1) annotation(Placement(visible = true, transformation(origin = {-35.5556,28.8889}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Log log1 annotation(Placement(visible = true, transformation(origin = {22.2222,47.5}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Add add1(k2 = +1) annotation(Placement(visible = true, transformation(origin = {49.7222,3.88889}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain3(k = maxT * 0.2) annotation(Placement(visible = true, transformation(origin = {76.6667,38.0556}, extent = {{-5.08917,-5.08917},{5.08917,5.08917}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = -0.001) annotation(Placement(visible = true, transformation(origin = {24.4444,1.66667}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Product product1 annotation(Placement(visible = true, transformation(origin = {-0.555556,1.11111}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain2(k = 0.1) annotation(Placement(visible = true, transformation(origin = {-35.8333,0.277778}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  parameter Real maxT(start = 200, unit = "1") "Maximum Engine Torque" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real ignitionSpeed(start = 30, unit = "1") "Minimum Engine Firing Speed" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(gain2.y,product1.u2) annotation(Line(points = {{-28.3823,0.277778},{-11.1111,0.277778},{-11.1111,-3.80659},{-10.3909,-3.80659}}));
  connect(gain2.y,product1.u1) annotation(Line(points = {{-28.3823,0.277778},{-11.9444,0.277778},{-11.9444,6.02881},{-10.3909,6.02881}}));
  connect(gain2.u,deadzone1.y) annotation(Line(points = {{-43.9618,0.277778},{-54.7222,0.277778},{-54.7222,9.72222},{-54.7156,9.72222}}));
  connect(product1.y,gain1.u) annotation(Line(points = {{8.46022,1.11111},{0,1.11111},{0,1.66667},{13.6255,1.66667}}));
  connect(gain1.y,add1.u2) annotation(Line(points = {{34.3618,1.66667},{39.4444,1.66667},{39.4444,-0.581746},{40.781,-0.581746}}));
  connect(add2.u1,deadzone1.y) annotation(Line(points = {{-10.6079,52.804},{-52.5,52.804},{-52.5,9.72222},{-54.7156,9.72222},{-54.7156,9.72222}}));
  connect(gain3.y,y) annotation(Line(points = {{82.2648,38.0556},{90.5556,38.0556},{90.5556,25.8333},{67.2222,25.8333},{67.2222,15.2778},{80.8333,15.2778},{80.8333,16.1111}}));
  connect(gain3.u,add1.y) annotation(Line(points = {{70.5597,38.0556},{62.5,38.0556},{62.5,4.44444},{57.9184,4.44444},{57.9184,3.88889}}));
  connect(log1.y,add1.u1) annotation(Line(points = {{32.1396,47.5},{39.1667,47.5},{39.1667,8.88889},{40.781,8.88889},{40.781,8.35953}}));
  connect(add2.y,log1.u) annotation(Line(points = {{6.52949,48.3333},{19.7222,48.3333},{19.7222,47.5},{11.4033,47.5}}));
  connect(const.y,add2.u2) annotation(Line(points = {{-25.6382,28.8889},{-11.6667,28.8889},{-11.6667,43.8627},{-10.6079,43.8627}}));
  connect(u,deadzone1.u) annotation(Line(points = {{-90.2778,12.2222},{-70.5556,12.2222},{-70.5556,9.72222},{-70.2951,9.72222}}));
end engine_max_torque;

