block Efficiency
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-62.069,29.8851},{56.8966,-27.5862}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-47.1264,-13.7931},{52.2989,22.9885}}, textString = "Eta")}));
  parameter Real eta(start = 1, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Logical.Not not1 annotation(Placement(visible = true, transformation(origin = {6.94444,38.6111}, extent = {{-9.91736,-9.91736},{9.91736,9.91736}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant Efficiency(k = eta) annotation(Placement(visible = true, transformation(origin = {-83.2042,78.1482}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Logical.GreaterEqual greaterequal1 annotation(Placement(visible = true, transformation(origin = {-29.8851,38.5057}, extent = {{-10.9091,-10.9091},{10.9091,10.9091}}, rotation = 0)));
  Modelica.Blocks.Logical.Switch switch1 annotation(Placement(visible = true, transformation(origin = {47.7011,38.5057}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Product product1 annotation(Placement(visible = true, transformation(origin = {8.04598,72.4138}, extent = {{-9.91736,-9.91736},{9.91736,9.91736}}, rotation = 0)));
  Modelica.Blocks.Math.Division division1 annotation(Placement(visible = true, transformation(origin = {9.1954,-15.5172}, extent = {{-9.91736,-9.91736},{9.91736,9.91736}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant Zero(k = 0) annotation(Placement(visible = true, transformation(origin = {-82.2877,29.3356}, extent = {{-9.91736,-9.91736},{9.91736,9.91736}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-77.4723,-1.77473}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-77.4723,-1.77473}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {79.2344,5.51673}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {79.2344,5.51673}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(u,product1.u2) annotation(Line(points = {{-77.4723,-1.77473},{-63.1424,-1.77473},{-63.1424,66.9604},{-3.85485,66.9604},{-3.85485,66.4634}}));
  connect(u,division1.u1) annotation(Line(points = {{-77.4723,-1.77473},{-3.81791,-1.77473},{-3.81791,-9.56678},{-2.70543,-9.56678}}));
  connect(division1.u2,Efficiency.y) annotation(Line(points = {{-2.70543,-21.4676},{-52.2761,-21.4676},{-52.2761,78.1204},{-73.2868,78.1204},{-73.2868,78.1482}}));
  connect(Efficiency.y,product1.u1) annotation(Line(points = {{-73.2868,78.1482},{-4.40529,78.1482},{-4.40529,78.3642},{-3.85485,78.3642}}));
  connect(switch1.y,y) annotation(Line(points = {{57.6185,38.5057},{68.1351,38.5057},{68.1351,5.51673},{79.2344,5.51673}}));
  connect(product1.y,switch1.u1) annotation(Line(points = {{18.9551,72.4138},{27.0115,72.4138},{27.0115,45.0454},{36.8822,45.0454},{36.8822,45.7183}}));
  connect(u,greaterequal1.u1) annotation(Line(points = {{-77.4723,-1.77473},{-58.6207,-1.77473},{-58.6207,39.0805},{-42.976,39.0805},{-42.976,38.5057}}));
  connect(Zero.y,greaterequal1.u2) annotation(Line(points = {{-71.3786,29.3356},{-44.8276,29.3356},{-44.8276,28.1609},{-42.976,28.1609},{-42.976,29.7784}}));
  connect(division1.y,switch1.u3) annotation(Line(points = {{20.1045,-15.5172},{27.5862,-15.5172},{27.5862,29.3103},{36.8822,29.3103},{36.8822,31.2931}}));
  connect(not1.y,switch1.u2) annotation(Line(points = {{17.8535,38.6111},{32.7778,38.6111},{32.7778,38.5057},{36.8822,38.5057}}));
  connect(greaterequal1.y,not1.u) annotation(Line(points = {{-17.8851,38.5057},{-6.11111,38.5057},{-6.11111,38.6111},{-4.95639,38.6111}}));
end Efficiency;

