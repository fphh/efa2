block Efficiency
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-62.069,29.8851},{56.8966,-27.5862}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-47.1264,-13.7931},{52.2989,22.9885}}, textString = "Eta")}));
  Modelica.Blocks.Logical.GreaterEqual greaterequal1 annotation(Placement(visible = true, transformation(origin = {-29.8851,38.5057}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant Zero(k = 0) annotation(Placement(visible = true, transformation(origin = {-90.8046,28.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {91.3793,-67.8161}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {82.7586,2.87356}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant Efficiency(k = eta) annotation(Placement(visible = true, transformation(origin = {-75.8621,78.7356}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Logical.Switch switch1 annotation(Placement(visible = true, transformation(origin = {47.7011,38.5057}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Product product1 annotation(Placement(visible = true, transformation(origin = {8.04598,72.4138}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Division division1 annotation(Placement(visible = true, transformation(origin = {9.1954,-15.5172}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real eta(start = 1, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-82.7586,0.574754}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-82.7586,0.574754}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(division1.y,switch1.u3) annotation(Line(points = {{22.3954,-15.5172},{27.5862,-15.5172},{27.5862,29.3103},{33.3011,29.3103},{33.3011,28.9057}}));
  connect(u,greaterequal1.u1) annotation(Line(points = {{-82.7586,0.574754},{-58.6207,0.574754},{-58.6207,39.0805},{-44.2851,39.0805},{-44.2851,38.5057}}));
  connect(division1.u1,u) annotation(Line(points = {{-5.2046,-8.3172},{-73.5632,-8.3172},{-73.5632,0.574754},{-82.7586,0.574754}}));
  connect(product1.u2,u) annotation(Line(points = {{-6.35402,65.2138},{-64.9425,65.2138},{-64.9425,-62.6437},{-82.7586,-62.6437},{-82.7586,0.574754}}));
  connect(product1.u1,Efficiency.y) annotation(Line(points = {{-6.35402,79.6138},{-62.069,79.6138},{-62.069,78.7356},{-62.6621,78.7356},{-62.6621,78.7356}}));
  connect(division1.u2,Efficiency.y) annotation(Line(points = {{-5.2046,-22.7172},{-50.5747,-22.7172},{-50.5747,78.1609},{-62.6621,78.1609},{-62.6621,78.7356}}));
  connect(product1.y,switch1.u1) annotation(Line(points = {{21.246,72.4138},{27.0115,72.4138},{27.0115,48.2759},{33.3011,48.2759},{33.3011,48.1057}}));
  connect(switch1.y,y) annotation(Line(points = {{60.9011,38.5057},{73.5632,38.5057},{73.5632,-69.5402},{86.7816,-69.5402},{86.7816,-67.8161},{91.3793,-67.8161}}));
  connect(greaterequal1.y,switch1.u2) annotation(Line(points = {{-16.6851,38.5057},{0.574713,38.5057},{0.574713,38.5057},{33.3011,38.5057}}));
  connect(Zero.y,greaterequal1.u2) annotation(Line(points = {{-77.6046,28.1609},{-44.8276,28.1609},{-44.8276,28.1609},{-44.2851,28.1609},{-44.2851,28.9057}}));
end Efficiency;

