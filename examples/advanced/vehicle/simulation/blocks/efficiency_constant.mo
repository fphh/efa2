block Efficiency
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-62.069,29.8851},{56.8966,-27.5862}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-47.1264,-13.7931},{52.2989,22.9885}}, textString = "Eta")}));
  parameter Real eta(start = 1, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Logical.Not not1 annotation(Placement(visible = true, transformation(origin = {6.94444,38.6111}, extent = {{-9.91736,-9.91736},{9.91736,9.91736}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_efficiency(k = eta) annotation(Placement(visible = true, transformation(origin = {-83.2042,78.1482}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Logical.GreaterEqual greaterequal_zeroPowerFlow annotation(Placement(visible = true, transformation(origin = {-29.8851,38.5057}, extent = {{-10.9091,-10.9091},{10.9091,10.9091}}, rotation = 0)));
  Modelica.Blocks.Logical.Switch switch_flowCase annotation(Placement(visible = true, transformation(origin = {47.7011,38.5057}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Product product_negPowerFlow annotation(Placement(visible = true, transformation(origin = {8.04598,72.4138}, extent = {{-9.91736,-9.91736},{9.91736,9.91736}}, rotation = 0)));
  Modelica.Blocks.Math.Division division_posPowerFlow annotation(Placement(visible = true, transformation(origin = {9.1954,-15.5172}, extent = {{-9.91736,-9.91736},{9.91736,9.91736}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_zero(k = 0) annotation(Placement(visible = true, transformation(origin = {-82.2877,29.3356}, extent = {{-9.91736,-9.91736},{9.91736,9.91736}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput realInport_power annotation(Placement(visible = true, transformation(origin = {-77.4723,-1.77473}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-77.4723,-1.77473}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput realOutport_powerAfterEfficiency annotation(Placement(visible = true, transformation(origin = {79.2344,5.51673}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {79.2344,5.51673}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(realInport_power,product_negPowerFlow.u2) annotation(Line(points = {{-77.4723,-1.77473},{-63.1424,-1.77473},{-63.1424,66.9604},{-3.85485,66.9604},{-3.85485,66.4634}}));
  connect(realInport_power,division_posPowerFlow.u1) annotation(Line(points = {{-77.4723,-1.77473},{-3.81791,-1.77473},{-3.81791,-9.56678},{-2.70543,-9.56678}}));
  connect(division_posPowerFlow.u2,const_efficiency.y) annotation(Line(points = {{-2.70543,-21.4676},{-52.2761,-21.4676},{-52.2761,78.1204},{-73.2868,78.1204},{-73.2868,78.1482}}));
  connect(const_efficiency.y,product_negPowerFlow.u1) annotation(Line(points = {{-73.2868,78.1482},{-4.40529,78.1482},{-4.40529,78.3642},{-3.85485,78.3642}}));
  connect(switch_flowCase.y,realOutport_powerAfterEfficiency) annotation(Line(points = {{57.6185,38.5057},{68.1351,38.5057},{68.1351,5.51673},{79.2344,5.51673}}));
  connect(product_negPowerFlow.y,switch_flowCase.u1) annotation(Line(points = {{18.9551,72.4138},{27.0115,72.4138},{27.0115,45.0454},{36.8822,45.0454},{36.8822,45.7183}}));
  connect(realInport_power,greaterequal_zeroPowerFlow.u1) annotation(Line(points = {{-77.4723,-1.77473},{-58.6207,-1.77473},{-58.6207,39.0805},{-42.976,39.0805},{-42.976,38.5057}}));
  connect(const_zero.y,greaterequal_zeroPowerFlow.u2) annotation(Line(points = {{-71.3786,29.3356},{-44.8276,29.3356},{-44.8276,28.1609},{-42.976,28.1609},{-42.976,29.7784}}));
  connect(division_posPowerFlow.y,switch_flowCase.u3) annotation(Line(points = {{20.1045,-15.5172},{27.5862,-15.5172},{27.5862,29.3103},{36.8822,29.3103},{36.8822,31.2931}}));
  connect(not1.y,switch_flowCase.u2) annotation(Line(points = {{17.8535,38.6111},{32.7778,38.6111},{32.7778,38.5057},{36.8822,38.5057}}));
  connect(greaterequal_zeroPowerFlow.y,not1.u) annotation(Line(points = {{-17.8851,38.5057},{-6.11111,38.5057},{-6.11111,38.6111},{-4.95639,38.6111}}));
end Efficiency;

