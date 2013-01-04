model Controller_SeriesHybrid
  Modelica.Blocks.Interfaces.RealInput Battery_SOC annotation(Placement(visible = true, transformation(origin = {-75.969,47.4419}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-75.969,47.4419}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001), Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-97.0543,69.4574},{97.6744,-89.3023}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-85.5814,-31.6279},{48.062,18.6047}}, textString = "Series Hybrid Control")}));
  Modelica.Blocks.Interfaces.RealOutput TorqueDemand_Engine annotation(Placement(visible = true, transformation(origin = {71.6279,-4.96124}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {71.6279,-4.96124}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput TorqueDemand_Motor annotation(Placement(visible = true, transformation(origin = {66.0465,-44.3411}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {71.6279,-44.3411}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Logical.Not not1 annotation(Placement(visible = true, transformation(origin = {-4.65116,80}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Modelica.Blocks.Logical.Switch switch1_engine annotation(Placement(visible = true, transformation(origin = {43.7209,-3.72093}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Modelica.Blocks.Logical.Switch switch2_motor annotation(Placement(visible = true, transformation(origin = {40.3101,-44.3411}, extent = {{-5.59809,-5.59809},{5.59809,5.59809}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant zero(k = 0) annotation(Placement(visible = true, transformation(origin = {-5.5814,-12.093}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = 10) annotation(Placement(visible = true, transformation(origin = {-17.0543,-59.845}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant Torque_Demand(k = 100) annotation(Placement(visible = true, transformation(origin = {-31.6279,17.9845}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-75.6589,-68.8372}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-75.6589,-68.8372}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant speed_Demand(k = 150) annotation(Placement(visible = true, transformation(origin = {-72.2481,-48.6822}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback1 annotation(Placement(visible = true, transformation(origin = {-43.7209,-49.6124}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Modelica.Blocks.Logical.Hysteresis hysteresis_lowSOC(uLow = 0.4, uHigh = 0.5) annotation(Placement(visible = true, transformation(origin = {-31.938,80}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Interfaces.BooleanOutput y annotation(Placement(visible = true, transformation(origin = {66.0465,-25.1163}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {66.0465,-25.1163}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(y,not1.y) annotation(Line(points = {{66.0465,-25.1163},{52.7132,-25.1163},{52.7132,80},{2.12253,80}}));
  connect(hysteresis_lowSOC.u,Battery_SOC) annotation(Line(points = {{-40.8793,80},{-66.3566,80},{-66.3566,47.4419},{-75.969,47.4419}}));
  connect(hysteresis_lowSOC.y,not1.u) annotation(Line(points = {{-23.7418,80},{-13.6434,80},{-13.6434,80},{-12.0406,80}}));
  connect(switch2_motor.u2,not1.y) annotation(Line(points = {{33.5924,-44.3411},{28.8372,-44.3411},{28.8372,80.3101},{2.12253,80.3101},{2.12253,80}}));
  connect(not1.y,switch1_engine.u2) annotation(Line(points = {{2.12253,80},{28.8372,80},{28.8372,-3.72093},{36.3314,-3.72093}}));
  connect(u,feedback1.u2) annotation(Line(points = {{-75.6589,-68.8372},{-42.1705,-68.8372},{-42.1705,-54.5387},{-43.7209,-54.5387}}));
  connect(speed_Demand.y,feedback1.u1) annotation(Line(points = {{-64.0519,-48.6822},{-50.8527,-48.6822},{-50.8527,-49.6124},{-48.6472,-49.6124}}));
  connect(Torque_Demand.y,switch1_engine.u1) annotation(Line(points = {{-22.6121,17.9845},{23.876,17.9845},{23.876,0.930233},{36.3314,0.930233},{36.3314,1.20539}}));
  connect(gain1.y,switch2_motor.u1) annotation(Line(points = {{-9.60321,-59.845},{25.1163,-59.845},{25.1163,-40},{33.5924,-40},{33.5924,-39.8626}}));
  connect(feedback1.y,gain1.u) annotation(Line(points = {{-36.0083,-59.2248},{-25.1163,-59.2248},{-25.1163,-59.845},{-25.1827,-59.845}}));
  connect(switch2_motor.u3,zero.y) annotation(Line(points = {{33.5924,-48.8196},{9.92248,-48.8196},{9.92248,-12.4031},{2.61477,-12.4031},{2.61477,-12.093}}));
  connect(zero.y,switch1_engine.u3) annotation(Line(points = {{2.61477,-12.093},{35.6589,-12.093},{35.6589,-8.64725},{36.3314,-8.64725}}));
  connect(switch2_motor.y,TorqueDemand_Motor) annotation(Line(points = {{46.468,-44.3411},{60.155,-44.3411},{60.155,-44.3411},{66.0465,-44.3411}}));
  connect(switch1_engine.y,TorqueDemand_Engine) annotation(Line(points = {{50.4946,-3.72093},{61.0853,-3.72093},{61.0853,-4.96124},{71.6279,-4.96124}}));
end Controller_SeriesHybrid;

