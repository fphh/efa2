model Controller_SeriesHybrid
  annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001), Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-97.0543,69.4574},{97.6744,-89.3023}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-67.907,-15.5039},{47.7519,-46.8217}}, textString = "Control"),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-59.2248,18.9147},{37.2093,-3.72093}}, textString = "Series Hybrid"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{54.7222,13.8889},{94.1667,-13.8889}})}));
  Modelica.Blocks.Logical.Switch switch_engine annotation(Placement(visible = true, transformation(origin = {43.7209,-3.72093}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_zero(k = 0) annotation(Placement(visible = true, transformation(origin = {-5.5814,-12.093}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_speedControl(k = 5) annotation(Placement(visible = true, transformation(origin = {-17.0543,-59.845}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_speedDemand(k = 150) annotation(Placement(visible = true, transformation(origin = {-72.2481,-48.6822}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback_speed annotation(Placement(visible = true, transformation(origin = {-43.7209,-49.6124}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_torqueDemand(k = 200) annotation(Placement(visible = true, transformation(origin = {-7.18345,15.4845}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Logical.Switch switch_motor annotation(Placement(visible = true, transformation(origin = {43.3657,-44.3411}, extent = {{-5.59809,-5.59809},{5.59809,5.59809}}, rotation = 0)));
  Modelica.Blocks.Logical.Hysteresis hysteresis_lowSOC(uLow = 0.2, uHigh = 0.8, pre_y_start = true) annotation(Placement(visible = true, transformation(origin = {-31.3825,47.7778}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Logical.Not not1 annotation(Placement(visible = true, transformation(origin = {-2.42894,47.7778}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  ControlBus controlbus1 annotation(Placement(visible = true, transformation(origin = {74.7222,0}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {74.7222,0}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_electricMotor annotation(Placement(visible = true, transformation(origin = {-14.4444,-84.1667}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
equation
  connect(not1.y,controlbus1.engineBus.switchOn) annotation(Line(points = {{4.34475,47.7778},{65.5556,47.7778},{65.5556,0.277778},{66.7111,0.277778}}));
  connect(switch_motor.y,controlbus1.generatorBus.torqueDemand) annotation(Line(points = {{49.5236,-44.3411},{63.0556,-44.3411},{63.0556,0.277778},{63.6556,0.277778}}));
  connect(switch_engine.y,controlbus1.engineBus.torqueDemand) annotation(Line(points = {{64.4889,-0.833333},{50.8333,-0.833333},{50.8333,-3.72093},{50.4946,-3.72093}}));
  connect(switch_motor.u2,not1.y) annotation(Line(points = {{36.6479,-44.3411},{28.8372,-44.3411},{28.8372,47.8101},{4.34475,47.8101},{4.34475,47.7778}}));
  connect(not1.y,switch_engine.u2) annotation(Line(points = {{4.34475,47.7778},{28.8372,47.7778},{28.8372,-3.72093},{36.3314,-3.72093}}));
  connect(hysteresis_lowSOC.y,not1.u) annotation(Line(points = {{-23.1863,47.7778},{-13.6434,47.7778},{-13.6434,47.7778},{-9.81842,47.7778}}));
  connect(hysteresis_lowSOC.u,controlbus1.batteryBus.SOC) annotation(Line(points = {{-40.3237,47.7778},{-58.0233,47.7778},{-58.0233,47.4419},{-75.969,47.4419}}));
  connect(switch_motor.u3,const_zero.y) annotation(Line(points = {{36.6479,-48.8196},{9.92248,-48.8196},{9.92248,-12.4031},{2.61477,-12.4031},{2.61477,-12.093}}));
  connect(gain_speedControl.y,switch_motor.u1) annotation(Line(points = {{-9.60324,-59.845},{25.1163,-59.845},{25.1163,-40},{36.6479,-40},{36.6479,-39.8626}}));
  connect(const_torqueDemand.y,switch_engine.u1) annotation(Line(points = {{1.83232,15.4845},{23.876,15.4845},{23.876,0.930233},{36.3314,0.930233},{36.3314,1.20539}}));
  connect(controlbus1.engineBus.speed,feedback_speed.u2) annotation(Line(points = {{-75.6589,-68.8372},{-42.1705,-68.8372},{-42.1705,-54.5387},{-43.7209,-54.5387}}));
  connect(const_speedDemand.y,feedback_speed.u1) annotation(Line(points = {{-64.0519,-48.6822},{-50.8527,-48.6822},{-50.8527,-49.6124},{-48.6472,-49.6124}}));
  connect(feedback_speed.y,gain_speedControl.u) annotation(Line(points = {{-36.0083,-59.2248},{-25.1163,-59.2248},{-25.1163,-59.845},{-25.1827,-59.845}}));
  connect(const_zero.y,switch_engine.u3) annotation(Line(points = {{2.61477,-12.093},{35.6589,-12.093},{35.6589,-8.64725},{36.3314,-8.64725}}));
  connect(gain_electricMotor.u,controlbus1.vehicleBus.gasPedalDemand) annotation(Line(points = {{4.34475,47.7778},{65.5556,47.7778},{65.5556,0.277778},{66.7111,0.277778}}));
  connect(gain_electricMotor.y,controlbus1.motorBus.torqueDemand) annotation(Line(points = {{4.34475,47.7778},{65.5556,47.7778},{65.5556,0.277778},{66.7111,0.277778}}));
end Controller_SeriesHybrid;

