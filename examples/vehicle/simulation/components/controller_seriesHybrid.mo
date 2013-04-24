model Controller_SeriesHybrid
  annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001), Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-97.0543,69.4574},{97.6744,-89.3023}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-67.907,-15.5039},{47.7519,-46.8217}}, textString = "Control"),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-59.2248,18.9147},{37.2093,-3.72093}}, textString = "Series Hybrid"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{54.7222,13.8889},{94.1667,-13.8889}})}));
  Modelica.Blocks.Logical.Hysteresis hysteresis_lowSOC(uLow = minSOC, uHigh = maxSOC, pre_y_start = not engineOnAtStart) annotation(Placement(visible = true, transformation(origin = {-31.3825,47.7778}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Logical.Not not1 annotation(Placement(visible = true, transformation(origin = {-2.42894,47.7778}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  ControlBus controlbus1 annotation(Placement(visible = true, transformation(origin = {74.7222,0}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {74.7222,0}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Control_MoGen control_mogen1 annotation(Placement(visible = true, transformation(origin = {35.2778,2.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_torqueDemand(k = torqueDemand) annotation(Placement(visible = true, transformation(origin = {-16.3501,3.81783}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_speedDemand(k = speedDemand) annotation(Placement(visible = true, transformation(origin = {-16.6925,-22.8489}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_electricMotor(k = 1) annotation(Placement(visible = true, transformation(origin = {-5.83333,-61.9444}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real speedDemand(start = 300);
  parameter Real torqueDemand(start = 150);
  parameter Real maxSOC(start = 0.9);
  parameter Real minSOC(start = 0.3);
  parameter Boolean engineOnAtStart(start = false);
equation
  connect(const_speedDemand.y,control_mogen1.input_speedDemand) annotation(Line(points = {{-8.49633,-22.8489},{9.44444,-22.8489},{9.44444,-0.555556},{25.3778,-0.555556},{25.3778,-0.688868}}));
  connect(const_torqueDemand.y,control_mogen1.input_torqueDemand) annotation(Line(points = {{-7.33432,3.81783},{25.5556,3.81783},{25.5556,4.04446},{25.4778,4.04446}}));
  connect(not1.y,control_mogen1.input_switchOn) annotation(Line(points = {{4.34475,47.7778},{25,47.7778},{25,8.31113},{25.5111,8.31113}}));
  connect(control_mogen1.output_engineOn,controlbus1.engineBus.switchOn) annotation(Line(points = {{4.34475,47.7778},{65.5556,47.7778},{65.5556,0.277778},{66.7111,0.277778}}));
  connect(control_mogen1.output_generatorTorque,controlbus1.generatorBus.torqueDemand) annotation(Line(points = {{49.5236,-44.3411},{63.0556,-44.3411},{63.0556,0.277778},{63.6556,0.277778}}));
  connect(control_mogen1.output_engineTorque,controlbus1.engineBus.torqueDemand) annotation(Line(points = {{64.4889,-0.833333},{50.8333,-0.833333},{50.8333,-3.72093},{50.4946,-3.72093}}));
  connect(hysteresis_lowSOC.y,not1.u) annotation(Line(points = {{-23.1863,47.7778},{-13.6434,47.7778},{-13.6434,47.7778},{-9.81842,47.7778}}));
  connect(hysteresis_lowSOC.u,controlbus1.batteryBus.SOC) annotation(Line(points = {{-40.3237,47.7778},{-58.0233,47.7778},{-58.0233,47.4419},{-75.969,47.4419}}));
  connect(controlbus1.engineBus.speed,control_mogen1.input_speed) annotation(Line(points = {{-75.6589,-68.8372},{-42.1705,-68.8372},{-42.1705,-54.5387},{-43.7209,-54.5387}}));
  connect(gain_electricMotor.u,controlbus1.vehicleBus.gasPedalDemand) annotation(Line(points = {{4.34475,47.7778},{65.5556,47.7778},{65.5556,0.277778},{66.7111,0.277778}}));
  connect(gain_electricMotor.y,controlbus1.motorBus.torqueDemand) annotation(Line(points = {{4.34475,47.7778},{65.5556,47.7778},{65.5556,0.277778},{66.7111,0.277778}}));
end Controller_SeriesHybrid;

