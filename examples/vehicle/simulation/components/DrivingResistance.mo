model DrivingResistance
  annotation(Diagram(), Icon(graphics = {Line(points = {{-36.5892,14.8837},{-6.51162,18.2946},{19.5349,23.5659},{35.0387,31.0077},{49.6124,39.6899},{59.845,50.5426},{68.217,60.4651}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-36.2791,14.8837},{69.1473,14.8837},{69.4573,14.8837}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-36.8992,14.5736},{-36.8992,78.7597}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-26.6666,86.2016},{66.6666,68.8372}}, textString = "Driving Resistance"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-48.6822,3.10078},{95.814,98.6047}})}), experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  Modelica.Mechanics.Translational.Sources.Force force1 annotation(Placement(visible = true, transformation(origin = {-23.2558,37.5194}, extent = {{12,12},{-12,-12}}, rotation = 180)));
  Modelica.Mechanics.Translational.Interfaces.Flange_a flange_a annotation(Placement(visible = true, transformation(origin = {-65.7364,36.5891}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-65.7364,36.5891}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Product product1 annotation(Placement(visible = true, transformation(origin = {-1.86047,-71.0078}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain2(k = 0.5 * cwA * 1.119) annotation(Placement(visible = true, transformation(origin = {30.3876,-71.0078}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.Limiter limiter1(uMax = 1) annotation(Placement(visible = true, transformation(origin = {5.5814,-31.6279}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Product product2 annotation(Placement(visible = true, transformation(origin = {33.7984,-3.72093}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Math.Add add1 annotation(Placement(visible = true, transformation(origin = {71.3178,-44.9612}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const(k = mass * fr * 9.81) annotation(Placement(visible = true, transformation(origin = {-28.5271,2.7907}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = -1) annotation(Placement(visible = true, transformation(origin = {41.815,37.8749}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  Modelica.Mechanics.Translational.Sensors.SpeedSensor speedsensor1 annotation(Placement(visible = true, transformation(origin = {-41.9286,-65.552}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real cwA(start = 0.677, unit = "1") "Area * air drag coefficient" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real mass(start = 150, unit = "1") "Vehicle Mass" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real fr(start = 0.014, unit = "1") "Rolling Resistance Coefficient" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(product1.u1,speedsensor1.v) annotation(Line(points = {{-12.6794,-65.5983},{-22.3256,-65.5983},{-22.3256,-65.552},{-28.7286,-65.552}}));
  connect(product1.u2,speedsensor1.v) annotation(Line(points = {{-12.6794,-76.4173},{-22.6357,-76.4173},{-22.6357,-65.552},{-28.7286,-65.552}}));
  connect(limiter1.u,speedsensor1.v) annotation(Line(points = {{-8.8186,-31.6279},{-22.6357,-31.6279},{-22.6357,-65.552},{-28.7286,-65.552}}));
  connect(speedsensor1.flange,flange_a) annotation(Line(points = {{-53.9286,-65.552},{-56.7442,-65.552},{-56.7442,36.5891},{-65.7364,36.5891}}));
  connect(gain1.y,force1.f) annotation(Line(points = {{28.615,37.8749},{-7.13178,37.8749},{-7.13178,37.5194},{-8.8558,37.5194}}));
  connect(add1.y,gain1.u) annotation(Line(points = {{84.5178,-44.9612},{89.3023,-44.9612},{89.3023,37.8749},{56.215,37.8749}}));
  connect(const.y,product2.u1) annotation(Line(points = {{-15.3271,2.7907},{17.6744,2.7907},{17.6744,0.343282},{25.67,0.343282}}));
  connect(product2.y,add1.u1) annotation(Line(points = {{41.2495,-3.72093},{55.5039,-3.72093},{55.5039,-37.7612},{56.9178,-37.7612}}));
  connect(gain2.y,add1.u2) annotation(Line(points = {{40.305,-71.0078},{40.9302,-71.0078},{40.9302,-52.1612},{56.9178,-52.1612}}));
  connect(limiter1.y,product2.u2) annotation(Line(points = {{18.7814,-31.6279},{25.7364,-31.6279},{25.7364,-7.78514},{25.67,-7.78514}}));
  connect(product1.y,gain2.u) annotation(Line(points = {{8.05689,-71.0078},{17.6744,-71.0078},{17.6744,-71.0078},{19.5687,-71.0078}}));
  connect(force1.flange,flange_a) annotation(Line(points = {{-35.2558,37.5194},{-62.6357,37.5194},{-62.6357,36.5891},{-65.7364,36.5891}}));
end DrivingResistance;

