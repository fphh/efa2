model DrivingResistance
  annotation(Diagram(), Icon(graphics = {Line(points = {{-37.8295,15.1938},{9.30233,19.2248},{18.2946,23.876},{33.7984,31.3178},{48.3721,40},{58.6047,50.8527},{66.9767,60.7752}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-37.5194,14.5736},{68.5271,14.8837},{68.5271,15.1938}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-36.8992,14.5736},{-37.2093,79.0698}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-11.7829,92.7132},{76.8992,79.3798}}, textString = "Driving Resistance"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-48.6822,3.10078},{95.814,98.6047}})}));
  Modelica.Mechanics.Translational.Sources.Force force1 annotation(Placement(visible = true, transformation(origin = {-23.2558,37.5194}, extent = {{12,12},{-12,-12}}, rotation = 180)));
  Modelica.Mechanics.Translational.Interfaces.Flange_a flange_a annotation(Placement(visible = true, transformation(origin = {-65.7364,36.5891}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-65.7364,36.5891}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Product product1 annotation(Placement(visible = true, transformation(origin = {-1.86047,-71.0078}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain2(k = 0.5 * 0.677 * 1.119) annotation(Placement(visible = true, transformation(origin = {30.3876,-71.0078}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.Limiter limiter1(uMax = 1) annotation(Placement(visible = true, transformation(origin = {5.5814,-31.6279}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Product product2 annotation(Placement(visible = true, transformation(origin = {33.7984,-3.72093}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Math.Add add1 annotation(Placement(visible = true, transformation(origin = {71.3178,-44.9612}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Translational.Sensors.SpeedSensor speedsensor1 annotation(Placement(visible = true, transformation(origin = {-42.7907,-39.6899}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const annotation(Placement(visible = true, transformation(origin = {-28.5271,2.7907}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = -1) annotation(Placement(visible = true, transformation(origin = {41.2403,38.4496}, extent = {{12,12},{-12,-12}}, rotation = -180)));
equation
  connect(add1.y,gain1.u) annotation(Line(points = {{84.5178,-44.9612},{89.3023,-44.9612},{89.3023,38.4496},{55.6403,38.4496}}));
  connect(gain1.y,force1.f) annotation(Line(points = {{28.0403,38.4496},{-7.13178,38.4496},{-7.13178,37.5194},{-8.85581,37.5194}}));
  connect(const.y,product2.u1) annotation(Line(points = {{-15.3271,2.7907},{17.6744,2.7907},{17.6744,0.343282},{25.67,0.343282}}));
  connect(speedsensor1.flange,flange_a) annotation(Line(points = {{-54.7907,-39.6899},{-56.7442,-39.6899},{-56.7442,36.5891},{-65.7364,36.5891}}));
  connect(limiter1.u,speedsensor1.v) annotation(Line(points = {{-8.8186,-31.6279},{-22.6357,-31.6279},{-22.6357,-39.6899},{-29.5907,-39.6899}}));
  connect(product1.u2,speedsensor1.v) annotation(Line(points = {{-12.6794,-76.4172},{-22.6357,-76.4172},{-22.6357,-39.6899},{-29.5907,-39.6899}}));
  connect(product1.u1,speedsensor1.v) annotation(Line(points = {{-12.6794,-65.5983},{-22.3256,-65.5983},{-22.3256,-39.6899},{-29.5907,-39.6899}}));
  connect(product2.y,add1.u1) annotation(Line(points = {{41.2495,-3.72093},{55.5039,-3.72093},{55.5039,-37.7612},{56.9178,-37.7612}}));
  connect(gain2.y,add1.u2) annotation(Line(points = {{40.305,-71.0078},{40.9302,-71.0078},{40.9302,-52.1612},{56.9178,-52.1612}}));
  connect(limiter1.y,product2.u2) annotation(Line(points = {{18.7814,-31.6279},{25.7364,-31.6279},{25.7364,-7.78514},{25.67,-7.78514}}));
  connect(product1.y,gain2.u) annotation(Line(points = {{8.05689,-71.0078},{17.6744,-71.0078},{17.6744,-71.0078},{19.5687,-71.0078}}));
  connect(force1.flange,flange_a) annotation(Line(points = {{-35.2558,37.5194},{-62.6357,37.5194},{-62.6357,36.5891},{-65.7364,36.5891}}));
end DrivingResistance;

