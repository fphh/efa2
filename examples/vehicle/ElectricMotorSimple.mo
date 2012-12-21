model ElectricMotorSchematic
  annotation(Diagram(), Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-67.5969,48.3721},{67.5969,-45.2713}}, textString = "E-Motor"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-92.093,59.2248},{72.6001,-51.2131}}),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{68.9461,73.2286},{105.845,51.2131}}),Line(points = {{-49.6124,91.1628},{-40,72.5581},{-22.0155,77.8295},{-12.7132,61.7054},{-13.3333,73.4884},{-24.186,66.9767},{-13.3333,63.2558}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}), experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  Modelica.Mechanics.Rotational.Components.Inertia inertia1 annotation(Placement(visible = true, transformation(origin = {66.6667,10.8528}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Sources.Torque torque1 annotation(Placement(visible = true, transformation(origin = {35.0388,10.5426}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.VariableLimiter TorqueLimit annotation(Placement(visible = true, transformation(origin = {-5.27132,10.2326}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant P_max(k = 60000) annotation(Placement(visible = true, transformation(origin = {44.6281,-80.6612}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  Modelica.Blocks.Sources.Constant T_max(k = 100) annotation(Placement(visible = true, transformation(origin = {-79.6694,-64.1322}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-89.9174,11.9008}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-89.9174,11.9008}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = -1) annotation(Placement(visible = true, transformation(origin = {-34.3802,0}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Math.Min maxTorque annotation(Placement(visible = true, transformation(origin = {-57.1901,-25.124}, extent = {{12,-12},{-12,12}}, rotation = -270)));
  Modelica.Mechanics.Rotational.Interfaces.Flange_a flange_a annotation(Placement(visible = true, transformation(origin = {91.1712,11.5315}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {91.1712,11.5315}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Sensors.SpeedSensor speedsensor1 annotation(Placement(visible = true, transformation(origin = {60.4826,-45.3137}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  Modelica.Blocks.Math.Division division1 annotation(Placement(visible = true, transformation(origin = {-26.7779,-66.4463}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  Modelica.Blocks.Nonlinear.Limiter AvoidZero(uMin = 1) annotation(Placement(visible = true, transformation(origin = {26.7913,-34.5794}, extent = {{12,12},{-12,-12}}, rotation = -180)));
equation
  connect(AvoidZero.u,speedsensor1.w) annotation(Line(points = {{41.1913,-34.5794},{47.9751,-34.5794},{47.9751,-45.3137},{47.2826,-45.3137}}));
  connect(division1.u2,AvoidZero.y) annotation(Line(points = {{-12.3779,-59.2463},{11.215,-59.2463},{11.215,-34.5794},{13.5913,-34.5794}}));
  connect(P_max.y,division1.u1) annotation(Line(points = {{31.4281,-80.6612},{6.61157,-80.6612},{6.61157,-74.0496},{-12.3779,-74.0496},{-12.3779,-73.6463}}));
  connect(division1.y,maxTorque.u2) annotation(Line(points = {{-39.9779,-66.4463},{-50,-66.4463},{-50,-39.524},{-49.9901,-39.524}}));
  connect(speedsensor1.flange,inertia1.flange_b) annotation(Line(points = {{72.4826,-45.3137},{78.7597,-45.3137},{78.7597,10.8528},{78.6667,10.8528}}));
  connect(inertia1.flange_b,flange_a) annotation(Line(points = {{78.6667,10.8528},{98.018,10.8528},{98.018,11.5315},{91.1712,11.5315}}));
  connect(gain1.y,TorqueLimit.limit2) annotation(Line(points = {{-26.9291,0},{-19.8347,0},{-19.8347,0.6326},{-19.6713,0.6326}}));
  connect(gain1.u,maxTorque.y) annotation(Line(points = {{-42.5086,0},{-57.1901,0},{-57.1901,-11.924},{-57.1901,-11.924}}));
  connect(maxTorque.y,TorqueLimit.limit1) annotation(Line(points = {{-57.1901,-11.924},{-57.5207,-11.924},{-57.5207,19.8347},{-19.6713,19.8347},{-19.6713,19.8326}}));
  connect(T_max.y,maxTorque.u1) annotation(Line(points = {{-66.4694,-64.1322},{-63.8017,-64.1322},{-63.8017,-39.524},{-64.3901,-39.524}}));
  connect(u,TorqueLimit.u) annotation(Line(points = {{-89.9174,11.9008},{-20.8264,11.9008},{-20.8264,10.2326},{-19.6713,10.2326}}));
  connect(TorqueLimit.y,torque1.tau) annotation(Line(points = {{7.92868,10.2326},{20.1653,10.2326},{20.1653,10.5426},{20.6388,10.5426}}));
  connect(torque1.flange,inertia1.flange_a) annotation(Line(points = {{47.0388,10.5426},{54.8837,10.5426},{54.8837,10.8528},{54.6667,10.8528}}));
end ElectricMotorSchematic;

