model VehicleElectricSingleSpeed
  annotation(Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-74.1085,33.7984},{74.1085,-43.4109}}, textString = "Simple EV"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-98.9147,74.1085},{99.2248,-72.2481}})}), experiment(StartTime = 0.0, StopTime = 50.0, Tolerance = 0.000001));
  Modelica.Mechanics.Translational.Components.IdealRollingWheel idealrollingwheel1 annotation(Placement(visible = true, transformation(origin = {35.1593,-7.10821}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Chassis chassis1 annotation(Placement(visible = true, transformation(origin = {71.9385,-11.7829}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  DrivingResistance drivingresistance1 annotation(Placement(visible = true, transformation(origin = {103.566,-12.093}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GearboxSingleSpeed gearboxsinglespeed1 annotation(Placement(visible = true, transformation(origin = {4.03104,-7.75194}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  DriverGasOnly drivergasonly1 annotation(Placement(visible = true, transformation(origin = {-63.2558,-8.99228}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Translational.Sensors.SpeedSensor speedsensor1 annotation(Placement(visible = true, transformation(origin = {33.1783,-50.8528}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  Modelica.Blocks.Sources.TimeTable timetable1(table = [0,0;10,0;20,20;30,20;40,0;50,0]) annotation(Placement(visible = true, transformation(origin = {-104.806,15.5039}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Sources.Torque torque1 annotation(Placement(visible = true, transformation(origin = {-30.2181,-7.16511}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
equation
  connect(drivergasonly1.y,torque1.tau) annotation(Line(points = {{-53.6558,-6.01554},{-38.6293,-6.01554},{-38.6293,-7.16511},{-38.3465,-7.16511}}));
  connect(torque1.flange,gearboxsinglespeed1.flange_a) annotation(Line(points = {{-23.4444,-7.16511},{-6.85358,-7.16511},{-6.85358,-6.82171},{-6.61081,-6.82171}}));
  connect(timetable1.y,drivergasonly1.u) annotation(Line(points = {{-91.606,15.5039},{-73.1783,15.5039},{-73.1783,-5.49461},{-72.4093,-5.49461}}));
  connect(speedsensor1.v,drivergasonly1.realinput1) annotation(Line(points = {{19.9783,-50.8528},{-71.6279,-50.8528},{-71.6279,-12.1179},{-71.7768,-12.1179}}));
  connect(speedsensor1.flange,chassis1.flange_a1) annotation(Line(points = {{45.1783,-50.8528},{62.0155,-50.8528},{62.0155,-4.97364},{62.8594,-4.97364}}));
  connect(gearboxsinglespeed1.flange_b,idealrollingwheel1.flangeR) annotation(Line(points = {{15.1938,-6.82171},{24.186,-6.82171},{24.186,-7.10821},{23.1593,-7.10821}}));
  connect(chassis1.flange_a,drivingresistance1.flange_a) annotation(Line(points = {{80.7571,-10.7783},{94.8837,-10.7783},{94.8837,-7.70233},{95.6775,-7.70233}}));
  connect(idealrollingwheel1.flangeT,chassis1.flange_a1) annotation(Line(points = {{47.1593,-7.10821},{63.2558,-7.10821},{63.2558,-4.97364},{62.8594,-4.97364}}));
end VehicleElectricSingleSpeed;

