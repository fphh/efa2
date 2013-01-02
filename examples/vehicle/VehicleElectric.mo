model VehicleElectric
  annotation(Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-74.1085,33.7984},{74.1085,-43.4109}}, textString = "Simple EV"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-98.9147,74.1085},{99.2248,-72.2481}})}), experiment(StartTime = 0.0, StopTime = 50.0, Tolerance = 0.000001));
  Modelica.Mechanics.Translational.Components.IdealRollingWheel idealrollingwheel1 annotation(Placement(visible = true, transformation(origin = {35.1593,-7.10821}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Chassis chassis1 annotation(Placement(visible = true, transformation(origin = {71.9385,-11.7829}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Gearbox gearbox1 annotation(Placement(visible = true, transformation(origin = {4.03104,-7.75194}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  ElectricMotor electricmotor1 annotation(Placement(visible = true, transformation(origin = {-29.595,-7.78816}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.TimeTable timetable1(table = [0,0;10,0;20,120;30,120;40,0;50,0]) annotation(Placement(visible = true, transformation(origin = {-93.9026,-5.36837}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Driver driver1 annotation(Placement(visible = true, transformation(origin = {-56.4022,-8.99228}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  DrivingResistance drivingresistance1 annotation(Placement(visible = true, transformation(origin = {97.647,-15.5198}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Translational.Sensors.SpeedSensor speedsensor1 annotation(Placement(visible = true, transformation(origin = {34.1129,-39.9494}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  Battery battery1 annotation(Placement(visible = true, transformation(origin = {-29.2683,57.622}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Basic.Ground ground1 annotation(Placement(visible = true, transformation(origin = {-4.96124,55.1938}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Basic.Ground ground2 annotation(Placement(visible = true, transformation(origin = {-10.5426,11.4729}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(electricmotor1.pin_n,ground2.p) annotation(Line(points = {{-26.6555,2.74207},{-26.9767,2.74207},{-26.9767,31.6279},{-10.5426,31.6279},{-10.5426,23.4729},{-10.5426,23.4729}}));
  connect(battery1.pin_p,electricmotor1.pin_p) annotation(Line(points = {{-36.3293,67.061},{-48.062,67.061},{-48.062,2.17054},{-35.6909,2.17054},{-35.6909,2.374}}));
  connect(battery1.pin_n,ground1.p) annotation(Line(points = {{-25.6464,67.061},{-4.34109,67.061},{-4.34109,67.1938},{-4.96124,67.1938}}));
  connect(speedsensor1.flange,chassis1.flange_a1) annotation(Line(points = {{46.1129,-39.9494},{62.0155,-39.9494},{62.0155,-4.9736},{62.8594,-4.9736}}));
  connect(speedsensor1.v,driver1.realinput1) annotation(Line(points = {{20.9129,-39.9494},{-71.6279,-39.9494},{-71.6279,-12.1179},{-64.9232,-12.1179}}));
  connect(chassis1.flange_a,drivingresistance1.flange_a) annotation(Line(points = {{80.7571,-10.7782},{94.8837,-10.7782},{94.8837,-11.1291},{89.7586,-11.1291}}));
  connect(driver1.y,electricmotor1.u) annotation(Line(points = {{-46.8022,-6.01554},{-40.4984,-6.01554},{-40.4984,-6.36007},{-40.3851,-6.36007}}));
  connect(timetable1.y,driver1.u) annotation(Line(points = {{-80.7026,-5.36837},{-73.1783,-5.36837},{-73.1783,-5.4946},{-65.5557,-5.4946}}));
  connect(electricmotor1.flange_a,gearbox1.flange_a) annotation(Line(points = {{-18.6545,-6.40438},{-6.23053,-6.40438},{-6.23053,-6.82171},{-6.61081,-6.82171}}));
  connect(gearbox1.flange_b,idealrollingwheel1.flangeR) annotation(Line(points = {{15.1938,-6.82171},{24.186,-6.82171},{24.186,-7.10821},{23.1593,-7.10821}}));
  connect(idealrollingwheel1.flangeT,chassis1.flange_a1) annotation(Line(points = {{47.1593,-7.10821},{63.2558,-7.10821},{63.2558,-4.97364},{62.8594,-4.97364}}));
end VehicleElectric;

