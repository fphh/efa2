model VehicleElectric
  annotation(Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-74.1085,33.7984},{74.1085,-43.4109}}, textString = "Simple EV"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-98.9147,74.1085},{99.2248,-72.2481}})}), experiment(StartTime = 0.0, StopTime = 50.0, Tolerance = 0.000001));
  Modelica.Blocks.Sources.TimeTable timetable1(table = [0,0;10,0;20,120;30,120;40,0;50,0]) annotation(Placement(visible = true, transformation(origin = {-150.224,-5.94308}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Driver driver1 annotation(Placement(visible = true, transformation(origin = {-97.7815,-9.56699}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Translational.Components.IdealRollingWheel idealrollingwheel1 annotation(Placement(visible = true, transformation(origin = {64.4696,-8.25764}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Chassis chassis1 annotation(Placement(visible = true, transformation(origin = {101.249,-12.9323}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Gearbox gearbox1 annotation(Placement(visible = true, transformation(origin = {33.3414,-8.90137}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  ElectricMotor electricmotor1 annotation(Placement(visible = true, transformation(origin = {-0.284654,-8.93759}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  DrivingResistance drivingresistance1 annotation(Placement(visible = true, transformation(origin = {126.957,-16.6692}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Translational.Sensors.SpeedSensor speedsensor1 annotation(Placement(visible = true, transformation(origin = {63.4232,-41.0988}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  Battery battery1 annotation(Placement(visible = true, transformation(origin = {0.0420457,56.4726}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Basic.Ground ground1 annotation(Placement(visible = true, transformation(origin = {24.3491,54.0444}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Basic.Ground ground2 annotation(Placement(visible = true, transformation(origin = {18.7677,10.3235}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(electricmotor1.pin_n,ground2.p) annotation(Line(points = {{2.65488,1.59264},{2.33364,1.59264},{2.33364,30.4785},{18.7677,30.4785},{18.7677,22.3235},{18.7677,22.3235}}));
  connect(battery1.pin_n,ground1.p) annotation(Line(points = {{3.66399,65.9116},{24.9693,65.9116},{24.9693,66.0444},{24.3491,66.0444}}));
  connect(battery1.pin_p,electricmotor1.pin_p) annotation(Line(points = {{-7.01893,65.9116},{-18.7517,65.9116},{-18.7517,1.02111},{-7.31722,1.02111},{-7.31722,1.44381}}));
  connect(speedsensor1.v,driver1.realinput1) annotation(Line(points = {{50.2232,-41.0988},{-115.881,-41.0988},{-115.881,-12.6926},{-106.302,-12.6926}}));
  connect(speedsensor1.flange,chassis1.flange_a1) annotation(Line(points = {{75.4232,-41.0988},{91.3258,-41.0988},{91.3258,-6.12302},{92.1698,-6.12302}}));
  connect(chassis1.flange_a,drivingresistance1.flange_a) annotation(Line(points = {{110.067,-11.9277},{124.194,-11.9277},{124.194,-12.2785},{119.069,-12.2785}}));
  connect(electricmotor1.flange_a,gearbox1.flange_a) annotation(Line(points = {{10.6559,-7.55381},{23.0798,-7.55381},{23.0798,-7.97113},{22.6995,-7.97113}}));
  connect(gearbox1.flange_b,idealrollingwheel1.flangeR) annotation(Line(points = {{44.5042,-7.97113},{53.4963,-7.97113},{53.4963,-8.25764},{52.4696,-8.25764}}));
  connect(idealrollingwheel1.flangeT,chassis1.flange_a1) annotation(Line(points = {{76.4696,-8.25764},{92.5661,-8.25764},{92.5661,-6.12302},{92.1698,-6.12302}}));
  connect(timetable1.y,driver1.u) annotation(Line(points = {{-137.024,-5.94308},{-118.006,-5.94308},{-118.006,-6.06932},{-106.711,-6.06932}}));
end VehicleElectric;

