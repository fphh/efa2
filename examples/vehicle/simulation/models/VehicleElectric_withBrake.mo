model VehicleElectric
  annotation(Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-74.1085,33.7984},{74.1085,-43.4109}}, textString = "Simple EV"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-98.9147,74.1085},{99.2248,-72.2481}})}), experiment(StartTime = 0.0, StopTime = 50.0, Tolerance = 0.000001));
  Modelica.Blocks.Sources.TimeTable timetable1(table = [0,0;10,0;20,120;39,120;40,0;50,0]) annotation(Placement(visible = true, transformation(origin = {-150.224,-5.94308}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Driver driver1 annotation(Placement(visible = true, transformation(origin = {-97.7815,-9.56699}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Gearbox gearbox1 annotation(Placement(visible = true, transformation(origin = {18.9736,-8.32666}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  ElectricMotor electricmotor1 annotation(Placement(visible = true, transformation(origin = {-14.6525,-8.36288}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Battery battery1 annotation(Placement(visible = true, transformation(origin = {-14.3258,57.0473}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Basic.Ground ground1 annotation(Placement(visible = true, transformation(origin = {9.98128,54.6191}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Basic.Ground ground2 annotation(Placement(visible = true, transformation(origin = {4.39988,10.8982}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Translational.Components.IdealRollingWheel idealrollingwheel1 annotation(Placement(visible = true, transformation(origin = {146.079,-6.5335}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Chassis chassis1 annotation(Placement(visible = true, transformation(origin = {182.858,-11.2082}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  DrivingResistance drivingresistance1 annotation(Placement(visible = true, transformation(origin = {208.566,-14.9451}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.Brake brake1 annotation(Placement(visible = true, transformation(origin = {87.3563,-8.04598}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GasPedal gaspedal1 annotation(Placement(visible = true, transformation(origin = {-56.8966,-14.3678}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Translational.Sensors.SpeedSensor speedsensor1 annotation(Placement(visible = true, transformation(origin = {142.733,-85.9264}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  Modelica.Mechanics.Rotational.Components.IdealRollingWheel idealrollingwheel2 annotation(Placement(visible = true, transformation(origin = {144.252,-50.5747}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.Brake brake2 annotation(Placement(visible = true, transformation(origin = {87.9311,-51.1495}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  BrakeSystem brakesystem1 annotation(Placement(visible = true, transformation(origin = {27.0115,-55.1724}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(brakesystem1.PedalPosition,driver1.BrakePedalPosition) annotation(Line(points = {{18.2529,-54.1379},{-74.1379,-54.1379},{-74.1379,-1.84285},{-73.8505,-1.84285}}));
  connect(brakesystem1.BrakeTorqueRear,brake2.f_normalized) annotation(Line(points = {{35.908,-60},{58.046,-60},{58.046,-33.3333},{87.3563,-33.3333},{87.3563,-37.9495},{87.9311,-37.9495}}));
  connect(brakesystem1.BrakeTorqueFront,brake1.f_normalized) annotation(Line(points = {{35.8391,-49.0345},{50,-49.0345},{50,21.8391},{87.3563,21.8391},{87.3563,5.15402},{87.3563,5.15402}}));
  connect(brake2.flange_b,idealrollingwheel2.flangeR) annotation(Line(points = {{99.9311,-51.1495},{136.207,-51.1495},{136.207,-50},{132.252,-50},{132.252,-50.5747}}));
  connect(idealrollingwheel2.flangeT,chassis1.flange_a1) annotation(Line(points = {{156.252,-50.5747},{174.138,-50.5747},{174.138,-4.3989},{173.779,-4.3989}}));
  connect(speedsensor1.v,driver1.Speed) annotation(Line(points = {{129.533,-85.9264},{-106.322,-85.9264},{-106.322,-12.6926},{-106.302,-12.6926}}));
  connect(speedsensor1.flange,chassis1.flange_a1) annotation(Line(points = {{154.733,-85.9264},{172.935,-85.9264},{172.935,-4.3989},{173.779,-4.3989}}));
  connect(gaspedal1.y,electricmotor1.u) annotation(Line(points = {{-47.2414,-13.6782},{-26.4368,-13.6782},{-26.4368,-6.93478},{-25.4426,-6.93478}}));
  connect(driver1.GasPedalPosition,gaspedal1.u) annotation(Line(points = {{-79.5056,-6.5325},{-66.6667,-6.5325},{-66.6667,-13.2644},{-66.3448,-13.2644}}));
  connect(timetable1.y,driver1.SpeedDemand) annotation(Line(points = {{-137.024,-5.94308},{-107.471,-5.94308},{-107.471,-6.06931},{-106.711,-6.06931}}));
  connect(gearbox1.flange_b,brake1.flange_a) annotation(Line(points = {{30.1364,-7.39643},{75.8621,-7.39643},{75.8621,-8.04598},{75.3563,-8.04598}}));
  connect(brake1.flange_b,idealrollingwheel1.flangeR) annotation(Line(points = {{99.3563,-8.04598},{134.483,-8.04598},{134.483,-6.5335},{134.079,-6.5335}}));
  connect(chassis1.flange_a,drivingresistance1.flange_a) annotation(Line(points = {{191.677,-10.2035},{205.803,-10.2035},{205.803,-10.5544},{200.678,-10.5544}}));
  connect(idealrollingwheel1.flangeT,chassis1.flange_a1) annotation(Line(points = {{158.079,-6.5335},{174.175,-6.5335},{174.175,-4.39886},{173.779,-4.39886}}));
  connect(electricmotor1.pin_n,ground2.p) annotation(Line(points = {{-11.7129,2.16735},{-12.0342,2.16735},{-12.0342,31.0532},{4.39988,31.0532},{4.39988,22.8982},{4.39988,22.8982}}));
  connect(battery1.pin_n,ground1.p) annotation(Line(points = {{-10.7038,66.4863},{10.6015,66.4863},{10.6015,66.6191},{9.98128,66.6191}}));
  connect(battery1.pin_p,electricmotor1.pin_p) annotation(Line(points = {{-21.3868,66.4863},{-33.1195,66.4863},{-33.1195,1.59582},{-21.685,1.59582},{-21.685,2.01851}}));
  connect(electricmotor1.flange_a,gearbox1.flange_a) annotation(Line(points = {{-3.71193,-6.9791},{8.71198,-6.9791},{8.71198,-7.39642},{8.33173,-7.39642}}));
end VehicleElectric;

