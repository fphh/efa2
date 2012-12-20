model vehicle
  annotation(experiment(StartTime = 0.0, StopTime = 50.0, Tolerance = 0.000001));
  Modelica.Mechanics.Rotational.Sources.Torque torque1 annotation(Placement(visible = true, transformation(origin = {-75.0387,33.4883}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Translational.Components.IdealRollingWheel idealrollingwheel1 annotation(Placement(visible = true, transformation(origin = {21.8846,28.8105}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  gearbox gearbox1 annotation(Placement(visible = true, transformation(origin = {-30.3876,28.8372}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  driver driver1 annotation(Placement(visible = true, transformation(origin = {-34.1085,78.7597}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Translational.Sensors.SpeedSensor speedsensor1 annotation(Placement(visible = true, transformation(origin = {-11.1629,58.2945}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  Modelica.Blocks.Sources.TimeTable timetable1(table = [0,0;10,0;20,20;30,20;40,0;50,0]) annotation(Placement(visible = true, transformation(origin = {-86.8215,81.5504}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  vehicle_body vehicle_body1 annotation(Placement(visible = true, transformation(origin = {82.1705,22.0155}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  driving_resistance driving_resistance1 annotation(Placement(visible = true, transformation(origin = {115.039,18.6047}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(driver1.y,torque1.tau) annotation(Line(points = {{-24.5085,81.7364},{20.155,81.7364},{20.155,48.3721},{-95.814,48.3721},{-95.814,32.8682},{-89.6124,32.8682},{-89.6124,33.4883},{-89.4387,33.4883}}));
  connect(vehicle_body1.flange_a,driving_resistance1.flange_a) annotation(Line(points = {{90.9892,23.0202},{107.907,23.0202},{107.907,22.9953},{107.15,22.9953}}));
  connect(torque1.flange,gearbox1.flange_a) annotation(Line(points = {{-63.0387,33.4883},{-40.3101,33.4883},{-40.3101,29.7674},{-41.0294,29.7674}}));
  connect(gearbox1.flange_b,idealrollingwheel1.flangeR) annotation(Line(points = {{-19.2248,29.7674},{9.92248,29.7674},{9.92248,28.8105},{9.8846,28.8105}}));
  connect(speedsensor1.flange,vehicle_body1.flange_a1) annotation(Line(points = {{0.837146,58.2945},{62.3256,58.2945},{62.3256,28.8248},{73.0915,28.8248}}));
  connect(idealrollingwheel1.flangeT,vehicle_body1.flange_a1) annotation(Line(points = {{33.8846,28.8105},{62.3256,28.8105},{62.3256,28.8248},{73.0915,28.8248}}));
  connect(timetable1.y,driver1.u) annotation(Line(points = {{-73.6215,81.5504},{-44.031,81.5504},{-44.031,82.2574},{-43.262,82.2574}}));
  connect(speedsensor1.v,driver1.realinput1) annotation(Line(points = {{-24.3629,58.2945},{-42.7907,58.2945},{-42.7907,75.6341},{-42.6295,75.6341}}));
end vehicle;

