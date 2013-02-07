model ResistanceTest
  DrivingResistance drivingresistance1 annotation(Placement(visible = true, transformation(origin = {62.069,6.46552}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Translational.Sources.Speed speed1 annotation(Placement(visible = true, transformation(origin = {24.1379,12.069}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = 1) annotation(Placement(visible = true, transformation(origin = {-17.6724,11.6379}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Clock clock1 annotation(Placement(visible = true, transformation(origin = {-77.5862,12.069}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(experiment(StartTime = 0.0, StopTime = 50.0, Tolerance = 0.000001));
equation
  connect(clock1.y,gain1.u) annotation(Line(points = {{-64.3862,12.069},{-33.1897,12.069},{-33.1897,11.6379},{-32.0724,11.6379}}));
  connect(gain1.y,speed1.v_ref) annotation(Line(points = {{-4.47241,11.6379},{9.05172,11.6379},{9.05172,12.069},{9.73793,12.069}}));
  connect(speed1.flange,drivingresistance1.flange_a) annotation(Line(points = {{36.1379,12.069},{53.8793,12.069},{53.8793,10.8562},{54.1806,10.8562}}));
end ResistanceTest;

