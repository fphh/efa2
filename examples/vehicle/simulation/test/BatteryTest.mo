model BatteryTest
  Battery battery1(resistance = 1, voltage = 200) annotation(Placement(visible = true, transformation(origin = {-14.1667,-19.1667}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Sources.SignalCurrent signalcurrent1 annotation(Placement(visible = true, transformation(origin = {-12.5,30}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(experiment(StartTime = 0.0, StopTime = 50.0, Tolerance = 0.000001));
  Modelica.Electrical.Analog.Basic.Ground ground1 annotation(Placement(visible = true, transformation(origin = {34.1667,11.3889}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Basic.Ground ground2 annotation(Placement(visible = true, transformation(origin = {43.0556,-28.6111}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Clock clock1(offset = -10, startTime = 0) annotation(Placement(visible = true, transformation(origin = {-80.2778,80.2778}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Electrical.Analog.Sensors.PotentialSensor potentialsensor1 annotation(Placement(visible = true, transformation(origin = {-53.6111,43.8889}, extent = {{12,-12},{-12,12}}, rotation = -270)));
  Modelica.Blocks.Math.Division division1 annotation(Placement(visible = true, transformation(origin = {-25.2778,73.8889}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = 1000) annotation(Placement(visible = true, transformation(origin = {-52.5,79.7222}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
equation
  connect(potentialsensor1.phi,division1.u2) annotation(Line(points = {{-53.6111,57.0889},{-53.6111,57.0889},{-53.6111,69.4183},{-34.219,69.4183}}));
  connect(clock1.y,gain1.u) annotation(Line(points = {{-71.262,80.2778},{-60.8333,80.2778},{-60.8333,79.7222},{-60.6284,79.7222}}));
  connect(gain1.y,division1.u1) annotation(Line(points = {{-45.0489,79.7222},{-35,79.7222},{-35,78.3595},{-34.219,78.3595}}));
  connect(division1.y,signalcurrent1.i) annotation(Line(points = {{-17.0816,73.8889},{-11.6667,73.8889},{-11.6667,38.4},{-12.5,38.4}}));
  connect(potentialsensor1.p,signalcurrent1.p) annotation(Line(points = {{-53.6111,31.8889},{-24.7222,31.8889},{-24.7222,30},{-24.5,30}}));
  connect(ground2.p,battery1.pin_n) annotation(Line(points = {{43.0556,-16.6111},{43.0556,-8.61111},{-10.5447,-8.61111},{-10.5447,-9.72765}}));
  connect(signalcurrent1.n,ground1.p) annotation(Line(points = {{-0.5,30},{33.8889,30},{33.8889,23.3889},{34.1667,23.3889}}));
  connect(signalcurrent1.p,battery1.pin_p) annotation(Line(points = {{-24.5,30},{-32.2222,30},{-32.2222,-9.72765},{-21.2276,-9.72765}}));
end BatteryTest;

