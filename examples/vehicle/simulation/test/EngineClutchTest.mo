model EngineClutchTest
  Modelica.Mechanics.Rotational.Components.Inertia inertia1(J = 25, w(start = 150)) annotation(Placement(visible = true, transformation(origin = {46.8217,15.1938}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.Clutch clutch1 annotation(Placement(visible = true, transformation(origin = {15.1938,14.8837}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Engine engine1 annotation(Placement(visible = true, transformation(origin = {-27.907,14.5736}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Sensors.SpeedSensor speedsensor1 annotation(Placement(visible = true, transformation(origin = {-22.3256,-21.7054}, extent = {{12,12},{-12,-12}}, rotation = -180)));
  ElectricMotor electricmotor1 annotation(Placement(visible = true, transformation(origin = {-27.5969,-60.4651}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
equation
  connect(electricmotor1.flange_a,clutch1.flange_a) annotation(Line(points = {{-20.8037,-59.6059},{4.03101,-59.6059},{4.03101,14.8837},{3.1938,14.8837}}));
  connect(speedsensor1.w,engine1.Speed) annotation(Line(points = {{-35.5256,-21.7054},{-55.5039,-21.7054},{-55.5039,9.6124},{-36.5677,9.6124},{-36.5677,9.96323}}));
  connect(speedsensor1.flange,clutch1.flange_a) annotation(Line(points = {{-10.3256,-21.7054},{3.72093,-21.7054},{3.72093,14.8837},{3.1938,14.8837}}));
  connect(engine1.flange_b,clutch1.flange_a) annotation(Line(points = {{-17.652,14.7029},{3.41085,14.7029},{3.41085,14.8837},{3.1938,14.8837}}));
  connect(clutch1.flange_b,inertia1.flange_a) annotation(Line(points = {{27.1938,14.8837},{35.6589,14.8837},{35.6589,15.1938},{34.8217,15.1938}}));
end EngineClutchTest;

