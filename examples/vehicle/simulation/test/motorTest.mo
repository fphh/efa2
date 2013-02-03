model motorTest
  ElectricMotor electricmotor2 annotation(Placement(visible = true, transformation(origin = {7.77778,-4.72222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant constant1(k = -10000) annotation(Placement(visible = true, transformation(origin = {-34.7222,-29.1667}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.Inertia inertia2(J = 5) annotation(Placement(visible = true, transformation(origin = {50,35}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const(k = 10000) annotation(Placement(visible = true, transformation(origin = {-44.7222,41.6667}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  annotation(experiment(StartTime = 0.0, StopTime = 50.0, Tolerance = 0.000001));
  Modelica.Electrical.Analog.Sources.ConstantVoltage constantvoltage1(V = 200) annotation(Placement(visible = true, transformation(origin = {20,77.5}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.Inertia inertia1(J = 5) annotation(Placement(visible = true, transformation(origin = {48.8889,-4.16667}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  ElectricMotor electricmotor1 annotation(Placement(visible = true, transformation(origin = {9.44444,33.6111}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(constantvoltage1.n,electricmotor1.pin_n) annotation(Line(points = {{32,77.5},{38.8889,77.5},{38.8889,49.1667},{11.6667,49.1667},{11.6667,44.1413},{12.384,44.1413}}));
  connect(constantvoltage1.p,electricmotor1.pin_p) annotation(Line(points = {{8,77.5},{2.77778,77.5},{2.77778,43.9925},{2.41188,43.9925}}));
  connect(const.y,electricmotor1.u) annotation(Line(points = {{-37.2712,41.6667},{-2.77778,41.6667},{-2.77778,35.0392},{-1.34564,35.0392}}));
  connect(electricmotor1.flange_a,inertia2.flange_a) annotation(Line(points = {{20.385,34.9949},{39.7222,34.9949},{39.7222,34.4444},{38.0556,34.4444},{38.0556,35},{38,35}}));
  connect(electricmotor2.flange_a,inertia1.flange_a) annotation(Line(points = {{18.7183,-3.33844},{37.7778,-3.33844},{37.7778,-4.16667},{36.8889,-4.16667}}));
  connect(constantvoltage1.n,electricmotor2.pin_n) annotation(Line(points = {{32,77.5},{31.1111,77.5},{31.1111,6.94444},{10.7173,6.94444},{10.7173,5.80801}}));
  connect(constantvoltage1.p,electricmotor2.pin_p) annotation(Line(points = {{8,77.5},{0.555556,77.5},{0.555556,5.65917},{0.745214,5.65917}}));
  connect(constant1.y,electricmotor2.u) annotation(Line(points = {{-25.7064,-29.1667},{-14.1667,-29.1667},{-14.1667,-2.77778},{-3.01231,-2.77778},{-3.01231,-3.29413}}));
end motorTest;

