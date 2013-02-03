model EngineMaxTorque
  Modelica.Blocks.Math.Gain gain1 annotation(Placement(visible = true, transformation(origin = {-28.6111,51.3889}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain2 annotation(Placement(visible = true, transformation(origin = {-10.8333,-0.555556}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.DeadZone deadzone1 annotation(Placement(visible = true, transformation(origin = {-62.1667,9.72222}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Math.Add add1(k2 = -1) annotation(Placement(visible = true, transformation(origin = {58.8889,3.88889}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Log log1 annotation(Placement(visible = true, transformation(origin = {-1.94444,51.3889}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain3 annotation(Placement(visible = true, transformation(origin = {28.3333,50.8333}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput speed annotation(Placement(visible = true, transformation(origin = {-93.0556,9.44444}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-93.0556,9.44444}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon());
  Modelica.Blocks.Interfaces.RealOutput maxTorque annotation(Placement(visible = true, transformation(origin = {94.1667,4.72222}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {94.1667,4.72222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(add1.y,maxTorque) annotation(Line(points = {{67.0851,3.88889},{85.8333,3.88889},{85.8333,4.72222},{94.1667,4.72222}}));
  connect(speed,deadzone1.u) annotation(Line(points = {{-93.0556,9.44444},{-70,9.44444},{-70,9.72222},{-70.2951,9.72222}}));
  connect(gain3.y,add1.u1) annotation(Line(points = {{37.3491,50.8333},{43.6111,50.8333},{43.6111,8.33333},{49.9476,8.33333},{49.9476,8.35952}}));
  connect(log1.y,gain3.u) annotation(Line(points = {{7.97291,51.3889},{18.6111,51.3889},{18.6111,50.8333},{18.4979,50.8333}}));
  connect(log1.u,gain1.y) annotation(Line(points = {{-12.7634,51.3889},{-19.4444,51.3889},{-19.4444,51.3889},{-19.5953,51.3889}}));
  connect(gain2.y,add1.u2) annotation(Line(points = {{-2.63717,-0.555556},{21.3889,-0.555556},{21.3889,-0.581745},{49.9476,-0.581745}}));
  connect(gain2.u,deadzone1.y) annotation(Line(points = {{-19.7746,-0.555556},{-50.2778,-0.555556},{-50.2778,9.72222},{-54.7156,9.72222},{-54.7156,9.72222}}));
  connect(deadzone1.y,gain1.u) annotation(Line(points = {{-54.7156,9.72222},{-50.2778,9.72222},{-50.2778,51.3889},{-38.4465,51.3889},{-38.4465,51.3889}}));
end EngineMaxTorque;

