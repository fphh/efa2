model EngineTest
  Engine engine1 annotation(Placement(visible = true, transformation(origin = {17.9845,10.8527}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Clock clock1 annotation(Placement(visible = true, transformation(origin = {-64.4961,-25.4264}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant ToqueDemand(k = 200) annotation(Placement(visible = true, transformation(origin = {-68.8372,10.5426}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain SpeedSlope(k = 10) annotation(Placement(visible = true, transformation(origin = {-27.5969,-25.1163}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.Fixed fixed1 annotation(Placement(visible = true, transformation(origin = {53.3333,7.13178}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(experiment(StartTime = 0.0, StopTime = 30.0, Tolerance = 0.000001));
equation
  connect(engine1.flange_b,fixed1.flange) annotation(Line(points = {{28.2394,10.982},{52.7132,10.982},{52.7132,7.13178},{53.3333,7.13178}}));
  connect(SpeedSlope.y,engine1.Speed) annotation(Line(points = {{-14.3969,-25.1163},{8.06202,-25.1163},{8.06202,6.2423},{9.32381,6.2423}}));
  connect(clock1.y,SpeedSlope.u) annotation(Line(points = {{-51.2961,-25.4264},{-40.3101,-25.4264},{-40.3101,-25.1163},{-41.9969,-25.1163}}));
  connect(ToqueDemand.y,engine1.TorqueDemand) annotation(Line(points = {{-55.6372,10.5426},{8.68217,10.5426},{8.68217,10.982},{8.33279,10.982}}));
end EngineTest;

