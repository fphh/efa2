model EngineTest
  annotation(Diagram(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-76.9444,58.3333},{85,84.7222}}, textString = "Check Max and Min Torque Curve")}), experiment(StartTime = 0.0, StopTime = 50.0, Tolerance = 0.000001));
  Modelica.Mechanics.Rotational.Components.Fixed fixed1 annotation(Placement(visible = true, transformation(origin = {61.1111,34.354}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.BooleanConstant booleanconstant1 annotation(Placement(visible = true, transformation(origin = {-9.44445,52.7778}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Engine engine1(Tmax = 200, ignitionSpeed = 30, dragTorqueMin = -30, dragTorqueMax = -70, etaOpt = 0.35) annotation(Placement(visible = true, transformation(origin = {28.6111,40.5556}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Engine engine2(Tmax = 200, ignitionSpeed = 30, dragTorqueMin = -30, dragTorqueMax = -70, etaOpt = 0.35) annotation(Placement(visible = true, transformation(origin = {30.8333,-15.2778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant ToqueDemand(k = 10000) annotation(Placement(visible = true, transformation(origin = {-61.3372,41.0981}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Clock clock1 annotation(Placement(visible = true, transformation(origin = {-72.2739,-17.9264}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.BooleanConstant booleanconstant2(k = false) annotation(Placement(visible = true, transformation(origin = {-13.0556,7.22227}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Gain SpeedSlope(k = 10) annotation(Placement(visible = true, transformation(origin = {-25.6525,-19.283}, extent = {{-9.91736,-9.91736},{9.91736,9.91736}}, rotation = 0)));
equation
  connect(engine1.Speed,SpeedSlope.y) annotation(Line(points = {{17.9444,35.9889},{1.11111,35.9889},{1.11111,-19.1667},{-14.7434,-19.1667},{-14.7434,-19.283}}));
  connect(clock1.y,SpeedSlope.u) annotation(Line(points = {{-59.0739,-17.9264},{-38.8889,-17.9264},{-38.8889,-19.283},{-37.5533,-19.283}}));
  connect(SpeedSlope.y,engine2.Speed) annotation(Line(points = {{-14.7434,-19.283},{19.4444,-19.283},{19.4444,-19.8445},{20.1666,-19.8445}}));
  connect(booleanconstant2.y,engine2.SwitchOn) annotation(Line(points = {{-4.8594,7.22227},{19.1667,7.22227},{19.1667,-10.1778},{19.8,-10.1778}}));
  connect(engine2.TorqueDemand,ToqueDemand.y) annotation(Line(points = {{19.8666,-14.1445},{4.72222,-14.1445},{4.72222,33.3333},{-48.6111,33.3333},{-48.6111,41.0981},{-48.1372,41.0981}}));
  connect(ToqueDemand.y,engine1.TorqueDemand) annotation(Line(points = {{-48.1372,41.0981},{16.9444,41.0981},{16.9444,41.6889},{17.6444,41.6889}}));
  connect(booleanconstant1.y,engine1.SwitchOn) annotation(Line(points = {{-2.67076,52.7778},{16.6667,52.7778},{16.6667,45.6556},{17.5778,45.6556}}));
  connect(engine1.flange_b,fixed1.flange) annotation(Line(points = {{38.618,40.8763},{61.1111,40.8763},{61.1111,34.354},{61.1111,34.354}}));
  connect(engine2.flange_b,fixed1.flange) annotation(Line(points = {{40.8402,-14.957},{61.6667,-14.957},{61.6667,34.354},{61.1111,34.354}}));
end EngineTest;

