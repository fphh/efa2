model Controller_Battery
  Modelica.Blocks.Interfaces.RealInput Voltage annotation(Placement(visible = true, transformation(origin = {-74.7287,52.4031}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-74.7287,52.4031}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-45.2713,81.8605},{58.6047,66.0465}}, textString = "Calculate SOC from Measurements")}), Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-34.4186,27.2868},{61.3954,-42.7907}}, textString = "BEM"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-61.0852,62.3255},{88.3721,-80.6202}})}));
  Modelica.Blocks.Interfaces.RealInput Flow annotation(Placement(visible = true, transformation(origin = {-75.3488,-62.6357}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-75.3488,-62.6357}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Product product1 annotation(Placement(visible = true, transformation(origin = {-37.8295,13.3333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Continuous.Integrator integrator1 annotation(Placement(visible = true, transformation(origin = {0.930233,13.3333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Add add1 annotation(Placement(visible = true, transformation(origin = {74.7287,6.20155}, extent = {{-9.91736,-9.91736},{9.91736,9.91736}}, rotation = 0)));
  Modelica.Blocks.Math.Gain toSOC(k = 1 / (0.01 * 3600 * 1000)) annotation(Placement(visible = true, transformation(origin = {36.2791,12.4031}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput SOC annotation(Placement(visible = true, transformation(origin = {101.705,5.5814}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {101.705,5.5814}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant Start_SOC(k = 0.35) annotation(Placement(visible = true, transformation(origin = {11.4729,-32.8682}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(Start_SOC.y,add1.u2) annotation(Line(points = {{24.6729,-32.8682},{47.4419,-32.8682},{47.4419,-1.24031},{62.8279,-1.24031},{62.8279,0.251137}}));
  connect(SOC,add1.y) annotation(Line(points = {{101.705,5.5814},{85.8915,5.5814},{85.8915,6.20155},{85.6378,6.20155}}));
  connect(toSOC.y,add1.u1) annotation(Line(points = {{49.4791,12.4031},{59.845,12.4031},{59.845,12.152},{62.8279,12.152}}));
  connect(integrator1.y,toSOC.u) annotation(Line(points = {{14.1302,13.3333},{21.7054,13.3333},{21.7054,12.4031},{21.8791,12.4031}}));
  connect(product1.y,integrator1.u) annotation(Line(points = {{-24.6295,13.3333},{-13.6434,13.3333},{-13.6434,13.3333},{-13.4698,13.3333}}));
  connect(Flow,product1.u2) annotation(Line(points = {{-75.3488,-62.6357},{-61.0853,-62.6357},{-61.0853,5.89147},{-52.2295,5.89147},{-52.2295,6.13333}}));
  connect(Voltage,product1.u1) annotation(Line(points = {{-74.7287,52.4031},{-56.7442,52.4031},{-56.7442,20.7752},{-52.2295,20.7752},{-52.2295,20.5333}}));
end Controller_Battery;

