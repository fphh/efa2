model BatteryManagement
  Modelica.Blocks.Interfaces.RealInput input_batteryVoltage annotation(Placement(visible = true, transformation(origin = {-74.7287,52.4031}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-74.7287,52.4031}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-34.4186,27.2868},{61.3954,-42.7907}}, textString = "BEM"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-61.0852,62.3255},{88.3721,-80.6202}})}), Diagram(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-70.8269,97.4161},{85.8269,50.7687}}, textString = "Calculate SOC from Measurements")}));
  Modelica.Blocks.Interfaces.RealInput input_batteryCurrent annotation(Placement(visible = true, transformation(origin = {-75.3488,-62.6357}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-75.3488,-62.6357}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput output_SOC annotation(Placement(visible = true, transformation(origin = {101.705,5.5814}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {101.705,5.5814}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Product product_calulatePower annotation(Placement(visible = true, transformation(origin = {-37.8295,13.3333}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Continuous.Integrator integrator_calculateEnergy annotation(Placement(visible = true, transformation(origin = {0.930233,18.3333}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_toSOC(k = 1 / (1 * 3600 * 1000)) annotation(Placement(visible = true, transformation(origin = {36.2791,12.4031}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Add add_SOC annotation(Placement(visible = true, transformation(origin = {71.9509,8.14599}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_startSOC(k = 0.35) annotation(Placement(visible = true, transformation(origin = {-7.41599,-23.7015}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
equation
  connect(const_startSOC.y,add_SOC.u2) annotation(Line(points = {{1.59979,-23.7015},{47.4419,-23.7015},{47.4419,-1.24031},{63.8225,-1.24031},{63.8225,4.08178}}));
  connect(gain_toSOC.y,add_SOC.u1) annotation(Line(points = {{44.4753,12.4031},{59.845,12.4031},{59.845,12.2102},{63.8225,12.2102}}));
  connect(output_SOC,add_SOC.y) annotation(Line(points = {{101.705,5.5814},{85.8915,5.5814},{85.8915,8.14599},{79.402,8.14599}}));
  connect(integrator_calculateEnergy.y,gain_toSOC.u) annotation(Line(points = {{9.94601,18.3333},{21.7054,18.3333},{21.7054,12.4031},{27.3378,12.4031}}));
  connect(product_calulatePower.y,integrator_calculateEnergy.u) annotation(Line(points = {{-27.9121,13.3333},{-13.6434,13.3333},{-13.6434,18.3333},{-8.90516,18.3333}}));
  connect(input_batteryVoltage,product_calulatePower.u1) annotation(Line(points = {{-74.7287,52.4031},{-56.7442,52.4031},{-56.7442,20.7752},{-48.6484,20.7752},{-48.6484,18.7428}}));
  connect(input_batteryCurrent,product_calulatePower.u2) annotation(Line(points = {{-75.3488,-62.6357},{-61.0853,-62.6357},{-61.0853,5.89147},{-48.6484,5.89147},{-48.6484,7.92383}}));
end BatteryManagement;

