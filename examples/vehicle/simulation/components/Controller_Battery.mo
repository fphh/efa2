model BatteryManagement
  annotation(Diagram(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-70.8269,97.4161},{85.8269,50.7687}}, textString = "Calculate SOC from Measurements")}), Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-34.4186,27.2868},{61.3954,-42.7907}}, textString = "BEM"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-61.0852,62.3255},{88.3721,-80.6202}}),Rectangle(rotation = 0, lineColor = {255,170,0}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 2, extent = {{52.5755,46.8917},{88.8099,22.0249}})}));
  Modelica.Electrical.Analog.Interfaces.PositivePin pin_p annotation(Placement(visible = true, transformation(origin = {-87.2222,-35.5556}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-87.2222,-35.5556}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Interfaces.PositivePin positivepin1 annotation(Placement(visible = true, transformation(origin = {71.9444,-38.8889}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {71.9444,-38.8889}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Sensors.CurrentSensor currentsensor1 annotation(Placement(visible = true, transformation(origin = {-1.38889,-35.5556}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Electrical.Analog.Sensors.VoltageSensor voltagesensor1 annotation(Placement(visible = true, transformation(origin = {-27.7778,-68.3333}, extent = {{-6.77369,6.77369},{6.77369,-6.77369}}, rotation = -90)));
  Modelica.Blocks.Math.Product product_calulatePower annotation(Placement(visible = true, transformation(origin = {-50.3295,39.7222}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  ControlBus controlbus1 annotation(Placement(visible = true, transformation(origin = {71.9445,34.4445}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {71.9445,34.4445}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Add add_SOC annotation(Placement(visible = true, transformation(origin = {46.307,34.5349}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Continuous.Integrator integrator_calculateEnergy annotation(Placement(visible = true, transformation(origin = {-18.6746,39.7488}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_toSOC(k = 1 / (1 * 3600 * 1000)) annotation(Placement(visible = true, transformation(origin = {13.1219,39.1472}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_startSOC(k = 0.35) annotation(Placement(visible = true, transformation(origin = {-7.41849,12.1906}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Electrical.Analog.Interfaces.NegativePin pin_n annotation(Placement(visible = true, transformation(origin = {2.38899,81.7436}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {2.38899,81.7436}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(voltagesensor1.n,pin_n) annotation(Line(points = {{-27.7778,-75.107},{-83.3333,-75.107},{-83.3333,81.7436},{2.38899,81.7436}}));
  connect(const_startSOC.y,add_SOC.u2) annotation(Line(points = {{1.59729,12.1906},{22.4419,12.1906},{22.4419,12.36},{38.1786,12.36},{38.1786,30.4707}}));
  connect(integrator_calculateEnergy.y,gain_toSOC.u) annotation(Line(points = {{-9.65882,39.7488},{9.2054,39.7488},{9.2054,39.1472},{4.18064,39.1472}}));
  connect(gain_toSOC.y,add_SOC.u1) annotation(Line(points = {{21.3181,39.1472},{47.345,39.1472},{47.345,38.5991},{38.1786,38.5991}}));
  connect(product_calulatePower.y,integrator_calculateEnergy.u) annotation(Line(points = {{-40.4121,39.7222},{-26.1434,39.7222},{-26.1434,39.7488},{-28.51,39.7488}}));
  connect(add_SOC.y,controlbus1.batteryBus.SOC) annotation(Line(points = {{66.902,34.5349},{77.7778,34.5349},{77.7778,36.1111},{79.4889,36.1111}}));
  connect(currentsensor1.i,product_calulatePower.u2) annotation(Line(points = {{-1.38889,-42.3293},{-1.38889,-50},{-71.1111,-50},{-71.1111,34.4444},{-61.1484,34.4444},{-61.1484,34.3127}}));
  connect(voltagesensor1.v,product_calulatePower.u1) annotation(Line(points = {{-34.5515,-68.3333},{-34.5515,-68.6111},{-78.8889,-68.6111},{-78.8889,45.2778},{-61.1484,45.2778},{-61.1484,45.1317}}));
  connect(pin_p,voltagesensor1.p) annotation(Line(points = {{-87.2222,-35.5556},{-28.3333,-35.5556},{-28.3333,-61.5596},{-27.7778,-61.5596}}));
  connect(currentsensor1.n,positivepin1) annotation(Line(points = {{5.3848,-35.5556},{71.6667,-35.5556},{71.6667,-38.8889},{71.9444,-38.8889}}));
  connect(pin_p,currentsensor1.p) annotation(Line(points = {{-87.2222,-35.5556},{-8.33333,-35.5556},{-8.33333,-35.5556},{-8.16258,-35.5556}}));
end BatteryManagement;

