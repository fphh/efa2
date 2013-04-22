model Battery
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {225,225,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.Solid, lineThickness = 0.25, extent = {{-76.2013,-48.4622},{74.8721,28.4547}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-59.7999,-93.541},{58.9598,-52.9208}}, textString = "Battery"),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-106.187,40.6352},{46.655,65.3458}}, textString = "+"),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-39.0233,35.8857},{84.295,72.412}}, textString = "-"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {150,150,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.Solid, lineThickness = 0.25, extent = {{-84.5815,28.4875},{81.9383,39.9413}}),Line(points = {{-52.5698,20.2643},{-52.5698,-39.0602},{-17.3275,-39.0602},{-17.3275,20.2643}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{15.859,19.6769},{15.859,-38.4728},{48.1645,-38.4728},{48.1645,19.9706}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{76.0213,17.0515},{111.901,-15.6306}})}));
  parameter Real resistance(start = 0.02, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real voltage(start = 200, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Sources.ConstantVoltage constantvoltage1(V = voltage) annotation(Placement(visible = true, transformation(origin = {-6.38889,-54.7222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Interfaces.NegativePin pin_n annotation(Placement(visible = true, transformation(origin = {49.3759,51.8221}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {49.3759,51.8221}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _internalCurrent_log annotation(Placement(visible = true, transformation(origin = {71.9444,78.8889}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _internalVoltage_log annotation(Placement(visible = true, transformation(origin = {71.3889,79.1667}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _poleCurrent_log annotation(Placement(visible = true, transformation(origin = {72.2222,79.1667}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _poleVoltage_log annotation(Placement(visible = true, transformation(origin = {72.2222,79.1667}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  BatteryManagement batterymanagement1 annotation(Placement(visible = true, transformation(origin = {-47.9574,20.6039}, extent = {{-12,12},{12,-12}}, rotation = -90)));
  Modelica.Electrical.Analog.Interfaces.PositivePin pin_p annotation(Placement(visible = true, transformation(origin = {-51.2307,51.838}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-51.2307,51.838}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Electrical.Analog.Basic.Resistor resistor_innerResistance(R = resistance) annotation(Placement(visible = true, transformation(origin = {-51.1222,-24.0066}, extent = {{-12,12},{12,-12}}, rotation = -90)));
  ControlBus controlbus1 annotation(Placement(visible = true, transformation(origin = {93.7833,1.06572}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {93.7833,1.06572}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(batterymanagement1.pin_n,pin_n) annotation(Line(points = {{-57.7574,30.8039},{-25.9325,30.8039},{-25.9325,30.5506},{49.3759,30.5506},{49.3759,51.8221}}));
  connect(batterymanagement1.controlbus1,controlbus1) annotation(Line(points = {{-43.824,11.9706},{-43.6945,11.9706},{-43.6945,1.06572},{93.7833,1.06572},{93.7833,1.06572}}));
  connect(resistor_innerResistance.n,constantvoltage1.p) annotation(Line(points = {{-51.1222,-36.0066},{-51.5092,-36.0066},{-51.5092,-54.7222},{-18.3889,-54.7222}}));
  connect(batterymanagement1.positivepin1,resistor_innerResistance.p) annotation(Line(points = {{-52.624,11.9706},{-52.5755,11.9706},{-52.5755,-12.0066},{-51.1222,-12.0066}}));
  connect(pin_p,batterymanagement1.pin_p) annotation(Line(points = {{-51.2307,51.838},{-52.9307,51.838},{-52.9307,31.0706},{-52.224,31.0706}}));
  connect(constantvoltage1.n,pin_n) annotation(Line(points = {{5.61111,-54.7222},{28.0225,-54.7222},{28.0225,-54.7063},{49.3759,-54.7063},{49.3759,51.8221}}));
  _poleCurrent_log = pin_p.i;
  _poleVoltage_log = pin_p.v;
  _internalCurrent_log = constantvoltage1.i;
  _internalVoltage_log = constantvoltage1.v;
end Battery;

