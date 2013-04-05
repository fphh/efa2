model ElectricVehicleSystem
  Modelica.Electrical.Analog.Interfaces.PositivePin pin_p annotation(Placement(visible = true, transformation(origin = {-83.0556,42.7778}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-83.0556,42.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-57.2222,63.3333},{88.8889,-53.3333}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-28.6111,30.8333},{67.7778,-23.8889}}, textString = "EVS")}), Diagram(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-20.8333,-58.0556},{31.9444,-68.6111}}, textString = "Using ramp to avoid initial solver problem")}));
  Modelica.Electrical.Analog.Sources.SignalCurrent signalcurrent1 annotation(Placement(visible = true, transformation(origin = {-4.16669,35.5555}, extent = {{7.45106,-7.45106},{-7.45106,7.45106}}, rotation = -270)));
  parameter Real powerDemand(start = 0) annotation(Placement(visible = true, transformation(origin = {70.8333,72.2222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Division division_calculateCurrent annotation(Placement(visible = true, transformation(origin = {-28.3333,31.6667}, extent = {{-5.59809,-5.59809},{5.59809,5.59809}}, rotation = 0)));
  Modelica.Electrical.Analog.Sensors.VoltageSensor voltagesensor1 annotation(Placement(visible = true, transformation(origin = {-79.7222,19.1667}, extent = {{6.1579,-6.1579},{-6.1579,6.1579}}, rotation = -270)));
  Modelica.Electrical.Analog.Interfaces.NegativePin pin_n annotation(Placement(visible = true, transformation(origin = {-82.2222,-22.7778}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-82.2222,-22.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Ramp ramp_powerDemand(height = powerDemand, duration = 1) annotation(Placement(visible = true, transformation(origin = {-60.5556,74.7222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _current_log annotation(Placement(visible = true, transformation(origin = {71.3889,42.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _voltage_log annotation(Placement(visible = true, transformation(origin = {71.6667,12.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _powerDemand_log annotation(Placement(visible = true, transformation(origin = {71.1111,-20.2778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(ramp_powerDemand.y,division_calculateCurrent.u1) annotation(Line(points = {{-47.3556,74.7222},{-35.8333,74.7222},{-35.8333,35.0256},{-35.051,35.0256}}));
  connect(voltagesensor1.p,pin_n) annotation(Line(points = {{-79.7222,13.0088},{-80.2778,13.0088},{-80.2778,-20.2778},{-90,-20.2778}}));
  connect(voltagesensor1.v,division_calculateCurrent.u2) annotation(Line(points = {{-73.5643,19.1667},{-73.5643,18.6111},{-35.8333,18.6111},{-35.8333,28.3078},{-35.051,28.3078}}));
  connect(pin_p,voltagesensor1.n) annotation(Line(points = {{-83.0556,42.7778},{-79.7222,42.7778},{-79.7222,31.1579},{-79.7222,25.3246}}));
  connect(division_calculateCurrent.y,signalcurrent1.i) annotation(Line(points = {{-22.1754,31.6667},{-10,31.6667},{-10,35.5555},{-9.38243,35.5555}}));
  connect(signalcurrent1.p,pin_n) annotation(Line(points = {{-4.16669,28.1045},{-4.16671,28.1045},{-4.16671,-19.4444},{-90,-19.4444},{-90,-20.2778}}));
  connect(signalcurrent1.n,pin_p) annotation(Line(points = {{-4.16669,43.0066},{-79.1667,43.0066},{-79.1667,42.7778},{-83.0556,42.7778}}));
  _powerDemand_log = powerDemand;
  _voltage_log = pin_p.v;
  _current_log = pin_p.i;
end ElectricVehicleSystem;

