model motorMaxTorque
  Modelica.Blocks.Math.Division division1 annotation(Placement(visible = true, transformation(origin = {-16.1341,32.9981}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput speed annotation(Placement(visible = true, transformation(origin = {-92.2222,12.2222}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-92.2222,12.2222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon(graphics = {Line(points = {{-59.7222,50.8333},{-59.7222,-63.0556},{80.2778,-63.0556},{80.2778,-63.0556}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-59.4444,28.6111},{-7.5,28.6111},{-4.72222,21.1111},{2.5,3.88889},{14.7222,-7.22222},{37.5,-17.5},{62.5,-21.1111},{71.9444,-21.3889}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}));
  Modelica.Blocks.Nonlinear.Limiter avoidDivisionByZero(uMin = 1, uMax = 1000000) annotation(Placement(visible = true, transformation(origin = {-45.2685,11.0357}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Min min1 annotation(Placement(visible = true, transformation(origin = {37.2543,28.4871}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {88.3333,28.3333}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {88.3333,28.3333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real maxTorque(start = 200, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real maxPower(start = 60000, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant P_max(k = maxPower) annotation(Placement(visible = true, transformation(origin = {-58.7052,46.0055}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant T_max(k = maxTorque) annotation(Placement(visible = true, transformation(origin = {-48.5583,-15.2433}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
equation
  connect(T_max.y,min1.u2) annotation(Line(points = {{-41.1072,-15.2433},{27.7778,-15.2433},{27.7778,24.0165},{28.313,24.0165}}));
  connect(P_max.y,division1.u1) annotation(Line(points = {{-51.2541,46.0055},{-26.1111,46.0055},{-26.1111,37.4687},{-25.0754,37.4687}}));
  connect(min1.y,y) annotation(Line(points = {{45.4505,28.4871},{80.8333,28.4871},{80.8333,28.3333},{88.3333,28.3333}}));
  connect(division1.y,min1.u1) annotation(Line(points = {{-7.93789,32.9981},{24.1667,32.9981},{24.1667,32.9577},{28.3131,32.9577}}));
  connect(avoidDivisionByZero.y,division1.u2) annotation(Line(points = {{-37.0723,11.0357},{-26.1111,11.0357},{-26.1111,28.5275},{-25.0753,28.5275}}));
  connect(speed,avoidDivisionByZero.u) annotation(Line(points = {{-92.2222,12.2222},{-56.6667,12.2222},{-56.6667,11.0357},{-54.2097,11.0357}}));
end motorMaxTorque;

