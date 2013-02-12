model engineDragTorque
  parameter Real dragTorqueMin(start = -30, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real dragTorqueMax(start = -60, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const(k = dragTorqueMin) annotation(Placement(visible = true, transformation(origin = {-31.9444,17.7778}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Gain DragTorqueSlope(k = (dragTorqueMax - dragTorqueMin) / 500) annotation(Placement(visible = true, transformation(origin = {-40.2812,-22.3414}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Math.Add add1 annotation(Placement(visible = true, transformation(origin = {4.99997,-1.66669}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-83.8889,-20.8333}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-83.8889,-20.8333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon());
  Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {55.5556,-1.94444}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {55.5556,-1.94444}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(u,DragTorqueSlope.u) annotation(Line(points = {{-83.8889,-20.8333},{-49.1667,-20.8333},{-49.1667,-22.3414},{-48.4096,-22.3414}}));
  connect(add1.y,y) annotation(Line(points = {{12.451,-1.66669},{47.5,-1.66669},{47.5,-1.94444},{55.5556,-1.94444}}));
  connect(DragTorqueSlope.y,add1.u2) annotation(Line(points = {{-32.8301,-22.3414},{-10.8333,-22.3414},{-10.8333,-5.83333},{-3.61111,-5.83333},{-3.61111,-5.7309},{-3.12846,-5.7309}}));
  connect(const.y,add1.u1) annotation(Line(points = {{-23.7483,17.7778},{-8.33333,17.7778},{-8.33333,2.22222},{-3.12846,2.22222},{-3.12846,2.39752}}));
end engineDragTorque;

