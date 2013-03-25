model engineDragTorque
  Modelica.Blocks.Math.Add add1 annotation(Placement(visible = true, transformation(origin = {-19.1667,-32.7778}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Math.Gain DragTorqueSlope(k = (dragTorqueMax - dragTorqueMin) / 500) annotation(Placement(visible = true, transformation(origin = {-47.2256,-38.1747}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const(k = dragTorqueMin) annotation(Placement(visible = true, transformation(origin = {-47.2222,-11.3889}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  parameter Real dragTorqueMin(start = -30, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real dragTorqueMax(start = -60, unit = "1") "Positive power multiplied, negative power devided by efficiency" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-83.3333,-38.6111}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-83.3333,-38.6111}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon(graphics = {Line(points = {{-56.9444,-36.1111},{-35.5556,-36.9444},{-19.7222,-40.8333},{-4.44444,-54.1667}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-61.1111,-9.16667},{-61.6667,-63.3333},{11.6667,-63.6111}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}));
  Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {26.1111,-34.4444}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {26.1111,-34.4444}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(add1.y,y) annotation(Line(points = {{-11.7156,-32.7778},{16.9444,-32.7778},{16.9444,-34.4444},{26.1111,-34.4444}}));
  connect(u,DragTorqueSlope.u) annotation(Line(points = {{-83.3333,-38.6111},{-55.2778,-38.6111},{-55.2778,-38.1747},{-55.354,-38.1747}}));
  connect(DragTorqueSlope.y,add1.u2) annotation(Line(points = {{-39.7745,-38.1747},{-27.7778,-38.1747},{-27.7778,-36.842},{-27.2951,-36.842}}));
  connect(const.y,add1.u1) annotation(Line(points = {{-39.026,-11.3889},{-28.0556,-11.3889},{-28.0556,-28.3333},{-27.2951,-28.3333},{-27.2951,-28.7136}}));
end engineDragTorque;

