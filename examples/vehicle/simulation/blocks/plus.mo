block Plus
  Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-84.1667,8.61111}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-84.1667,8.61111}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-57.2222,44.7222},{46.1111,-42.2222}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-53.3333,41.9444},{45,-33.6111}}, textString = "+")}));
  Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {83.0556,3.88889}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {83.0556,3.88889}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Add add1 annotation(Placement(visible = true, transformation(origin = {25.2778,15.8333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const(k = k) annotation(Placement(visible = true, transformation(origin = {-43.3333,35.8333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real k(start = 1, unit = "1") "Constant Value to Add" annotation(Placement(visible = true, transformation(origin = {71.2644,78.1609}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(add1.u1,const.y) annotation(Line(points = {{10.8778,23.0333},{-18.8889,23.0333},{-18.8889,35.8333},{-30.1333,35.8333}}));
  connect(u,add1.u2) annotation(Line(points = {{-84.1667,8.61111},{1.94444,8.61111},{1.94444,8.63333},{10.8778,8.63333}}));
  connect(add1.y,y) annotation(Line(points = {{38.4778,15.8333},{75.5556,15.8333},{75.5556,3.88889},{83.0556,3.88889}}));
end Plus;

