model GearboxSingleSpeed
  Modelica.Mechanics.Rotational.Components.BearingFriction bearingfriction1 annotation(Placement(visible = true, transformation(origin = {-47.4419,3.72093}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.BearingFriction bearingfriction2 annotation(Placement(visible = true, transformation(origin = {76.8992,2.7907}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.Inertia inertia1 annotation(Placement(visible = true, transformation(origin = {-17.0543,4.03101}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.Inertia inertia2 annotation(Placement(visible = true, transformation(origin = {45.2713,3.41085}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.LossyGear lossygear1 annotation(Placement(visible = true, transformation(origin = {15.814,3.10078}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-45.5814,38.1395},{62.0155,-18.2946}}, textString = "Gearbox"),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-47.7519,42.4806},{66.6667,-27.907}})}));
  Modelica.Mechanics.Rotational.Interfaces.Flange_b flange_b annotation(Placement(visible = true, transformation(origin = {93.0232,7.75194}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {93.0232,7.75194}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Interfaces.Flange_a flange_a annotation(Placement(visible = true, transformation(origin = {-88.6821,7.75194}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-88.6821,7.75194}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(bearingfriction1.flange_a,flange_a) annotation(Line(points = {{-59.4419,3.72093},{-87.7519,3.72093},{-87.7519,7.75194},{-88.6821,7.75194}}));
  connect(bearingfriction2.flange_b,flange_b) annotation(Line(points = {{88.8992,2.7907},{114.109,2.7907},{114.109,3.72093},{116.279,3.72093}}));
  connect(bearingfriction1.flange_b,inertia1.flange_a) annotation(Line(points = {{-35.4419,3.72093},{-28.5271,3.72093},{-28.5271,4.03101},{-29.0543,4.03101}}));
  connect(inertia1.flange_b,lossygear1.flange_a) annotation(Line(points = {{-5.05426,4.03101},{3.72093,4.03101},{3.72093,3.10078},{3.81395,3.10078}}));
  connect(lossygear1.flange_b,inertia2.flange_a) annotation(Line(points = {{27.814,3.10078},{31.0078,3.10078},{31.0078,2.7907},{33.2713,2.7907},{33.2713,3.41085}}));
  connect(inertia2.flange_b,bearingfriction2.flange_a) annotation(Line(points = {{57.2713,3.41085},{65.7364,3.41085},{65.7364,2.7907},{64.8992,2.7907}}));
end GearboxSingleSpeed;

