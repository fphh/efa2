model Engine
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-72.694,31.8433},{78.3243,-53.4341}}),Ellipse(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-46.6135,8.76566},{-21.8379,10.561}}),Ellipse(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-10.3478,11.6382},{18.7366,8.04756}}),Ellipse(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{30.2267,12.3564},{57.1567,7.68846}}),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-48.6371,-83.8551},{59.7509,-53.1885}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-61.8084,-36.6064},{68.5322,-10.6724}}, textString = "Engine")}));
  Modelica.Blocks.Interfaces.RealInput TorqueDemand annotation(Placement(visible = true, transformation(origin = {-80.4309,1.0772}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-85.4578,5.74506}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput Speed annotation(Placement(visible = true, transformation(origin = {-72.1724,-38.4201}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-85.4578,5.74506}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain DragTorqueSlope(k = -30 / 600) annotation(Placement(visible = true, transformation(origin = {-39.4478,-38.7302}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Components.Inertia inertia1(J = 0.16) annotation(Placement(visible = true, transformation(origin = {96.6351,3.96587}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Sources.Torque torque1 annotation(Placement(visible = true, transformation(origin = {66.8326,4.32493}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput FuelPower annotation(Placement(visible = true, transformation(origin = {96.8816,-45.5035}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {96.8816,-45.5035}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain Efficiency(k = 1 / 0.35) annotation(Placement(visible = true, transformation(origin = {72.5287,-43.6591}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Modelica.Blocks.Math.Product Power annotation(Placement(visible = true, transformation(origin = {44.6512,-43.7209}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.BooleanInput SwitchOn annotation(Placement(visible = true, transformation(origin = {-85.2713,-45.2713}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-85.2713,-45.2713}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Mechanics.Rotational.Interfaces.Flange_b flange_b annotation(Placement(visible = true, transformation(origin = {92.2793,5.72836}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {92.2793,5.72836}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Logical.Switch switch1 annotation(Placement(visible = true, transformation(origin = {17.8166,1.53747}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.VariableLimiter ToqueLimit annotation(Placement(visible = true, transformation(origin = {-3.08947,6.04329}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.DeadZone deadzone1(uMax = 30, uMin = 0) annotation(Placement(visible = true, transformation(origin = {-38.6111,58.0556}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = 1.2) annotation(Placement(visible = true, transformation(origin = {1.38889,58.3333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(gain1.y,ToqueLimit.limit1) annotation(Line(points = {{14.5889,58.3333},{31.1111,58.3333},{31.1111,31.3889},{-40,31.3889},{-40,12.5},{-12.9249,12.5},{-12.9249,12.6002}}));
  connect(deadzone1.y,gain1.u) annotation(Line(points = {{-25.4111,58.0556},{-12.7778,58.0556},{-12.7778,58.3333},{-13.0111,58.3333}}));
  connect(deadzone1.u,Speed) annotation(Line(points = {{-53.0111,58.0556},{-67.2222,58.0556},{-67.2222,-38.4201},{-72.1724,-38.4201}}));
  connect(DragTorqueSlope.y,ToqueLimit.limit2) annotation(Line(points = {{-26.2478,-38.7302},{-21.544,-38.7302},{-21.544,-1.33453},{-12.9249,-1.33453},{-12.9249,-0.513639}}));
  connect(TorqueDemand,ToqueLimit.u) annotation(Line(points = {{-80.4309,1.0772},{-35.8398,1.0772},{-35.8398,6.04329},{-12.9249,6.04329}}));
  connect(ToqueLimit.y,switch1.u1) annotation(Line(points = {{5.92631,6.04329},{5.83337,6.04329},{5.83337,6.46379},{10.4271,6.46379}}));
  connect(Power.u1,switch1.y) annotation(Line(points = {{30.2512,-36.5209},{22.009,-36.5209},{22.009,-17.9845},{24.5801,-17.9845},{24.5801,-10.6848},{24.5903,-10.6848},{24.5903,1.53747}}));
  connect(SwitchOn,switch1.u2) annotation(Line(points = {{-85.2713,-45.2713},{5.7558,-45.2713},{5.7558,1.53747},{10.4271,1.53747}}));
  connect(switch1.y,torque1.tau) annotation(Line(points = {{24.5903,1.53747},{51.1111,1.53747},{51.1111,4.32493},{52.4326,4.32493}}));
  connect(DragTorqueSlope.y,switch1.u3) annotation(Line(points = {{-26.2478,-38.7302},{7.44186,-38.7302},{7.44186,-3.42377},{10.4271,-3.42377},{10.4271,-3.38885}}));
  connect(inertia1.flange_b,flange_b) annotation(Line(points = {{108.635,3.96587},{116.384,3.96587},{116.384,5.72836},{92.2793,5.72836}}));
  connect(Power.u2,Speed) annotation(Line(points = {{30.2512,-50.9209},{-6.51163,-50.9209},{-6.51163,-62.0155},{-67.5969,-62.0155},{-67.5969,-38.4201},{-72.1724,-38.4201}}));
  connect(Power.y,Efficiency.u) annotation(Line(points = {{57.8512,-43.7209},{43.4109,-43.7209},{43.4109,-43.6591},{65.1392,-43.6591}}));
  connect(Efficiency.y,FuelPower) annotation(Line(points = {{79.3023,-43.6591},{87.905,-43.6591},{87.905,-45.5035},{96.8816,-45.5035}}));
  connect(torque1.flange,inertia1.flange_a) annotation(Line(points = {{78.8326,4.32493},{85.5041,4.32493},{85.5041,3.96587},{84.6351,3.96587}}));
  connect(Speed,DragTorqueSlope.u) annotation(Line(points = {{-72.1724,-38.4201},{-50.9874,-38.4201},{-50.9874,-38.7302},{-53.8478,-38.7302}}));
end Engine;

