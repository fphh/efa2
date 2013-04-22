model Driver
  annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001), Diagram(), Icon(graphics = {Ellipse(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 1.75, extent = {{-30.404,43.4108},{-3.73731,17.3643}}),Line(points = {{-18.3109,14.8837},{-11.4892,-45.5814},{29.1309,-35.969},{26.3402,-75.6589},{47.1154,-66.3566}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 1.75),Ellipse(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 1.75, extent = {{24.1697,21.3953},{38.4333,-21.3953}}),Line(points = {{-16.1241,-7.44189},{1.86043,-23.2558},{24.8062,-15.1938}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 1.75),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-94.2472,84.6839},{94.5779,-81.2239}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-51.1628,52.093},{45.8915,84.3411}}, textString = "Driver")}));
  Modelica.Blocks.Math.Gain kph_to_mps(k = 1 / 3.6) annotation(Placement(visible = true, transformation(origin = {-75.9249,34.6922}, extent = {{-9.01578,-9.01578},{9.01578,9.01578}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback_speed annotation(Placement(visible = true, transformation(origin = {-48.2648,35.1759}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.Limiter LimitGas(uMin = -1, uMax = 1) annotation(Placement(visible = true, transformation(origin = {2.91689,34.6506}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain1(k = 10) annotation(Placement(visible = true, transformation(origin = {-21.0462,34.8598}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback_calculateBrake annotation(Placement(visible = true, transformation(origin = {19.7852,72.9077}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain ChangeSign(k = -1) annotation(Placement(visible = true, transformation(origin = {46.4266,72.7951}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Nonlinear.Limiter LimitBrake(uMin = 0) annotation(Placement(visible = true, transformation(origin = {70.2306,72.7951}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput input_speedDemand annotation(Placement(visible = true, transformation(origin = {-106.478,34.1578}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-106.478,34.1578}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput input_speed annotation(Placement(visible = true, transformation(origin = {-106.6,-37.1888}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-106.6,-37.1888}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput output_brakePedalPosition annotation(Placement(visible = true, transformation(origin = {106.19,32.5979}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {106.19,32.5979}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput output_gasPedalPosition annotation(Placement(visible = true, transformation(origin = {106.135,-37.2315}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {106.135,-37.2315}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _gasPedalPosition_log annotation(Placement(visible = true, transformation(origin = {-65.0862,75.8621}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _speedDemand_log annotation(Placement(visible = true, transformation(origin = {-65.5172,76.2931}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Real _vehicleSpeed_log annotation(Placement(visible = true, transformation(origin = {-65.5172,76.7241}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(feedback_calculateBrake.y,ChangeSign.u) annotation(Line(points = {{30.5852,72.9077},{37.5918,72.9077},{37.5918,72.7951},{37.4853,72.7951}}));
  connect(LimitGas.y,output_gasPedalPosition) annotation(Line(points = {{10.3679,34.6506},{20.0269,34.6506},{20.0269,-37.2315},{106.135,-37.2315}}));
  connect(input_speed,feedback_speed.u2) annotation(Line(points = {{-106.6,-37.1888},{-47.5984,-37.1888},{-47.5984,25.5759},{-48.2648,25.5759}}));
  connect(input_speedDemand,kph_to_mps.u) annotation(Line(points = {{-106.478,34.1578},{-81.7967,34.1578},{-81.7967,34.6922},{-86.7438,34.6922}}));
  connect(LimitBrake.y,output_brakePedalPosition) annotation(Line(points = {{78.4268,72.7951},{83.8826,72.7951},{83.8826,30.8358},{83.2829,30.8358}}));
  connect(ChangeSign.y,LimitBrake.u) annotation(Line(points = {{54.6228,72.7951},{64.6968,72.7951},{64.6968,72.7951},{61.2893,72.7951}}));
  connect(feedback_calculateBrake.u1,LimitGas.u) annotation(Line(points = {{10.1852,72.9077},{-5.54544,72.9077},{-5.54544,34.6506},{-5.21153,34.6506}}));
  connect(feedback_calculateBrake.u2,LimitGas.y) annotation(Line(points = {{19.7852,63.3077},{19.7852,34.538},{10.3679,34.538},{10.3679,34.6506}}));
  connect(gain1.y,LimitGas.u) annotation(Line(points = {{-13.5951,34.8598},{-4.6212,34.8598},{-4.6212,34.6506},{-5.21153,34.6506}}));
  connect(feedback_speed.y,gain1.u) annotation(Line(points = {{-37.4648,35.1759},{-28.6515,35.1759},{-28.6515,34.8598},{-29.1746,34.8598}}));
  connect(kph_to_mps.y,feedback_speed.u1) annotation(Line(points = {{-66.0076,34.6922},{-58.2271,34.6922},{-58.2271,35.1759},{-57.8648,35.1759}}));
  _vehicleSpeed_log = input_speed;
  _speedDemand_log = input_speedDemand;
  _gasPedalPosition_log = output_gasPedalPosition;
end Driver;

