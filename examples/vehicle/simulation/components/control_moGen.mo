model Control_MoGen
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-59.7222,60.8333},{55.5556,-68.6111}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-46.9444,-26.9444},{47.5,21.9444}}, textString = "MoGen")}));
  Modelica.Blocks.Interfaces.RealOutput output_engineTorque annotation(Placement(visible = true, transformation(origin = {80,24.7222}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {73.0556,24.7222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput output_generatorTorque annotation(Placement(visible = true, transformation(origin = {73.0556,-33.8889}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {73.0556,-33.8889}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Logical.Switch switch_engine annotation(Placement(visible = true, transformation(origin = {34.7222,26.9444}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Interfaces.BooleanOutput output_engineOn annotation(Placement(visible = true, transformation(origin = {73.0556,65.2778}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {73.0556,24.7222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Logical.Switch switch_generator annotation(Placement(visible = true, transformation(origin = {39.1667,-32.7778}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_zero(k = 0) annotation(Placement(visible = true, transformation(origin = {-15.8333,-55.2778}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback_speed annotation(Placement(visible = true, transformation(origin = {-43.1653,-9.16794}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_speedControl(k = 5) annotation(Placement(visible = true, transformation(origin = {-21.221,-9.67833}, extent = {{-5.59809,-5.59809},{5.59809,5.59809}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput input_speed annotation(Placement(visible = true, transformation(origin = {-82.7778,-67.7778}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-82.7778,-67.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput input_speedDemand annotation(Placement(visible = true, transformation(origin = {-82.5,-28.8889}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-82.5,-28.8889}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput input_torqueDemand annotation(Placement(visible = true, transformation(origin = {-81.6667,10.5555}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-81.6667,10.5555}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.BooleanInput input_switchOn annotation(Placement(visible = true, transformation(origin = {-81.3889,46.1111}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-81.3889,46.1111}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(switch_generator.u2,input_switchOn) annotation(Line(points = {{31.0383,-32.7778},{-4.16667,-32.7778},{-4.16667,46.1111},{-81.3889,46.1111},{-81.3889,46.1111}}));
  connect(switch_engine.u2,input_switchOn) annotation(Line(points = {{26.5938,26.9444},{-4.16667,26.9444},{-4.16667,46.1111},{-81.3889,46.1111},{-81.3889,46.1111}}));
  connect(input_speedDemand,feedback_speed.u1) annotation(Line(points = {{-82.5,-28.8889},{-65.2778,-28.8889},{-65.2778,-9.16794},{-48.0916,-9.16794}}));
  connect(input_speed,feedback_speed.u2) annotation(Line(points = {{-82.7778,-67.7778},{-43.0556,-67.7778},{-43.0556,-14.0943},{-43.1653,-14.0943}}));
  connect(input_torqueDemand,switch_engine.u1) annotation(Line(points = {{-81.6667,10.5555},{-20.8333,10.5555},{-20.8333,32.2222},{26.5938,32.2222},{26.5938,32.3634}}));
  connect(gain_speedControl.y,switch_generator.u1) annotation(Line(points = {{-15.0631,-9.67833},{26.9444,-9.67833},{26.9444,-27.3588},{31.0383,-27.3588}}));
  connect(const_zero.y,switch_generator.u3) annotation(Line(points = {{-8.38224,-55.2778},{26.6667,-55.2778},{26.6667,-38.1968},{31.0383,-38.1968}}));
  connect(feedback_speed.y,gain_speedControl.u) annotation(Line(points = {{-37.6232,-9.16794},{-28.0556,-9.16794},{-28.0556,-9.67833},{-27.9387,-9.67833}}));
  connect(const_zero.y,switch_engine.u3) annotation(Line(points = {{-8.38228,-55.2778},{6.94444,-55.2778},{6.94444,21.6667},{26.5938,21.6667},{26.5938,21.5254}}));
  connect(switch_engine.y,output_engineTorque) annotation(Line(points = {{42.1733,26.9444},{70.8333,26.9444},{70.8333,24.7222},{80,24.7222}}));
  connect(switch_generator.y,output_generatorTorque) annotation(Line(points = {{46.6177,-32.7778},{64.1667,-32.7778},{64.1667,-33.8889},{73.0556,-33.8889}}));
  connect(input_switchOn,output_engineOn) annotation(Line(points = {{-80,64.4444},{62.5,64.4444},{62.5,65.2778},{73.0556,65.2778}}));
end Control_MoGen;

