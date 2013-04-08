model BrakeSystem
  Modelica.Blocks.Interfaces.RealInput input_pedalPosition annotation(Placement(visible = true, transformation(origin = {-72.9885,8.62069}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-72.9885,8.62069}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-60.4652,63.8759},{60.4651,-52.093}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-46.5116,24.186},{43.7209,-12.093}}, textString = "Brake System"),Line(points = {{-44.3411,-15.5039},{8.06202,-15.5039},{8.06202,-33.4884},{43.7209,-33.4884},{44.3411,-33.1783},{51.7829,-26.0465},{51.7829,-26.0465},{52.4031,-26.3566}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}), experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  Modelica.Blocks.Interfaces.RealOutput output_brakeTorqueFront annotation(Placement(visible = true, transformation(origin = {73.5632,51.1494}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {73.5632,51.1494}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput output_brakeTorqueRear annotation(Placement(visible = true, transformation(origin = {74.1379,-40.2299}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {74.1379,-40.2299}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback_rearAxle annotation(Placement(visible = true, transformation(origin = {24.1379,-41.3793}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_shareFrontAxle(k = 0.6) annotation(Placement(visible = true, transformation(origin = {16.092,50.5747}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_brakeSystem(k = 10000) annotation(Placement(visible = true, transformation(origin = {-28.1609,9.77011}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(input_pedalPosition,gain_brakeSystem.u) annotation(Line(points = {{-72.9885,8.62069},{-43.1034,8.62069},{-43.1034,9.77011},{-42.5609,9.77011}}));
  connect(feedback_rearAxle.u1,gain_brakeSystem.y) annotation(Line(points = {{14.5379,-41.3793},{-14.3678,-41.3793},{-14.3678,9.77011},{-14.9609,9.77011}}));
  connect(gain_brakeSystem.y,gain_shareFrontAxle.u) annotation(Line(points = {{-14.9609,9.77011},{-7.47126,9.77011},{-7.47126,50},{1.69195,50},{1.69195,50.5747}}));
  connect(feedback_rearAxle.u2,gain_shareFrontAxle.y) annotation(Line(points = {{24.1379,-50.9793},{24.1379,-71.8391},{52.2989,-71.8391},{52.2989,50.5747},{29.292,50.5747},{29.292,50.5747}}));
  connect(gain_shareFrontAxle.y,output_brakeTorqueFront) annotation(Line(points = {{29.292,50.5747},{67.8161,50.5747},{67.8161,51.1494},{73.5632,51.1494}}));
  connect(feedback_rearAxle.y,output_brakeTorqueRear) annotation(Line(points = {{34.9379,-41.3793},{64.9425,-41.3793},{64.9425,-40.2299},{74.1379,-40.2299}}));
end BrakeSystem;

