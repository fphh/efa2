model BrakeSystem
  Modelica.Blocks.Interfaces.RealInput PedalPosition annotation(Placement(visible = true, transformation(origin = {-72.9885,8.62069}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-72.9885,8.62069}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-71.0078,84.9612},{61.7054,-67.2868}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-46.5116,24.186},{43.7209,-12.093}}, textString = "Brake System")}));
  Modelica.Blocks.Interfaces.RealOutput BrakeTorqueFront annotation(Placement(visible = true, transformation(origin = {73.5632,51.1494}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {73.5632,51.1494}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput BrakeTorqueRear annotation(Placement(visible = true, transformation(origin = {74.1379,-40.2299}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {74.1379,-40.2299}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Feedback feedback1 annotation(Placement(visible = true, transformation(origin = {24.1379,-41.3793}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain ShareFrontAxle(k = 0.6) annotation(Placement(visible = true, transformation(origin = {16.092,50.5747}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Gain Gain(k = 100) annotation(Placement(visible = true, transformation(origin = {-28.1609,9.77011}, extent = {{-12,-12},{12,12}}, rotation = 0)));
equation
  connect(PedalPosition,Gain.u) annotation(Line(points = {{-72.9885,8.62069},{-43.1034,8.62069},{-43.1034,9.77011},{-42.5609,9.77011}}));
  connect(feedback1.u1,Gain.y) annotation(Line(points = {{14.5379,-41.3793},{-14.3678,-41.3793},{-14.3678,9.77011},{-14.9609,9.77011}}));
  connect(Gain.y,ShareFrontAxle.u) annotation(Line(points = {{-14.9609,9.77011},{-7.47126,9.77011},{-7.47126,50},{1.69195,50},{1.69195,50.5747}}));
  connect(feedback1.u2,ShareFrontAxle.y) annotation(Line(points = {{24.1379,-50.9793},{24.1379,-71.8391},{52.2989,-71.8391},{52.2989,50.5747},{29.292,50.5747},{29.292,50.5747}}));
  connect(ShareFrontAxle.y,BrakeTorqueFront) annotation(Line(points = {{29.292,50.5747},{67.8161,50.5747},{67.8161,51.1494},{73.5632,51.1494}}));
  connect(feedback1.y,BrakeTorqueRear) annotation(Line(points = {{34.9379,-41.3793},{64.9425,-41.3793},{64.9425,-40.2299},{74.1379,-40.2299}}));
end BrakeSystem;

