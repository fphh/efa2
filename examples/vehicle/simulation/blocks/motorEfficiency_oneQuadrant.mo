model MotorEfficiency
  annotation(Icon(graphics = {Line(points = {{-48.7395,-60.9244},{-48.7395,71.0084},{-52.9412,56.3025},{-42.0168,56.3025},{-48.3193,71.0084}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-47.8992,-60.5042},{69.7479,-60.5042},{51.2605,-55.4622},{51.2605,-67.2269},{69.3277,-60.5042}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-44.958,21.0084},{-2.52101,21.0084},{5.46218,6.72269},{19.3277,-5.46218},{35.2941,-10.084},{59.6639,-12.605}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-14.7059,-1.2605},{-37.8151,-5.88235},{-37.395,-22.6891},{-21.0084,-39.916},{22.2689,-41.5966},{43.6975,-34.4538},{23.5294,-23.9496},{-7.98319,-7.56303},{-13.4454,-0.420168}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-18.0672,-14.7059},{-24.7899,-23.1092},{-14.7059,-31.5126},{14.2857,-34.0336},{6.30252,-25.2101},{-15.5462,-14.7059}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-46.6387,95.7983},{56.3025,71.4286}}, textString = "MotorEfficiency")}), Diagram());
  parameter Real speedScale(start = 1) annotation(Placement(visible = true, transformation(origin = {73.5294,76.4706}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter Real maxEta(start = 1) annotation(Placement(visible = true, transformation(origin = {73.5294,76.4706}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  parameter String fileName(start = "") annotation(Placement(visible = true, transformation(origin = {73.5294,76.4706}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput input_minTorque annotation(Placement(visible = true, transformation(origin = {-73.9496,-67.6471}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-73.9496,-67.6471}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_zero2(k = 0) annotation(Placement(visible = true, transformation(origin = {-81.9328,-56.7227}, extent = {{-5.59809,-5.59809},{5.59809,5.59809}}, rotation = 0)));
  RelativeTorque relativetorque1 annotation(Placement(visible = true, transformation(origin = {-47.6975,-25.6303}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Abs abs_torque annotation(Placement(visible = true, transformation(origin = {-24.5882,-21.4286}, extent = {{-8.19616,-8.19616},{8.19616,8.19616}}, rotation = 0)));
  Modelica.Blocks.Logical.GreaterEqual greaterequal1 annotation(Placement(visible = true, transformation(origin = {22.2689,-17.6471}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_zero(k = 0) annotation(Placement(visible = true, transformation(origin = {-2.94118,-35.7143}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput input_speed annotation(Placement(visible = true, transformation(origin = {-76.0504,47.479}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-76.0504,47.479}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput input_torque annotation(Placement(visible = true, transformation(origin = {-75.6302,7.14282}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-75.6302,7.14282}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealInput input_maxTorque annotation(Placement(visible = true, transformation(origin = {-74.7899,-31.5126}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-74.7899,-31.5126}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Math.Product product_calculatePower annotation(Placement(visible = true, transformation(origin = {-37.8758,-1.16013}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_speedScale(k = speedScale) annotation(Placement(visible = true, transformation(origin = {-48.0042,47.3693}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Modelica.Blocks.Math.Gain gain_maxEta(k = maxEta) annotation(Placement(visible = true, transformation(origin = {17.1919,12.0425}, extent = {{-5.59809,-5.59809},{5.59809,5.59809}}, rotation = 0)));
  Modelica.Blocks.Tables.CombiTable2D combitable2d1(tableName = "table2D_efficiencyMap_firstQuadrant", tableOnFile = true, fileName = fileName) annotation(Placement(visible = true, transformation(origin = {-1.7297,11.4869}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Math.Abs abs_speed annotation(Placement(visible = true, transformation(origin = {-29.0957,16.8138}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
  Modelica.Blocks.Interfaces.RealOutput output_electricPower annotation(Placement(visible = true, transformation(origin = {78.436,3.48272}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {78.436,3.48272}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Logical.Switch switch_powerFlowDirection annotation(Placement(visible = true, transformation(origin = {44.7082,5.3198}, extent = {{-5.08917,-5.08917},{5.08917,5.08917}}, rotation = 0)));
  Modelica.Blocks.Math.Division division_invertEfficiency annotation(Placement(visible = true, transformation(origin = {28.1513,32.7731}, extent = {{-5.59809,-5.59809},{5.59809,5.59809}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const_one(k = 1) annotation(Placement(visible = true, transformation(origin = {0.936044,37.3576}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  Modelica.Blocks.Math.Product product_electricPower annotation(Placement(visible = true, transformation(origin = {73.5504,22.9062}, extent = {{-5.59809,-5.59809},{5.59809,5.59809}}, rotation = 0)));
equation
  connect(product_calculatePower.u2,input_torque) annotation(Line(points = {{-46.0042,-5.22434},{-59.797,-5.22434},{-59.797,7.14282},{-75.6302,7.14282}}));
  connect(product_electricPower.y,output_electricPower) annotation(Line(points = {{79.7083,22.9062},{83.6904,22.9062},{83.6904,10.5953},{83.3684,10.5953},{83.3684,3.12562},{78.436,3.12562},{78.436,3.48272}}));
  connect(product_calculatePower.y,product_electricPower.u2) annotation(Line(points = {{-30.4248,-1.16013},{62.4043,-1.16013},{62.4043,19.5473},{66.8327,19.5473}}));
  connect(switch_powerFlowDirection.y,product_electricPower.u1) annotation(Line(points = {{50.3063,5.3198},{52.4977,5.3198},{52.4977,26.265},{66.8327,26.265}}));
  connect(const_one.y,division_invertEfficiency.u1) annotation(Line(points = {{7.70973,37.3576},{19.7479,37.3576},{19.7479,36.132},{21.4336,36.132}}));
  connect(division_invertEfficiency.y,switch_powerFlowDirection.u1) annotation(Line(points = {{34.3092,32.7731},{38.2353,32.7731},{38.2353,9.39114},{38.6012,9.39114}}));
  connect(gain_maxEta.y,division_invertEfficiency.u2) annotation(Line(points = {{23.3498,12.0425},{26.4706,12.0425},{26.4706,22.2689},{19.7479,22.2689},{19.7479,29.4142},{21.4336,29.4142}}));
  connect(greaterequal1.y,switch_powerFlowDirection.u2) annotation(Line(points = {{30.4651,-17.6471},{33.1933,-17.6471},{33.1933,4.62185},{38.6012,4.62185},{38.6012,5.3198}}));
  connect(gain_maxEta.y,switch_powerFlowDirection.u3) annotation(Line(points = {{23.3498,12.0425},{25.7237,12.0425},{25.7237,-1.2605},{38.6012,-1.2605},{38.6012,1.24846}}));
  connect(gain_speedScale.y,abs_speed.u) annotation(Line(points = {{-41.2305,47.3693},{-40.2077,47.3693},{-40.2077,16.8138},{-38.037,16.8138}}));
  connect(abs_speed.y,combitable2d1.u1) annotation(Line(points = {{-20.8996,16.8138},{-19.3277,16.8138},{-19.3277,15.9576},{-10.671,15.9576}}));
  connect(abs_torque.y,combitable2d1.u2) annotation(Line(points = {{-15.5724,-21.4286},{-15.5462,-21.4286},{-15.5462,7.14286},{-10.671,7.14286},{-10.671,7.01629}}));
  connect(combitable2d1.y,gain_maxEta.u) annotation(Line(points = {{6.46647,11.4869},{10.9244,11.4869},{10.9244,12.0425},{10.4742,12.0425}}));
  connect(input_torque,relativetorque1.input_torque) annotation(Line(points = {{-75.6302,7.14282},{-59.6148,7.14282},{-59.6148,-20.2353},{-56.9244,-20.2353}}));
  connect(input_speed,gain_speedScale.u) annotation(Line(points = {{-76.0504,47.479},{-55.8824,47.479},{-55.8824,47.3693},{-55.3937,47.3693}}));
  connect(greaterequal1.u1,product_calculatePower.y) annotation(Line(points = {{13.3276,-17.6471},{9.2437,-17.6471},{9.2437,-1.02474},{-30.4248,-1.02474},{-30.4248,-1.16013}}));
  connect(product_calculatePower.u1,input_speed) annotation(Line(points = {{-46.0042,2.90409},{-84.0336,2.90409},{-84.0336,47.479},{-76.0504,47.479}}));
  connect(const_zero.y,greaterequal1.u2) annotation(Line(points = {{5.25498,-35.7143},{7.56303,-35.7143},{7.56303,-23.6079},{13.3276,-23.6079}}));
  connect(relativetorque1.output_relativeTorque,abs_torque.u) annotation(Line(points = {{-38.269,-21.7479},{-34.2521,-21.7479},{-34.2521,-21.4286},{-34.4236,-21.4286}}));
  connect(const_zero2.y,relativetorque1.input_minTorque) annotation(Line(points = {{-75.7749,-56.7227},{-58.8235,-56.7227},{-58.8235,-29.4622},{-56.5715,-29.4622}}));
  connect(input_maxTorque,relativetorque1.input_maxTorque) annotation(Line(points = {{-74.3697,-41.1765},{-68.4874,-41.1765},{-68.4874,-25.6303},{-56.1681,-25.6303},{-56.1681,-24.7227}}));
end MotorEfficiency;
