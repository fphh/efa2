package EFA_Blocks_simple
  block w3_node
  end w3_node;
  block powerCon
    annotation(Diagram(), Icon(graphics = {Line(points = {{-74.6711,-1.97368},{70.3947,-1.97368},{76.9737,-1.64474},{76.6447,-1.64474}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-32.5658,-1.31579},{21.7105,33.8816}}, textString = "Connector", fontSize = 12, textStyle = {TextStyle.Bold})}));
    Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-86.8421,-2.30263}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-86.8421,-2.30263}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {83.8816,-1.64474}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {83.8816,-1.64474}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Gain gain2(k = -1) annotation(Placement(visible = true, transformation(origin = {-49.6711,-45.3947}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Nonlinear.Limiter negPower(uMin = 0) annotation(Placement(visible = true, transformation(origin = {-5.92105,-45.0658}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Continuous.Integrator posFlow annotation(Placement(visible = true, transformation(origin = {30.9211,32.2368}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Nonlinear.Limiter posPower(uMin = 0) annotation(Placement(visible = true, transformation(origin = {-12.1711,31.9079}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Continuous.Integrator negFlow annotation(Placement(visible = true, transformation(origin = {42.4342,-45.0658}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(negPower.y,negFlow.u) annotation(Line(points = {{7.27895,-45.0658},{27.3026,-45.0658},{27.3026,-45.0658},{28.0342,-45.0658}}));
    connect(posPower.y,posFlow.u) annotation(Line(points = {{1.02895,31.9079},{16.4474,31.9079},{16.4474,32.2368},{16.5211,32.2368}}));
    connect(posPower.u,u) annotation(Line(points = {{-26.5711,31.9079},{-80.5921,31.9079},{-80.5921,-2.30263},{-86.8421,-2.30263}}));
    connect(gain2.y,negPower.u) annotation(Line(points = {{-36.4711,-45.3947},{-19.7368,-45.3947},{-19.7368,-45.0658},{-20.3211,-45.0658}}));
    connect(gain2.u,u) annotation(Line(points = {{-64.0711,-45.3947},{-79.9342,-45.3947},{-79.9342,-2.30263},{-86.8421,-2.30263}}));
    connect(u,y) annotation(Line(points = {{-86.8421,-2.30263},{74.6711,-2.30263},{74.6711,-1.64474},{83.8816,-1.64474}}));
  end powerCon;
  annotation(Diagram(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-40.9396,-19.1275},{41.6107,29.1946}}, textString = "Simple EFA Blocks")}), experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  block ETA
    Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-68.0921,-1.64474}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-68.0921,-1.64474}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-61.5132,48.3553},{73.0263,-62.1711}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-15.7895,-7.23684},{-15.7895,-7.23684}}, textString = "eta", textStyle = {TextStyle.Bold}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-23.3553,-25.3289},{39.8026,22.0395}}, textString = "ETA", fontSize = 12, textStyle = {TextStyle.Bold})}));
    Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {82.2368,-4.60526}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {82.2368,-4.60526}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Gain gain1(k = eta) annotation(Placement(visible = true, transformation(origin = {-20.0658,42.1053}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Gain gain2(k = 1 / eta) annotation(Placement(visible = true, transformation(origin = {-21.3816,-57.2368}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Logical.Switch switch1 annotation(Placement(visible = true, transformation(origin = {39.4737,-2.96053}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Logical.GreaterEqual greaterequal1 annotation(Placement(visible = true, transformation(origin = {-11.5132,-3.61842}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Constant const(k = 0) annotation(Placement(visible = true, transformation(origin = {-67.1053,-26.9737}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real eta(start = 1) "Constant efficiency value" annotation(Placement(visible = false, transformation(origin = {48.3553,66.1184}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(gain2.y,switch1.u3) annotation(Line(points = {{-8.18158,-57.2368},{17.1053,-57.2368},{17.1053,-12.5605},{25.0737,-12.5605}}));
    connect(gain1.y,switch1.u1) annotation(Line(points = {{-6.86579,42.1053},{17.7632,42.1053},{17.7632,6.63947},{25.0737,6.63947}}));
    connect(u,gain1.u) annotation(Line(points = {{-68.0921,-1.64474},{-45.0658,-1.64474},{-45.0658,42.1053},{-34.4658,42.1053}}));
    connect(u,gain2.u) annotation(Line(points = {{-68.0921,-1.64474},{-45.0658,-1.64474},{-45.0658,-57.2368},{-35.7816,-57.2368}}));
    connect(greaterequal1.y,switch1.u2) annotation(Line(points = {{1.68684,-3.61842},{24.3421,-3.61842},{24.3421,-2.96053},{25.0737,-2.96053}}));
    connect(greaterequal1.u1,u) annotation(Line(points = {{-25.9132,-3.61842},{-57.5658,-3.61842},{-57.5658,-1.64474},{-68.0921,-1.64474}}));
    connect(switch1.y,y) annotation(Line(points = {{52.6737,-2.96053},{72.6974,-2.96053},{72.6974,-4.60526},{82.2368,-4.60526}}));
    connect(const.y,greaterequal1.u2) annotation(Line(points = {{-53.9053,-26.9737},{-26.9737,-26.9737},{-26.9737,-13.2184},{-25.9132,-13.2184}}));
  end ETA;
  block signalSource
    Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {86.5132,7.89474}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {86.5132,7.89474}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(Diagram(), Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-23.4628,12.1686},{32.913,45.3901}}, textString = "SignalSource"),Line(points = {{-45.7912,-12.7946},{-20.202,10.7744},{0.673399,-12.4579},{21.2121,9.42757},{37.7104,-13.1313},{58.2491,8.75419}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-67.0034,75.7576},{72.0539,-52.1886}})}));
    parameter Real Rect_Period(start = 1) "Constant Period Value" annotation(Placement(visible = true, transformation(origin = {-57.0138,75.1788}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real Rect_Amplitude(start = 0) "Constant Amplitude Value" annotation(Placement(visible = true, transformation(origin = {-12.383,75.8499}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real Rect_StartTime(start = 0) "Constant Amplitude Value" annotation(Placement(visible = true, transformation(origin = {-12.383,75.8499}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real Trap_Period(start = 1) "Constant Period Value" annotation(Placement(visible = true, transformation(origin = {-57.0138,75.1788}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real Trap_Amplitude(start = 0) "Constant Amplitude Value" annotation(Placement(visible = true, transformation(origin = {-12.383,75.8499}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real Trap_StartTime(start = 0) "Constant Amplitude Value" annotation(Placement(visible = true, transformation(origin = {-12.383,75.8499}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real Sine_Period(start = 1) "Constant Period Value" annotation(Placement(visible = true, transformation(origin = {-57.0138,75.1788}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real Sine_Amplitude(start = 0) "Constant Amplitude Value" annotation(Placement(visible = true, transformation(origin = {-12.383,75.8499}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real Sine_StartTime(start = 0) "Constant Amplitude Value" annotation(Placement(visible = true, transformation(origin = {-12.383,75.8499}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real Offset(start = 0) "Constant Offset Value" annotation(Placement(visible = true, transformation(origin = {30.5701,75.8499}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Trapezoid trapezoid1(period = Trap_Period, amplitude = Trap_Amplitude, width = Trap_Period / 2, rising = Trap_Period / 10, falling = Trap_Period / 10, startTime = Trap_StartTime) annotation(Placement(visible = true, transformation(origin = {-71.4436,42.4696}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Constant const(k = Offset) annotation(Placement(visible = true, transformation(origin = {-71.4436,2.00912}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Pulse pulse1(amplitude = Rect_Amplitude, width = Rect_Period * 50, startTime = Rect_StartTime, period = Rect_Period) annotation(Placement(visible = true, transformation(origin = {-71.3805,-39.3939}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Add add1 annotation(Placement(visible = true, transformation(origin = {12.8744,24.0331}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Sine sine1(amplitude = Sine_Amplitude, freqHz = 1 / Sine_Period, phase = 0, startTime = Sine_StartTime) annotation(Placement(visible = true, transformation(origin = {-72.0539,-78.1145}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Add add2 annotation(Placement(visible = true, transformation(origin = {19.5286,-55.5556}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Add add3 annotation(Placement(visible = true, transformation(origin = {51.5152,-11.4478}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(add2.y,add3.u2) annotation(Line(points = {{32.7286,-55.5556},{37.037,-55.5556},{37.037,-18.6478},{37.1152,-18.6478}}));
    connect(add1.y,add3.u1) annotation(Line(points = {{26.0744,24.0331},{37.037,24.0331},{37.037,-4.24781},{37.1152,-4.24781}}));
    connect(add3.y,y) annotation(Line(points = {{64.7152,-11.4478},{75.7576,-11.4478},{75.7576,7.89474},{86.5132,7.89474}}));
    connect(sine1.y,add2.u2) annotation(Line(points = {{-58.8539,-78.1145},{4.3771,-78.1145},{4.3771,-62.7556},{5.1286,-62.7556}}));
    connect(pulse1.y,add2.u1) annotation(Line(points = {{-58.1805,-39.3939},{4.3771,-39.3939},{4.3771,-48.3556},{5.1286,-48.3556}}));
    connect(const.y,add1.u2) annotation(Line(points = {{-58.2436,2.00912},{-10.2594,2.00912},{-10.2594,16.8331},{-1.52563,16.8331}}));
    connect(trapezoid1.y,add1.u1) annotation(Line(points = {{-58.2436,42.4696},{-12.2331,42.4696},{-12.2331,31.2331},{-1.52563,31.2331}}));
  end signalSource;
end EFA_Blocks_simple;

