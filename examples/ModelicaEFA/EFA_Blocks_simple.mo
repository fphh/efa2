package EFA_Blocks_simple
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
  block w3_node
    Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-72.7273,65.3199}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-72.7273,65.3199}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(Diagram(), Icon(graphics = {Line(points = {{-67.6768,66.6667},{-21.8855,2.3569},{52.5253,2.0202},{55.2189,1.6835}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-21.8855,2.3569},{-74.0741,-61.2795},{-73.7374,-61.2795}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-23.2323,44.7811},{41.7508,67.0034}}, textString = "3-Way-2ins", fontSize = 12)}));
    Modelica.Blocks.Interfaces.RealInput realinput2 annotation(Placement(visible = true, transformation(origin = {62.963,3.0303}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {62.963,3.0303}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealInput realinput1 annotation(Placement(visible = true, transformation(origin = {-77.1044,-61.2795}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-77.1044,-61.2795}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Add add1 annotation(Placement(visible = true, transformation(origin = {-8.08081,6.73401}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(realinput1,add1.u2) annotation(Line(points = {{-77.1044,-61.2795},{-23.569,-61.2795},{-23.569,-0.465993},{-22.4808,-0.465993}}));
    connect(u,add1.u1) annotation(Line(points = {{-72.7273,65.3199},{-22.8956,65.3199},{-22.8956,13.934},{-22.4808,13.934}}));
    connect(add1.y,realinput2) annotation(Line(points = {{5.11919,6.73401},{68.0135,6.73401},{68.0135,3.0303},{62.963,3.0303}}));
  end w3_node;
  block ETA_lin
    parameter Real eta0(start = 1) "Start efficiency value" annotation(Placement(visible = false, transformation(origin = {80.0051,82.28}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    parameter Real etaSlope(start = 0) "Efficiency slope" annotation(Placement(visible = false, transformation(origin = {80.0051,82.28}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-61.5132,48.3553},{73.0263,-62.1711}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-15.7895,-7.23684},{-15.7895,-7.23684}}, textString = "ETA_lin", textStyle = {TextStyle.Bold}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-46.2509,-4.11678},{16.907,43.2516}}, textString = "ETA", fontSize = 12, textStyle = {TextStyle.Bold}),Line(points = {{-54.5455,-58.2492},{59.596,-6.73401},{59.2593,-7.07071}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-54.8822,-58.2492},{64.9832,-58.2492},{65.9933,-58.2492}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-54.2088,-57.5758},{-54.2088,40.404}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}), Diagram());
    Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {82.2368,-4.60526}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {82.2368,-4.60526}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Logical.GreaterEqual greaterequal1 annotation(Placement(visible = true, transformation(origin = {-11.5132,-3.61842}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Constant const(k = 0) annotation(Placement(visible = true, transformation(origin = {-67.1053,-26.9737}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-68.0921,-1.30804}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-68.0921,-1.30804}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Gain Slope annotation(Placement(visible = true, transformation(origin = {-38.0471,70.3704}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Add add1 annotation(Placement(visible = true, transformation(origin = {-0.3367,61.9529}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Logical.Switch switch1 annotation(Placement(visible = true, transformation(origin = {50.2481,-2.62383}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Constant constant1(k = eta0) annotation(Placement(visible = true, transformation(origin = {-48.4848,34.6801}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Constant constant2(k = 1) annotation(Placement(visible = true, transformation(origin = {-19.1919,-60.6061}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Division division1 annotation(Placement(visible = true, transformation(origin = {34.6801,-68.6869}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(division1.y,switch1.u3) annotation(Line(points = {{47.8801,-68.6869},{61.6162,-68.6869},{61.6162,-30.6397},{24.5791,-30.6397},{24.5791,-11.7845},{35.8481,-11.7845},{35.8481,-12.2238}}));
    connect(division1.u2,add1.y) annotation(Line(points = {{20.2801,-75.8869},{12.7946,-75.8869},{12.7946,61.9529},{12.8633,61.9529}}));
    connect(constant2.y,division1.u1) annotation(Line(points = {{-5.99192,-60.6061},{10.101,-60.6061},{10.101,-61.4869},{20.2801,-61.4869}}));
    connect(Slope.y,add1.u1) annotation(Line(points = {{-24.8471,70.3704},{-15.4882,70.3704},{-15.4882,69.1529},{-14.7367,69.1529}}));
    connect(constant1.y,add1.u2) annotation(Line(points = {{-35.2848,34.6801},{-16.1616,34.6801},{-16.1616,54.7529},{-14.7367,54.7529}}));
    connect(add1.y,switch1.u1) annotation(Line(points = {{12.8633,61.9529},{34.3434,61.9529},{34.3434,6.97617},{35.8481,6.97617}}));
    connect(switch1.y,y) annotation(Line(points = {{63.4481,-2.62383},{72.6974,-2.62383},{72.6974,-4.60526},{82.2368,-4.60526}}));
    connect(greaterequal1.y,switch1.u2) annotation(Line(points = {{1.6868,-3.61842},{24.3421,-3.61842},{24.3421,-2.62383},{35.8481,-2.62383}}));
    connect(Slope.u,u) annotation(Line(points = {{-52.4471,70.3704},{-65.6566,70.3704},{-65.6566,-1.30804},{-68.0921,-1.30804}}));
    connect(greaterequal1.u1,u) annotation(Line(points = {{-25.9132,-3.61842},{-57.5658,-3.61842},{-57.5658,-1.30804},{-68.0921,-1.30804}}));
    connect(const.y,greaterequal1.u2) annotation(Line(points = {{-53.9053,-26.9737},{-26.9737,-26.9737},{-26.9737,-13.2184},{-25.9132,-13.2184}}));
  end ETA_lin;
  block ETA_var
    parameter Real etaOffset(start = 1) "Start efficiency value" annotation(Placement(visible = false, transformation(origin = {80.0051,82.28}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-61.5132,48.3553},{73.0263,-62.1711}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-15.7895,-7.23684},{-15.7895,-7.23684}}, textString = "ETA_lin", textStyle = {TextStyle.Bold}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-46.2509,-4.11678},{16.907,43.2516}}, textString = "ETA", fontSize = 12, textStyle = {TextStyle.Bold}),Line(points = {{-54.5455,-58.2492},{59.596,-6.73401},{59.2593,-7.07071}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-54.8822,-58.2492},{64.9832,-58.2492},{65.9933,-58.2492}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-54.2088,-57.5758},{-54.2088,40.404}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}), Diagram());
    Modelica.Blocks.Interfaces.RealOutput y annotation(Placement(visible = true, transformation(origin = {82.2368,-4.60526}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {82.2368,-4.60526}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Logical.GreaterEqual greaterequal1 annotation(Placement(visible = true, transformation(origin = {-11.5132,-3.61842}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Constant const(k = 0) annotation(Placement(visible = true, transformation(origin = {-67.1053,-26.9737}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-68.0921,-1.30804}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-68.0921,-1.30804}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Add add1 annotation(Placement(visible = true, transformation(origin = {-0.3367,61.9529}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Logical.Switch switch1 annotation(Placement(visible = true, transformation(origin = {50.2481,-2.62383}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Constant constant1(k = etaOffset) annotation(Placement(visible = true, transformation(origin = {-48.4848,34.6801}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Constant constant2(k = 1) annotation(Placement(visible = true, transformation(origin = {-19.1919,-60.6061}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Division division1 annotation(Placement(visible = true, transformation(origin = {34.6801,-68.6869}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Tables.CombiTable1Ds combitable1ds1(table = [0,1;3,4], columns = 1) annotation(Placement(visible = true, transformation(origin = {-49.0566,69.8113}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(u,combitable1ds1.u) annotation(Line(points = {{-68.0921,-1.30804},{-72.327,-1.30804},{-72.327,69.1824},{-63.4566,69.1824},{-63.4566,69.8113}}));
    connect(division1.y,switch1.u3) annotation(Line(points = {{47.8801,-68.6869},{61.6162,-68.6869},{61.6162,-30.6397},{24.5791,-30.6397},{24.5791,-11.7845},{35.8481,-11.7845},{35.8481,-12.2238}}));
    connect(division1.u2,add1.y) annotation(Line(points = {{20.2801,-75.8869},{12.7946,-75.8869},{12.7946,61.9529},{12.8633,61.9529}}));
    connect(constant2.y,division1.u1) annotation(Line(points = {{-5.99192,-60.6061},{10.101,-60.6061},{10.101,-61.4869},{20.2801,-61.4869}}));
    connect(constant1.y,add1.u2) annotation(Line(points = {{-35.2848,34.6801},{-16.1616,34.6801},{-16.1616,54.7529},{-14.7367,54.7529}}));
    connect(add1.y,switch1.u1) annotation(Line(points = {{12.8633,61.9529},{34.3434,61.9529},{34.3434,6.97617},{35.8481,6.97617}}));
    connect(switch1.y,y) annotation(Line(points = {{63.4481,-2.62383},{72.6974,-2.62383},{72.6974,-4.60526},{82.2368,-4.60526}}));
    connect(greaterequal1.y,switch1.u2) annotation(Line(points = {{1.6868,-3.61842},{24.3421,-3.61842},{24.3421,-2.62383},{35.8481,-2.62383}}));
    connect(greaterequal1.u1,u) annotation(Line(points = {{-25.9132,-3.61842},{-57.5658,-3.61842},{-57.5658,-1.30804},{-68.0921,-1.30804}}));
    connect(const.y,greaterequal1.u2) annotation(Line(points = {{-53.9053,-26.9737},{-26.9737,-26.9737},{-26.9737,-13.2184},{-25.9132,-13.2184}}));
  end ETA_var;
end EFA_Blocks_simple;

