package Electric
  model PowerSink
    Modelica.Electrical.Analog.Interfaces.PositivePin pin_p annotation(Placement(visible = true, transformation(origin = {-83.0556,42.7778}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-83.0556,42.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(Diagram(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-20.8333,-58.0556},{31.9444,-68.6111}}, textString = "Using ramp to avoid initial solver problem")}), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-57.2222,63.3333},{88.8889,-53.3333}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,255}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-28.6111,30.8333},{67.7778,-23.8889}}, textString = "EVS")}));
    Modelica.Electrical.Analog.Sources.SignalCurrent signalcurrent1 annotation(Placement(visible = true, transformation(origin = {-4.16669,35.5555}, extent = {{7.45106,-7.45106},{-7.45106,7.45106}}, rotation = -270)));
    parameter Real powerDemand(start = 0) annotation(Placement(visible = true, transformation(origin = {70.8333,72.2222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Division division_calculateCurrent annotation(Placement(visible = true, transformation(origin = {-28.3333,31.6667}, extent = {{-5.59809,-5.59809},{5.59809,5.59809}}, rotation = 0)));
    Modelica.Electrical.Analog.Sensors.VoltageSensor voltagesensor1 annotation(Placement(visible = true, transformation(origin = {-79.7222,19.1667}, extent = {{6.1579,-6.1579},{-6.1579,6.1579}}, rotation = -270)));
    Modelica.Electrical.Analog.Interfaces.NegativePin pin_n annotation(Placement(visible = true, transformation(origin = {-82.2222,-22.7778}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-82.2222,-22.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Real _current_log annotation(Placement(visible = true, transformation(origin = {71.3889,42.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Real _voltage_log annotation(Placement(visible = true, transformation(origin = {71.6667,12.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Real _powerDemand_log annotation(Placement(visible = true, transformation(origin = {71.1111,-20.2778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealInput u annotation(Placement(visible = true, transformation(origin = {-84.1667,73.6111}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-84.1667,73.6111}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(u,division_calculateCurrent.u1) annotation(Line(points = {{-84.1667,73.6111},{-37.7778,73.6111},{-37.7778,35},{-35.051,35},{-35.051,35.0256}}));
    connect(voltagesensor1.p,pin_n) annotation(Line(points = {{-79.7222,13.0088},{-80.2778,13.0088},{-80.2778,-20.2778},{-90,-20.2778}}));
    connect(voltagesensor1.v,division_calculateCurrent.u2) annotation(Line(points = {{-73.5643,19.1667},{-73.5643,18.6111},{-35.8333,18.6111},{-35.8333,28.3078},{-35.051,28.3078}}));
    connect(pin_p,voltagesensor1.n) annotation(Line(points = {{-83.0556,42.7778},{-79.7222,42.7778},{-79.7222,31.1579},{-79.7222,25.3246}}));
    connect(division_calculateCurrent.y,signalcurrent1.i) annotation(Line(points = {{-22.1754,31.6667},{-10,31.6667},{-10,35.5555},{-9.38243,35.5555}}));
    connect(signalcurrent1.p,pin_n) annotation(Line(points = {{-4.16669,28.1045},{-4.16671,28.1045},{-4.16671,-19.4444},{-90,-19.4444},{-90,-20.2778}}));
    connect(signalcurrent1.n,pin_p) annotation(Line(points = {{-4.16669,43.0066},{-79.1667,43.0066},{-79.1667,42.7778},{-83.0556,42.7778}}));
    _powerDemand_log = powerDemand;
    _voltage_log = pin_p.v;
    _current_log = pin_p.i;
  end PowerSink;
  model Storage
    Modelica.Electrical.Analog.Interfaces.PositivePin pin_p annotation(Placement(visible = true, transformation(origin = {58.6111,54.7222}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {58.6111,54.7222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(Diagram(), Icon(graphics = {Ellipse(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,0}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-72.7778,63.8889},{39.7222,-46.6667}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,0}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-56.1111,-3.61111},{22.7778,19.7222}}, textString = "Storage")}));
    Modelica.Electrical.Analog.Interfaces.NegativePin pin_n annotation(Placement(visible = true, transformation(origin = {63.0556,-32.2222}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {63.0556,-32.2222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Electrical.Analog.Sources.ConstantVoltage constantvoltage1 annotation(Placement(visible = true, transformation(origin = {-68.3333,18.8889}, extent = {{12,-12},{-12,12}}, rotation = 90)));
    Modelica.Electrical.Analog.Basic.VariableResistor variableresistor1 annotation(Placement(visible = true, transformation(origin = {-44.7222,54.7222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Electrical.Analog.Sensors.PowerSensor powersensor1 annotation(Placement(visible = true, transformation(origin = {59.7222,18.3333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.Constant const annotation(Placement(visible = true, transformation(origin = {-79.9378,80.5599}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(const.y,variableresistor1.R) annotation(Line(points = {{-66.7378,80.5599},{-45.1011,80.5599},{-45.1011,67.9222},{-44.7222,67.9222}}));
    connect(pin_p,powersensor1.nc) annotation(Line(points = {{58.6111,54.7222},{86.9444,54.7222},{86.9444,18.6111},{71.7222,18.6111},{71.7222,18.3333}}));
    connect(powersensor1.pc,variableresistor1.n) annotation(Line(points = {{47.7222,18.3333},{-8.88889,18.3333},{-8.88889,55.2778},{-32.7222,55.2778},{-32.7222,54.7222}}));
    connect(powersensor1.nv,pin_n) annotation(Line(points = {{59.7222,6.33333},{61.9444,6.33333},{61.9444,-32.2222},{63.0556,-32.2222}}));
    connect(pin_p,powersensor1.pv) annotation(Line(points = {{58.6111,54.7222},{59.7222,54.7222},{59.7222,30.3333},{59.7222,30.3333}}));
    connect(variableresistor1.p,constantvoltage1.p) annotation(Line(points = {{-56.7222,54.7222},{-67.7778,54.7222},{-67.7778,30.8889},{-68.3333,30.8889}}));
    connect(constantvoltage1.n,pin_n) annotation(Line(points = {{-68.3333,6.88889},{-68.0556,6.88889},{-68.0556,-32.7778},{63.0556,-32.7778},{63.0556,-32.2222}}));
  end Storage;
  model ElectricEfficiency
    parameter Real powerScale(start = 1.0);
    parameter Real etaMax(start = 1.0);
    parameter String filename(start = "maps/etaOne.txt");
    Modelica.Electrical.Analog.Interfaces.PositivePin pin_p annotation(Placement(visible = true, transformation(origin = {-76.3889,12.2222}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-76.3889,12.2222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(Diagram(), Icon(graphics = {Rectangle(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,0}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-63.8889,54.4444},{69.4444,-36.3889}}),Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,0}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-51.3889,-6.66667},{60.8333,10}}, textString = "Eta Electric"),Line(points = {{-50.2778,-21.3889},{57.2222,-21.3889},{46.3889,-18.3333},{46.3889,-25},{56.9444,-20.8333}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-50.8333,-21.1111},{-50.8333,51.6667},{-54.4444,41.1111},{-46.1111,41.1111},{-50.2778,51.9444}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-50.2778,-18.0556},{-45.2778,12.2222},{-35,25.8333},{-21.1111,36.9444},{4.16667,41.3889},{22.7778,44.1667},{53.6111,45.2778},{55.2778,45.8333}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}));
    Modelica.Electrical.Analog.Interfaces.NegativePin pin_n annotation(Placement(visible = true, transformation(origin = {81.3889,12.2222}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {81.3889,12.2222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Electrical.Analog.Sensors.PowerSensor powersensor1 annotation(Placement(visible = true, transformation(origin = {-43.3333,13.3333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.IntegerConstant integerconstant1(k = 1) annotation(Placement(visible = true, transformation(origin = {-18.8889,26.3889}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
    Modelica.Electrical.Analog.Basic.VariableResistor variableresistor1 annotation(Placement(visible = true, transformation(origin = {53.8707,13.3288}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Gain gain_EtaMax(k = etaMax) annotation(Placement(visible = true, transformation(origin = {39.3442,56.0109}, extent = {{-6.77369,-6.77369},{6.77369,6.77369}}, rotation = 0)));
    Modelica.Blocks.Routing.Extractor extractor1 annotation(Placement(visible = true, transformation(origin = {9.79505,56.3752}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
    Modelica.Blocks.Tables.CombiTable1Ds combitable1ds1(tableOnFile = true, tableName = "eta", fileName = filename) annotation(Placement(visible = true, transformation(origin = {-14.3533,56.3797}, extent = {{-7.45106,-7.45106},{7.45106,7.45106}}, rotation = 0)));
    Modelica.Blocks.Math.Gain gain_powerScale(k = powerScale) annotation(Placement(visible = true, transformation(origin = {-40.4371,56.0109}, extent = {{-6.1579,-6.1579},{6.1579,6.1579}}, rotation = 0)));
  equation
    connect(powersensor1.power,gain_powerScale.u) annotation(Line(points = {{-52.9333,0.1333},{-52.9333,-13.9344},{-61.2022,-13.9344},{-61.2022,56.0109},{-47.8266,56.0109},{-47.8266,56.0109}}));
    connect(gain_powerScale.y,combitable1ds1.u) annotation(Line(points = {{-33.6635,56.0109},{-23.4973,56.0109},{-23.4973,56.3797},{-23.2946,56.3797}}));
    connect(combitable1ds1.y,extractor1.u) annotation(Line(points = {{-6.15713,56.3797},{0.819672,56.3797},{0.819672,56.3752},{0.853777,56.3752}}));
    connect(integerconstant1.y,extractor1.index) annotation(Line(points = {{-10.6927,26.3889},{9.79055,26.3889},{9.79055,47.4339},{9.79505,47.4339}}));
    connect(extractor1.y,gain_EtaMax.u) annotation(Line(points = {{17.9912,56.3752},{30.3279,56.3752},{30.3279,56.0109},{31.2158,56.0109}}));
    connect(gain_EtaMax.y,variableresistor1.R) annotation(Line(points = {{46.7953,56.0109},{53.5519,56.0109},{53.5519,26.5288},{53.8707,26.5288}}));
    connect(powersensor1.nc,variableresistor1.p) annotation(Line(points = {{-31.3333,13.3333},{23.6111,13.3333},{23.6111,13.3288},{41.8707,13.3288}}));
    connect(variableresistor1.n,pin_n) annotation(Line(points = {{65.8707,13.3288},{80.5556,13.3288},{80.5556,12.2222},{81.3889,12.2222}}));
    connect(powersensor1.nv,pin_n) annotation(Line(points = {{-43.3333,1.33333},{81.9444,1.33333},{81.9444,12.2222},{81.3889,12.2222}}));
    connect(powersensor1.pv,pin_p) annotation(Line(points = {{-43.3333,25.3333},{-73.8889,25.3333},{-73.8889,12.2222},{-76.3889,12.2222}}));
    connect(pin_p,powersensor1.pc) annotation(Line(points = {{-76.3889,12.2222},{-54.7222,12.2222},{-54.7222,13.3333},{-55.3333,13.3333}}));
  end ElectricEfficiency;
  block powerInEfficiency
    parameter Real powerScale(start = 1.0);
    parameter Real maxEta(start = 1.0);
    parameter String filename(start = "maps/etaOne.txt");
    Real _powerIn_log annotation(Placement(visible = true, transformation(origin = {71.3889,42.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Real _powerOut_log annotation(Placement(visible = true, transformation(origin = {71.6667,12.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Real _eta_log annotation(Placement(visible = true, transformation(origin = {71.1111,-20.2778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealInput powerIn annotation(Placement(visible = true, transformation(origin = {-78.0556,11.9444}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-78.0556,11.9444}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(Diagram(), Icon(graphics = {Line(points = {{-56.3889,-65.2778},{73.3333,-65.2778},{61.6667,-62.7778},{61.6667,-68.6111},{73.0556,-65}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-55.5556,-65},{-55.5556,65.2778},{-57.5,56.6667},{-52.2222,56.6667},{-55.2778,65.5556}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-54.4444,-63.8889},{-44.4444,-19.1667},{-26.3889,20},{-6.66667,42.2222},{18.6111,52.7778},{51.6667,61.6667}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}));
    Modelica.Blocks.Interfaces.RealOutput powerOut annotation(Placement(visible = true, transformation(origin = {69.7222,19.1667}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {69.7222,19.1667}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Tables.CombiTable1Ds combitable1ds1(tableOnFile = true, tableName = "eta", fileName = filename) annotation(Placement(visible = true, transformation(origin = {-42.2222,11.6667}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Routing.Extractor extractor1 annotation(Placement(visible = true, transformation(origin = {-2.5,10.8333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.IntegerConstant integerconstant1(k = 1) annotation(Placement(visible = true, transformation(origin = {-35.2778,-30.2778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Division division1 annotation(Placement(visible = true, transformation(origin = {41.3889,53.8889}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(extractor1.y,division1.u2) annotation(Line(points = {{10.7,10.8333},{20,10.8333},{20,36.6667},{20.2778,36.6667},{20.2778,46.6667},{26.9889,46.6667},{26.9889,46.6889}}));
    connect(division1.y,powerOut) annotation(Line(points = {{54.5889,53.8889},{60.8333,53.8889},{60.8333,19.1667},{69.7222,19.1667}}));
    connect(division1.u1,powerOut) annotation(Line(points = {{26.9889,61.0889},{-71.3889,61.0889},{-71.3889,11.9444},{-78.0556,11.9444}}));
    connect(integerconstant1.y,extractor1.index) annotation(Line(points = {{-22.0778,-30.2778},{-3.33333,-30.2778},{-3.33333,-3.56667},{-2.5,-3.56667}}));
    connect(combitable1ds1.y,extractor1.u) annotation(Line(points = {{-29.0222,11.6667},{-16.9444,11.6667},{-16.9444,10.8333},{-16.9,10.8333}}));
    connect(powerIn,combitable1ds1.u) annotation(Line(points = {{-78.0556,11.9444},{-43.6111,11.9444},{-43.6111,11.6667},{-56.6222,11.6667}}));
    _powerIn_log = powerIn;
    _powerOut_log = powerOut;
    _eta_log = combitable1ds1.y[1];
  end powerInEfficiency;
  model powerSourceDemand
    parameter String filename(start = "maps/etaOne.txt");
    parameter Real powerScale(start = 1.0);
    parameter Real maxEta(start = 1.0);
    annotation(Diagram(), Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,0}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-31.3889,-15},{41.3889,11.6667}}, textString = "Power Source"),Line(points = {{-57.5,-7.22222},{-0.555556,57.5},{61.3889,-3.33333},{6.38889,-61.1111},{-57.2222,-7.22222}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}));
    Modelica.Blocks.Interfaces.RealOutput coalPower annotation(Placement(visible = true, transformation(origin = {73.3333,60.8333}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {73.3333,60.8333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealInput PowerDemand annotation(Placement(visible = true, transformation(origin = {-80.3143,1.03801}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-80.3143,1.03801}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Electrical.Analog.Interfaces.NegativePin pin_n annotation(Placement(visible = true, transformation(origin = {85.0877,-20.9649}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {85.0877,-20.9649}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Electrical.Analog.Sources.SignalCurrent signalcurrent1 annotation(Placement(visible = true, transformation(origin = {-37.5,0.833333}, extent = {{12,-12},{-12,12}}, rotation = 90)));
    Modelica.Electrical.Analog.Interfaces.PositivePin pin_p annotation(Placement(visible = true, transformation(origin = {87.9605,32.4707}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {87.9605,32.4707}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Electrical.Analog.Sensors.PowerSensor powersensor1 annotation(Placement(visible = true, transformation(origin = {26.9444,32.2222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    powerOutEfficiency poweroutefficiency1 annotation(Placement(visible = true, transformation(origin = {-21.6667,65}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(PowerDemand,signalcurrent1.i) annotation(Line(points = {{-80.3143,1.03801},{-46.0342,1.03801},{-46.0342,0.833333},{-45.9,0.833333}}));
    connect(powersensor1.power,poweroutefficiency1.powerOut) annotation(Line(points = {{17.3444,19.0222},{17.3444,13.0556},{-69.4444,13.0556},{-69.4444,66.9444},{-31.0333,66.9444},{-31.0333,66.4333}}));
    connect(poweroutefficiency1.powerIn,coalPower) annotation(Line(points = {{-13.3,67.3},{64.4444,67.3},{64.4444,60.8333},{73.3333,60.8333}}));
    connect(powersensor1.nv,pin_n) annotation(Line(points = {{26.9444,20.2222},{85.8333,20.2222},{85.8333,-20.9649},{85.0877,-20.9649}}));
    connect(powersensor1.pv,pin_p) annotation(Line(points = {{26.9444,44.2222},{87.2222,44.2222},{87.2222,32.4707},{87.9605,32.4707}}));
    connect(powersensor1.pc,signalcurrent1.n) annotation(Line(points = {{14.9444,32.2222},{-37.7778,32.2222},{-37.7778,12.8333},{-37.5,12.8333}}));
    connect(powersensor1.nc,pin_p) annotation(Line(points = {{38.9444,32.2222},{85,32.2222},{85,32.4707},{87.9605,32.4707}}));
    connect(pin_n,signalcurrent1.p) annotation(Line(points = {{85.0877,-20.9649},{-37.2222,-20.9649},{-37.2222,-11.1667},{-37.5,-11.1667}}));
  end powerSourceDemand;
  model powerSourceSupply
    parameter String filename(start = "maps/etaOne.txt");
    parameter Real powerScale(start = 1.0);
    parameter Real maxEta(start = 1.0);
    annotation(Diagram(), Icon(graphics = {Text(rotation = 0, lineColor = {0,0,255}, fillColor = {0,0,0}, pattern = LinePattern.Solid, fillPattern = FillPattern.None, lineThickness = 0.25, extent = {{-31.3889,-15},{41.3889,11.6667}}, textString = "Power Source"),Line(points = {{-57.5,-7.22222},{-0.555556,57.5},{61.3889,-3.33333},{6.38889,-61.1111},{-57.2222,-7.22222}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}));
    Modelica.Blocks.Interfaces.RealOutput coalPower annotation(Placement(visible = true, transformation(origin = {73.3333,60.8333}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {73.3333,60.8333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealInput PowerDemand annotation(Placement(visible = true, transformation(origin = {-80.3143,1.03801}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-80.3143,1.03801}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Electrical.Analog.Interfaces.NegativePin pin_n annotation(Placement(visible = true, transformation(origin = {85.0877,-20.9649}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {85.0877,-20.9649}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Electrical.Analog.Sources.SignalCurrent signalcurrent1 annotation(Placement(visible = true, transformation(origin = {-37.5,0.833333}, extent = {{12,-12},{-12,12}}, rotation = 90)));
    Modelica.Electrical.Analog.Interfaces.PositivePin pin_p annotation(Placement(visible = true, transformation(origin = {87.9605,32.4707}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {87.9605,32.4707}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Electrical.Analog.Sensors.PowerSensor powersensor1 annotation(Placement(visible = true, transformation(origin = {26.9444,32.2222}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    powerOutEfficiency poweroutefficiency1 annotation(Placement(visible = true, transformation(origin = {-21.6667,65}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(PowerDemand,signalcurrent1.i) annotation(Line(points = {{-80.3143,1.03801},{-46.0342,1.03801},{-46.0342,0.833333},{-45.9,0.833333}}));
    connect(powersensor1.power,poweroutefficiency1.powerOut) annotation(Line(points = {{17.3444,19.0222},{17.3444,13.0556},{-69.4444,13.0556},{-69.4444,66.9444},{-31.0333,66.9444},{-31.0333,66.4333}}));
    connect(poweroutefficiency1.powerIn,coalPower) annotation(Line(points = {{-13.3,67.3},{64.4444,67.3},{64.4444,60.8333},{73.3333,60.8333}}));
    connect(powersensor1.nv,pin_n) annotation(Line(points = {{26.9444,20.2222},{85.8333,20.2222},{85.8333,-20.9649},{85.0877,-20.9649}}));
    connect(powersensor1.pv,pin_p) annotation(Line(points = {{26.9444,44.2222},{87.2222,44.2222},{87.2222,32.4707},{87.9605,32.4707}}));
    connect(powersensor1.pc,signalcurrent1.n) annotation(Line(points = {{14.9444,32.2222},{-37.7778,32.2222},{-37.7778,12.8333},{-37.5,12.8333}}));
    connect(powersensor1.nc,pin_p) annotation(Line(points = {{38.9444,32.2222},{85,32.2222},{85,32.4707},{87.9605,32.4707}}));
    connect(pin_n,signalcurrent1.p) annotation(Line(points = {{85.0877,-20.9649},{-37.2222,-20.9649},{-37.2222,-11.1667},{-37.5,-11.1667}}));
  end powerSourceSupply;
  block powerOutEfficiency
    parameter Real powerScale(start = 1.0);
    parameter Real maxEta(start = 1.0);
    parameter String filename(start = "maps/etaOne.txt");
    Real _powerIn_log annotation(Placement(visible = true, transformation(origin = {71.3889,42.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Real _powerOut_log annotation(Placement(visible = true, transformation(origin = {71.6667,12.7778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Real _eta_log annotation(Placement(visible = true, transformation(origin = {71.1111,-20.2778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealInput powerOut annotation(Placement(visible = true, transformation(origin = {-78.0556,11.9444}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {-78.0556,11.9444}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(Diagram(), Icon(graphics = {Line(points = {{-56.3889,-65.2778},{73.3333,-65.2778},{61.6667,-62.7778},{61.6667,-68.6111},{73.0556,-65}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-55.5556,-65},{-55.5556,65.2778},{-57.5,56.6667},{-52.2222,56.6667},{-55.2778,65.5556}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25),Line(points = {{-54.4444,-63.8889},{-44.4444,-19.1667},{-26.3889,20},{-6.66667,42.2222},{18.6111,52.7778},{51.6667,61.6667}}, rotation = 0, color = {0,0,255}, pattern = LinePattern.Solid, thickness = 0.25)}));
    Modelica.Blocks.Interfaces.RealOutput powerIn annotation(Placement(visible = true, transformation(origin = {69.7222,19.1667}, extent = {{-12,-12},{12,12}}, rotation = 0), iconTransformation(origin = {69.7222,19.1667}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Tables.CombiTable1Ds combitable1ds1(tableOnFile = true, tableName = "eta", fileName = filename) annotation(Placement(visible = true, transformation(origin = {-42.2222,11.6667}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Routing.Extractor extractor1 annotation(Placement(visible = true, transformation(origin = {-2.5,10.8333}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Sources.IntegerConstant integerconstant1(k = 1) annotation(Placement(visible = true, transformation(origin = {-35.2778,-30.2778}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    Modelica.Blocks.Math.Product product1 annotation(Placement(visible = true, transformation(origin = {41.3889,53.8889}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(extractor1.y,product1.u2) annotation(Line(points = {{10.7,10.8333},{20,10.8333},{20,36.6667},{20.2778,36.6667},{20.2778,46.6667},{26.9889,46.6667},{26.9889,46.6889}}));
    connect(product1.y,powerIn) annotation(Line(points = {{54.5889,53.8889},{60.8333,53.8889},{60.8333,19.1667},{69.7222,19.1667}}));
    connect(product1.u1,powerOut) annotation(Line(points = {{26.9889,61.0889},{-71.3889,61.0889},{-71.3889,11.9444},{-78.0556,11.9444}}));
    connect(integerconstant1.y,extractor1.index) annotation(Line(points = {{-22.0778,-30.2778},{-3.33333,-30.2778},{-3.33333,-3.56667},{-2.5,-3.56667}}));
    connect(combitable1ds1.y,extractor1.u) annotation(Line(points = {{-29.0222,11.6667},{-16.9444,11.6667},{-16.9444,10.8333},{-16.9,10.8333}}));
    connect(powerOut,combitable1ds1.u) annotation(Line(points = {{-78.0556,11.9444},{-43.6111,11.9444},{-43.6111,11.6667},{-56.6222,11.6667}}));
    _powerIn_log = powerIn;
    _powerOut_log = powerOut;
    _eta_log = combitable1ds1.y[1];
  end powerOutEfficiency;
end Electric;

