package modLinearTwo
  annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  model RectA
    EFA_Blocks_simple.signalSource signalsource1(Rect_Amplitude = 2, Offset = -1, Rect_StartTime = 0.25) annotation(Placement(visible = true, transformation(origin = {-66.33,16.1616}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon1 annotation(Placement(visible = true, transformation(origin = {-39.3939,17.1717}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta1(eta = 0.4) annotation(Placement(visible = true, transformation(origin = {-13.468,16.835}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon2 annotation(Placement(visible = true, transformation(origin = {14.1414,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta2(eta = 0.8) annotation(Placement(visible = true, transformation(origin = {39.3939,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon3 annotation(Placement(visible = true, transformation(origin = {64.3098,16.1616}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  equation
    connect(eta2.y,powercon3.u) annotation(Line(points = {{49.2624,15.9457},{54.2088,15.9457},{54.2088,15.8853},{53.8887,15.8853}}));
    connect(powercon2.y,eta2.u) annotation(Line(points = {{24.2072,16.3009},{31.9865,16.3009},{31.9865,16.3009},{31.2229,16.3009}}));
    connect(eta1.y,powercon2.u) annotation(Line(points = {{-3.5996,16.2824},{4.0404,16.2824},{4.0404,16.222},{3.72036,16.222}}));
    connect(powercon1.y,eta1.u) annotation(Line(points = {{-29.3281,16.9743},{-21.2121,16.9743},{-21.2121,16.6376},{-21.6391,16.6376}}));
    connect(signalsource1.y,powercon1.u) annotation(Line(points = {{-55.9484,17.109},{-49.8316,17.109},{-49.8316,16.8954},{-49.815,16.8954}}));
  end RectA;
  model RectB
    EFA_Blocks_simple.signalSource signalsource1(Rect_Amplitude = 2, Offset = -1, Rect_StartTime = 0.25) annotation(Placement(visible = true, transformation(origin = {-66.33,16.1616}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon1 annotation(Placement(visible = true, transformation(origin = {-39.3939,17.1717}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta1(eta = 0.45) annotation(Placement(visible = true, transformation(origin = {-13.468,16.835}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon2 annotation(Placement(visible = true, transformation(origin = {14.4781,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta2(eta = 0.85) annotation(Placement(visible = true, transformation(origin = {38.7205,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon3 annotation(Placement(visible = true, transformation(origin = {64.3098,15.8249}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  equation
    connect(eta2.y,powercon3.u) annotation(Line(points = {{48.589,15.9457},{54.5455,15.9457},{54.5455,15.5486},{53.8887,15.5486}}));
    connect(powercon2.y,eta2.u) annotation(Line(points = {{24.5439,16.3009},{31.6498,16.3009},{31.6498,16.3009},{30.5495,16.3009}}));
    connect(eta1.y,powercon2.u) annotation(Line(points = {{-3.59958,16.2824},{4.0404,16.2824},{4.0404,16.222},{4.05705,16.222}}));
    connect(powercon1.y,eta1.u) annotation(Line(points = {{-29.3281,16.9743},{-21.2121,16.9743},{-21.2121,16.6376},{-21.6391,16.6376}}));
    connect(signalsource1.y,powercon1.u) annotation(Line(points = {{-55.9484,17.109},{-49.8316,17.109},{-49.8316,16.8954},{-49.815,16.8954}}));
  end RectB;
  model TrapA
    EFA_Blocks_simple.signalSource signalsource1(Offset = -1, Trap_Amplitude = 2, Trap_StartTime = 0.25) annotation(Placement(visible = true, transformation(origin = {-66.33,16.1616}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon1 annotation(Placement(visible = true, transformation(origin = {-39.3939,17.1717}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta1(eta = 0.4) annotation(Placement(visible = true, transformation(origin = {-13.468,16.835}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon2 annotation(Placement(visible = true, transformation(origin = {14.1414,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta2(eta = 0.8) annotation(Placement(visible = true, transformation(origin = {39.3939,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon3 annotation(Placement(visible = true, transformation(origin = {64.3098,16.1616}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  equation
    connect(eta2.y,powercon3.u) annotation(Line(points = {{49.2624,15.9457},{54.2088,15.9457},{54.2088,15.8853},{53.8887,15.8853}}));
    connect(powercon2.y,eta2.u) annotation(Line(points = {{24.2072,16.3009},{31.9865,16.3009},{31.9865,16.3009},{31.2229,16.3009}}));
    connect(eta1.y,powercon2.u) annotation(Line(points = {{-3.5996,16.2824},{4.0404,16.2824},{4.0404,16.222},{3.72036,16.222}}));
    connect(powercon1.y,eta1.u) annotation(Line(points = {{-29.3281,16.9743},{-21.2121,16.9743},{-21.2121,16.6376},{-21.6391,16.6376}}));
    connect(signalsource1.y,powercon1.u) annotation(Line(points = {{-55.9484,17.109},{-49.8316,17.109},{-49.8316,16.8954},{-49.815,16.8954}}));
  end TrapA;
  model TrapB
    EFA_Blocks_simple.signalSource signalsource1(Offset = -1, Trap_Amplitude = 2, Trap_StartTime = 0.25) annotation(Placement(visible = true, transformation(origin = {-66.33,16.1616}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon1 annotation(Placement(visible = true, transformation(origin = {-39.3939,17.1717}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta1(eta = 0.45) annotation(Placement(visible = true, transformation(origin = {-13.468,16.835}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon2 annotation(Placement(visible = true, transformation(origin = {14.4781,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta2(eta = 0.85) annotation(Placement(visible = true, transformation(origin = {38.7205,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon3 annotation(Placement(visible = true, transformation(origin = {64.3098,15.8249}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  equation
    connect(eta2.y,powercon3.u) annotation(Line(points = {{48.589,15.9457},{54.5455,15.9457},{54.5455,15.5486},{53.8887,15.5486}}));
    connect(powercon2.y,eta2.u) annotation(Line(points = {{24.5439,16.3009},{31.6498,16.3009},{31.6498,16.3009},{30.5495,16.3009}}));
    connect(eta1.y,powercon2.u) annotation(Line(points = {{-3.59958,16.2824},{4.0404,16.2824},{4.0404,16.222},{4.05705,16.222}}));
    connect(powercon1.y,eta1.u) annotation(Line(points = {{-29.3281,16.9743},{-21.2121,16.9743},{-21.2121,16.6376},{-21.6391,16.6376}}));
    connect(signalsource1.y,powercon1.u) annotation(Line(points = {{-55.9484,17.109},{-49.8316,17.109},{-49.8316,16.8954},{-49.815,16.8954}}));
  end TrapB;
  model SinA
    EFA_Blocks_simple.signalSource signalsource1(Offset = 0, Sine_Period = 1, Sine_Amplitude = 1, Sine_StartTime = 0) annotation(Placement(visible = true, transformation(origin = {-66.33,16.1616}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon1 annotation(Placement(visible = true, transformation(origin = {-39.3939,17.1717}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta1(eta = 0.4) annotation(Placement(visible = true, transformation(origin = {-13.468,16.835}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon2 annotation(Placement(visible = true, transformation(origin = {14.1414,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta2(eta = 0.8) annotation(Placement(visible = true, transformation(origin = {39.3939,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon3 annotation(Placement(visible = true, transformation(origin = {64.3098,16.1616}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  equation
    connect(eta2.y,powercon3.u) annotation(Line(points = {{49.2624,15.9457},{54.2088,15.9457},{54.2088,15.8853},{53.8887,15.8853}}));
    connect(powercon2.y,eta2.u) annotation(Line(points = {{24.2072,16.3009},{31.9865,16.3009},{31.9865,16.3009},{31.2229,16.3009}}));
    connect(eta1.y,powercon2.u) annotation(Line(points = {{-3.5996,16.2824},{4.0404,16.2824},{4.0404,16.222},{3.72036,16.222}}));
    connect(powercon1.y,eta1.u) annotation(Line(points = {{-29.3281,16.9743},{-21.2121,16.9743},{-21.2121,16.6376},{-21.6391,16.6376}}));
    connect(signalsource1.y,powercon1.u) annotation(Line(points = {{-55.9484,17.109},{-49.8316,17.109},{-49.8316,16.8954},{-49.815,16.8954}}));
  end SinA;
  model SinB
    EFA_Blocks_simple.signalSource signalsource1(Offset = 0, Sine_Period = 1, Sine_Amplitude = 1, Sine_StartTime = 0) annotation(Placement(visible = true, transformation(origin = {-66.33,16.1616}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon1 annotation(Placement(visible = true, transformation(origin = {-39.3939,17.1717}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta1(eta = 0.45) annotation(Placement(visible = true, transformation(origin = {-13.468,16.835}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon2 annotation(Placement(visible = true, transformation(origin = {14.4781,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta2(eta = 0.85) annotation(Placement(visible = true, transformation(origin = {38.7205,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon3 annotation(Placement(visible = true, transformation(origin = {64.3098,15.8249}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(experiment(StartTime = 0.0, StopTime = 1.0, Tolerance = 0.000001));
  equation
    connect(eta2.y,powercon3.u) annotation(Line(points = {{48.589,15.9457},{54.5455,15.9457},{54.5455,15.5486},{53.8887,15.5486}}));
    connect(powercon2.y,eta2.u) annotation(Line(points = {{24.5439,16.3009},{31.6498,16.3009},{31.6498,16.3009},{30.5495,16.3009}}));
    connect(eta1.y,powercon2.u) annotation(Line(points = {{-3.59958,16.2824},{4.0404,16.2824},{4.0404,16.222},{4.05705,16.222}}));
    connect(powercon1.y,eta1.u) annotation(Line(points = {{-29.3281,16.9743},{-21.2121,16.9743},{-21.2121,16.6376},{-21.6391,16.6376}}));
    connect(signalsource1.y,powercon1.u) annotation(Line(points = {{-55.9484,17.109},{-49.8316,17.109},{-49.8316,16.8954},{-49.815,16.8954}}));
  end SinB;
end modLinearTwo;

